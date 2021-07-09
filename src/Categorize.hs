{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE Rank2Types, CPP, FlexibleContexts #-}
module Categorize where

import Data

import qualified Text.Regex.PCRE.Light.Text as RE
import qualified Data.MyText as T
import Data.MyText (Text)
import Control.Applicative (empty, (<*), (<$), (<$>))
import Control.Monad
import Control.Monad.Trans.Reader hiding (local)
import Control.Monad.Reader.Class (local)
import Control.Monad.Trans.Class
import Data.Functor.Identity

import Control.DeepSeq
import Data.Char
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Time.Calendar (toGregorian, fromGregorian)
import Data.Time.Calendar.WeekDate (toWeekDate)
import Data.Time.Clock
import Data.Time.Format (formatTime)
import Data.Time.LocalTime
import System.Exit
import System.IO
import Text.Show.Functions
import Text.Parsec
import Text.Parsec.ExprFail
import Text.Parsec.Token
#if MIN_VERSION_time(1,5,0)
import Data.Time.Format(defaultTimeLocale, iso8601DateFormat)
#else
import System.Locale (defaultTimeLocale, iso8601DateFormat)
#endif
import Debug.Trace
import Text.Printf
import GHC.Generics (Generic)

type Categorizer = TimeLog CaptureData -> TimeLog (Ctx, ActivityData)
type ApplyCond = TimeLogEntry (Ctx, ActivityData) -> Bool
type Rule = Ctx -> ActivityData
type Environment = Map String Cond

type Parser = ParsecT String () (ReaderT (TimeZone, Environment) Identity)

data Ctx = Ctx
        { cNow :: TimeLogEntry CaptureData
        , cCurrentWindow :: Maybe WindowData
        , cWindowInScope :: Maybe WindowData
        , cSubsts :: [Text]
        , cCurrentTime :: ZonedTime
        , conditionBindings :: Map String Cond
        } deriving (Show, Generic, NFData)

type Cond = CtxFun [Text]

type CtxFun a = Ctx -> Maybe a

data CondPrim
        = CondString (CtxFun Text)
        | CondRegex (CtxFun RE.Regex)
        | CondInteger (CtxFun Integer)
        | CondTime (CtxFun NominalDiffTime)
        | CondDate (CtxFun UTCTime)
        | CondCond (CtxFun [Text])
        | CondStringList (CtxFun [Text])
        | CondRegexList (CtxFun [RE.Regex])

newtype Cmp = Cmp (forall a. Ord a => a -> a -> Bool)

data DateVar = DvDate | DvNow

data TimeVar = TvTime | TvSampleAge

data NumVar = NvIdle

data BoolVar = BvScreenSaver

runParserStack :: Stream s (ReaderT r Identity) t
               => r
               -> ParsecT s () (ReaderT r Identity) a
               -> SourceName
               -> s
               -> Either ParseError a
runParserStack env p filename =
  runIdentity . flip runReaderT env . runParserT p () filename

readCategorizer :: FilePath -> IO Categorizer
readCategorizer filename = withFile filename ReadMode $ \h -> do
        hSetEncoding h utf8
        content <- hGetContents h
        time <- getZonedTime
        case runParserStack (zonedTimeZone time, Map.empty) (between (return ()) eof parseRules) filename content of
          Left err -> do
                putStrLn "Parser error:"
                print err
                exitFailure
          Right cat -> return
                (map (fmap (mkSecond (postpare . cat))) . prepare time)

mkApplyCond :: String -> IO ApplyCond
mkApplyCond s = do
        tz <- getCurrentTimeZone
        case runParserStack (tz, Map.empty) (parseCond <* eof) "command line parameter" s of
          Left err -> do
                putStrLn "Parser error:"
                print err
                exitFailure
          Right c -> return (isJust . c . fst . tlData)

prepare :: ZonedTime -> TimeLog CaptureData -> TimeLog Ctx
prepare time = map go
  where go now  = now {tlData = Ctx now (find wActive (cWindows (tlData now))) Nothing [] time Map.empty }

-- | Here, we filter out tags appearing twice, and make sure that only one of
--   each category survives
postpare :: ActivityData -> ActivityData
postpare = nubBy go
  where go (Activity (Just c1) _) (Activity (Just c2) _) = c1 == c2
        go a1                     a2                     = a1 == a2

lang :: GenTokenParser String () (ReaderT (TimeZone, Environment) Identity)
lang = makeTokenParser LanguageDef
                { commentStart   = "{-"
                , commentEnd     = "-}"
                , commentLine    = "--"
                , nestedComments = True
                , identStart     = letter
                , identLetter    = alphaNum <|> oneOf "_'"
                , opStart        = oneOf ":!#$%&*+./<=>?@\\^|-~"
                , opLetter       = oneOf ":!#$%&*+./<=>?@\\^|-~"
                , reservedOpNames= []
                , reservedNames  = [ "title"
                                   , "program"
                                   , "wdesktop"
                                   , "active"
                                   , "hidden"
                                   , "idle"
                                   , "time"
                                   , "sampleage"
                                   , "date"
                                   , "now"
                                   , "desktop"
                                   , "screensaver"
                                   ]
                , caseSensitive  = True
                }

parseRules :: Parser Rule
parseRules = do
        whiteSpace lang
        a <- option id (reserved lang "aliases" >> parens lang parseAliasSpecs)
        rb <- parseRulesBody
        return (a . rb)

parseAliasSpecs :: Parser (ActivityData -> ActivityData)
parseAliasSpecs = do as <- sepEndBy1 parseAliasSpec (comma lang)
                     return $ \ad -> foldr doAlias ad as

doAlias :: (Text, Text) -> ActivityData -> ActivityData
doAlias (s1,s2) = map go
  where go (Activity cat tag) = Activity (if cat == Just s1 then Just s2 else cat)
                                         (if tag == s1 then s2 else tag)

parseAliasSpec :: Parser (Text, Text)
parseAliasSpec = do s1 <- T.pack <$> stringLiteral lang
                    reservedOp lang "->"
                    s2 <- T.pack <$> stringLiteral lang
                    return (s1,s2)

parseRulesBody :: Parser Rule
parseRulesBody = do
        x <- parseRule
        choice [ do _ <- comma lang
                    xs <- parseRule `sepEndBy1` comma lang
                    return (matchAny (x:xs))
               , do _ <- semi lang
                    xs <- parseRule `sepEndBy1` semi lang
                    return (matchFirst (x:xs))
               ,    return x
               ]

withBinding :: String -> Cond -> Parser a -> Parser a
withBinding k v = local (\(tz,env) -> (tz, Map.insert k v env))

parseConditionBinding :: Parser Rule
parseConditionBinding = do
  _ <- reserved lang "condition"
  varname <- identifier lang
  _ <- reservedOp lang "="
  cond <- parseCond
  _ <- reserved lang "in"
  withBinding varname cond parseRule

parseRule :: Parser Rule
parseRule = choice
        [    braces lang parseRules
        , do cond <- parseCond
             reservedOp lang "==>"
             rule <- parseRule
             return (ifThenElse cond rule matchNone)
        , do reserved lang "if"
             cond <- parseCond
             reserved lang "then"
             rule1 <- parseRule
             reserved lang "else"
             rule2 <- parseRule
             return (ifThenElse cond rule1 rule2)
        , do reserved lang "tag"
             parseSetTag
        , parseConditionBinding
        ]

parseCond :: Parser Cond
parseCond = do cp <- parseCondExpr
               case cp of
                CondCond c -> return c
                _          -> fail $ printf "Expression of type %s" (cpType cp)

parseCondExpr :: Parser CondPrim
parseCondExpr = buildExpressionParser [
                [ Prefix (reservedOp lang "!" >> return checkNot) ],
                [ Prefix (reserved lang "day of week" >> return evalDayOfWeek)
                , Prefix (reserved lang "day of month" >> return evalDayOfMonth)
                , Prefix (reserved lang "month" >> return evalMonth)
                , Prefix (reserved lang "year" >> return evalYear)
                , Prefix (reserved lang "format" >> return formatDate) ],
                [ Infix (reservedOp lang "=~" >> return checkRegex) AssocNone
                , Infix (checkCmp <$> parseCmp) AssocNone
                ],
                [ Prefix (reserved lang "current window" >> return checkCurrentwindow)
                , Prefix (reserved lang "any window" >> return checkAnyWindow)
                ],
                [ Infix (reservedOp lang "&&" >> return checkAnd) AssocRight ],
                [ Infix (reservedOp lang "||" >> return checkOr) AssocRight ]
            ] parseCondPrim

cpType :: CondPrim -> String
cpType (CondString _) = "String"
cpType (CondRegex _) = "Regex"
cpType (CondInteger _) = "Integer"
cpType (CondTime _) = "Time"
cpType (CondDate _) = "Date"
cpType (CondCond _) = "Condition"
cpType (CondStringList _) = "List of Strings"
cpType (CondRegexList _) = "List of regular expressions"

checkRegex :: CondPrim -> CondPrim -> Erring CondPrim
checkRegex (CondString getStr) (CondRegex getRegex) = Right $ CondCond $ \ctx -> do
        str <- getStr ctx
        regex <- getRegex ctx
        tail <$> RE.match regex str [RE.exec_no_utf8_check]
checkRegex (CondString getStr) (CondRegexList getRegexList) = Right $ CondCond $ \ctx -> do
        str <- getStr ctx
        regexes <- getRegexList ctx
        tail <$> msum (map (\regex -> RE.match regex str [RE.exec_no_utf8_check]) regexes)
checkRegex cp1 cp2 = Left $
        printf "Cannot apply =~ to an expression of type %s and type %s"
               (cpType cp1) (cpType cp2)

checkAnd :: CondPrim-> CondPrim -> Erring CondPrim
checkAnd (CondCond c1) (CondCond c2) = Right $ CondCond $ do
        res1 <- c1
        res2 <- c2
        return $ res1 >> res2
checkAnd cp1 cp2 = Left $
        printf "Cannot apply && to an expression of type %s and type %s"
               (cpType cp1) (cpType cp2)

checkOr :: CondPrim-> CondPrim -> Erring CondPrim
checkOr (CondCond c1) (CondCond c2) = Right $ CondCond $ do
        res1 <- c1
        res2 <- c2
        return $ res1 `mplus` res2
checkOr cp1 cp2 = Left $
        printf "Cannot apply && to an expression of type %s and type %s"
               (cpType cp1) (cpType cp2)

checkNot :: CondPrim -> Erring CondPrim
checkNot (CondCond getCnd) = Right . CondCond $ fmap (maybe (Just []) (const Nothing)) getCnd
checkNot cp = Left $
        printf "Cannot apply ! to an expression of type %s"
               (cpType cp)

checkCmp :: Cmp -> CondPrim -> CondPrim -> Erring CondPrim
checkCmp (Cmp (?)) (CondInteger getN1) (CondInteger getN2) = Right $ CondCond $ \ctx -> do
        n1 <- getN1 ctx
        n2 <- getN2 ctx
        guard (n1 ? n2)
        return []
checkCmp (Cmp (?)) (CondTime getT1) (CondTime getT2) = Right $ CondCond $ \ctx -> do
        t1 <- getT1 ctx
        t2 <- getT2 ctx
        guard (t1 ? t2)
        return []
checkCmp (Cmp (?)) (CondDate getT1) (CondDate getT2) = Right $ CondCond $ \ctx -> do
        t1 <- getT1 ctx
        t2 <- getT2 ctx
        guard (t1 ? t2)
        return []
checkCmp (Cmp (?)) (CondString getS1) (CondString getS2) = Right $ CondCond $ \ctx -> do
        s1 <- getS1 ctx
        s2 <- getS2 ctx
        guard (s1 ? s2)
        return []
checkCmp (Cmp (?)) (CondString getS1) (CondStringList getS2) = Right $ CondCond $ \ctx -> do
        s1 <- getS1 ctx
        sl <- getS2 ctx
        guard (any (s1 ?) sl)
        return []
checkCmp _ cp1 cp2 = Left $
        printf "Cannot compare expressions of type %s and type %s"
               (cpType cp1) (cpType cp2)

checkCurrentwindow :: CondPrim -> Erring CondPrim
checkCurrentwindow (CondCond cond) = Right $ CondCond $ \ctx ->
        cond (ctx { cWindowInScope = cCurrentWindow ctx })
checkCurrentwindow cp = Left $
        printf "Cannot apply current window to an expression of type %s"
               (cpType cp)

checkAnyWindow :: CondPrim -> Erring CondPrim
checkAnyWindow (CondCond cond) = Right $ CondCond $ \ctx ->
        msum $ map (\w -> cond (ctx { cWindowInScope = Just w }))
                                     (cWindows (tlData (cNow ctx)))
checkAnyWindow cp = Left $
        printf "Cannot apply current window to an expression of type %s"
               (cpType cp)

fst3 :: (a,b,c) -> a
fst3 (a,_,_) = a

snd3 :: (a,b,c) -> b
snd3 (_,b,_) = b

trd3 :: (a,b,c) -> c
trd3 (_,_,c) = c

-- Day of week is an integer in [1..7].
evalDayOfWeek :: CondPrim -> Erring CondPrim
evalDayOfWeek (CondDate df) = Right $ CondInteger $ \ctx ->
  let tz = zonedTimeZone (cCurrentTime ctx) in
  (toInteger . trd3 . toWeekDate . localDay . utcToLocalTime tz) `fmap` df ctx
evalDayOfWeek cp = Left $ printf
  "Cannot apply day of week to an expression of type %s, only to $date."
  (cpType cp)

-- Day of month is an integer in [1..31].
evalDayOfMonth :: CondPrim -> Erring CondPrim
evalDayOfMonth (CondDate df) = Right $ CondInteger $ \ctx ->
  let tz = zonedTimeZone (cCurrentTime ctx) in
  (toInteger . trd3 . toGregorian . localDay . utcToLocalTime tz) `fmap` df ctx
evalDayOfMonth cp = Left $ printf
  "Cannot apply day of month to an expression of type %s, only to $date."
  (cpType cp)

-- Month is an integer in [1..12].
evalMonth :: CondPrim -> Erring CondPrim
evalMonth (CondDate df) = Right $ CondInteger $ \ctx ->
  let tz = zonedTimeZone (cCurrentTime ctx) in
  (toInteger . snd3 . toGregorian . localDay . utcToLocalTime tz) `fmap` df ctx
evalMonth cp = Left $ printf
  "Cannot apply month to an expression of type %s, only to $date."
  (cpType cp)

evalYear :: CondPrim -> Erring CondPrim
evalYear (CondDate df) = Right $ CondInteger $ \ctx ->
  let tz = zonedTimeZone (cCurrentTime ctx) in
  (fst3 . toGregorian . localDay . utcToLocalTime tz) `fmap` df ctx
evalYear cp = Left $ printf
  "Cannot apply year to an expression of type %s, only to $date."
  (cpType cp)

-- format date according to ISO 8601 (YYYY-MM-DD)
formatDate :: CondPrim -> Erring CondPrim
formatDate (CondDate df) = Right $ CondString $ \ctx ->
  let tz = zonedTimeZone (cCurrentTime ctx)
      local = utcToLocalTime tz `fmap` df ctx
   in T.pack . formatTime defaultTimeLocale (iso8601DateFormat Nothing) <$> local
formatDate cp = Left $ printf
  "Cannot format an expression of type %s, only $date." (cpType cp)

parseCmp :: Parser Cmp
parseCmp = choice $ map (\(s,o) -> reservedOp lang s >> return o)
                        [(">=",Cmp (>=)),
                         (">", Cmp (>)),
                         ("==",Cmp (==)),
                         ("=", Cmp (==)),
                         ("!=",Cmp (/=)),
                         ("<", Cmp (<)),
                         ("<=",Cmp (<=))]

parseCondPrim :: Parser CondPrim
parseCondPrim = choice
        [ parens lang parseCondExpr
        , brackets lang (choice [
            (do list <- commaSep1 lang (stringLiteral lang)
                return $ CondStringList (const (Just (map T.pack list)))
            ) <?> "list of strings",
            (do list <- commaSep1 lang parseRegex
                return $ CondRegexList (const (Just list))
            ) <?> "list of regular expressions"
            ])
        , char '$' >> choice
             [ do backref <- read <$> many1 digit
                  return $ CondString (getBackref backref)
             , choice [ reserved lang "title" >> return (CondString (getVar "title"))
                      , reserved lang "program" >> return (CondString (getVar "program"))
                      , reserved lang "wdesktop" >> return (CondString (getVar "wdesktop"))
                      , reserved lang "active" >> return (CondCond checkActive)
                      , reserved lang "hidden" >> return (CondCond checkHidden)
                      , reserved lang "idle" >> return (CondInteger (getNumVar NvIdle))
                      , reserved lang "time" >> return (CondTime (getTimeVar TvTime))
                      , reserved lang "sampleage" >> return (CondTime (getTimeVar TvSampleAge))
                      , reserved lang "date" >> return (CondDate (getDateVar DvDate))
                      , reserved lang "now" >> return (CondDate (getDateVar DvNow))
                      , reserved lang "desktop" >> return (CondString (getVar "desktop"))
                      , reserved lang "screensaver" >> return (CondCond (checkBoolVar BvScreenSaver))
                      , do varname <- identifier lang
                           inEnvironment <- (lift (asks (Map.lookup varname . snd)))
                           case inEnvironment of
                             Nothing -> fail ("Reference to unbound variable: '" ++ varname ++ "'")
                             Just cond -> return (CondCond cond)
                      ]
              ] <?> "variable"
        , do regex <- parseRegex <?> "regular expression"
             return $ CondRegex (const (Just regex))
        , do str <- T.pack <$> stringLiteral lang <?> "string"
             return $ CondString (const (Just str))
        , try $ do time <- parseTime <?> "time" -- backtrack here, it might have been a number
                   return $ CondTime (const (Just time))
        , try $ do date <- parseDate <?> "date" -- backtrack here, it might have been a number
                   return $ CondDate (const (Just date))
        , do num <- natural lang <?> "number"
             return $ CondInteger (const (Just num))
        ]

parseRegex :: Parser RE.Regex
parseRegex = fmap (flip RE.compile [] . T.pack) $ lexeme lang $ choice
        [ between (char '/') (char '/') (many1 (noneOf "/"))
        , do _ <- char 'm'
             c <- anyChar
             str <- many1 (noneOf [c])
             _ <- char c
             return str
        ]

-- | Parses a day-of-time specification (hh:mm)
parseTime :: Parser NominalDiffTime
parseTime = fmap fromIntegral $ lexeme lang $ do
               hour <- read <$> many1 digit
               _ <- char ':'
               minute <- read <$> count 2 digit
               return $ (hour * 60 + minute) * 60

parseDate :: Parser UTCTime
parseDate = lexeme lang $ do
    tz <- lift (asks fst)
    year <- read <$> count 4 digit
    _ <- char '-'
    month <- read <$> count 2 digit
    _ <- char '-'
    day <- read <$> count 2 digit
    time <- option 0 parseTime
    let date = LocalTime (fromGregorian year month day) (TimeOfDay 0 0 0)
    return $ addUTCTime time $ localTimeToUTC tz date


parseSetTag :: Parser Rule
parseSetTag = lexeme lang $ do
                 firstPart <- parseTagPart
                 choice [ do char ':'
                             secondPart <- parseTagPart
                             return $ do cat <- firstPart
                                         tag <- secondPart
                                         return $ maybeToList $ do
                                            cat <- cat
                                            tag <- tag
                                            return $ Activity (Just cat) tag
                        ,    return $ do tag <- firstPart
                                         return $ maybeToList $ do
                                            tag <- tag
                                            return $ Activity Nothing tag
                        ]

replaceForbidden :: Maybe Text -> Maybe Text
replaceForbidden = fmap $ T.map go
  where
    go c | isAlphaNum c  = c
         | c `elem` "-_" = c
         | otherwise     = '_'

parseTagPart :: Parser (Ctx -> Maybe Text)
parseTagPart = do parts <- many1 (choice
                        [ do char '$'
                             (replaceForbidden . ) <$> choice
                               [ do num <- read <$> many1 digit
                                    return $ getBackref num
                               , do varname <- many1 (letter <|> oneOf ".")
                                    return $ getVar varname
                               ] <?> "variable"
                        , do s <- many1 (alphaNum <|> oneOf "-_")
                             return $ const (Just (T.pack s))
                        ])
                  return $ (fmap T.concat . sequence) <$> sequence parts

ifThenElse :: Cond -> Rule -> Rule -> Rule
ifThenElse cond r1 r2 = do res <- cond
                           case res of
                            Just substs -> r1 . setSubsts substs
                            Nothing -> r2
  where setSubsts :: [Text] -> Ctx -> Ctx
        setSubsts substs ctx = ctx { cSubsts = substs }


matchAny :: [Rule] -> Rule
matchAny rules = concat <$> sequence rules
matchFirst :: [Rule] -> Rule
matchFirst rules = takeFirst <$> sequence rules
  where takeFirst [] = []
        takeFirst ([]:xs) = takeFirst xs
        takeFirst (x:_) = x


getBackref :: Integer -> CtxFun Text
getBackref n ctx = listToMaybe (drop (fromIntegral n-1) (cSubsts ctx))

getVar :: String -> CtxFun Text
getVar v ctx | "current" `isPrefixOf` v = do
                let var = drop (length "current.") v
                win <- cCurrentWindow ctx
                getVar var (ctx { cWindowInScope = Just win })
getVar "title"   ctx = wTitle <$> cWindowInScope ctx
getVar "program" ctx = wProgram <$> cWindowInScope ctx
getVar "wdesktop" ctx = wDesktop <$> cWindowInScope ctx
getVar "desktop" ctx = return $ cDesktop (tlData (cNow ctx))
getVar v _ = error $ "Unknown variable " ++ v

getNumVar :: NumVar -> CtxFun Integer
getNumVar NvIdle ctx = Just $ cLastActivity (tlData (cNow ctx)) `div` 1000

getTimeVar :: TimeVar -> CtxFun NominalDiffTime
getTimeVar TvTime ctx = Just $
   let utc = tlTime . cNow $ ctx
       tz = zonedTimeZone (cCurrentTime ctx)
       local = utcToLocalTime tz utc
       midnightUTC = localTimeToUTC tz $ local { localTimeOfDay = midnight }
    in utc `diffUTCTime` midnightUTC
getTimeVar TvSampleAge ctx = Just $ zonedTimeToUTC (cCurrentTime ctx) `diffUTCTime` tlTime (cNow ctx)

getDateVar :: DateVar -> CtxFun UTCTime
getDateVar DvDate = Just . tlTime . cNow
getDateVar DvNow = Just . zonedTimeToUTC . cCurrentTime

checkBoolVar :: BoolVar -> Cond
checkBoolVar BvScreenSaver ctx = [] <$ guard (cScreenSaver (tlData (cNow ctx)))

checkActive, checkHidden :: Cond
checkActive ctx = [] <$ (guard =<< wActive <$> cWindowInScope ctx)
checkHidden ctx = [] <$ (guard =<< wHidden <$> cWindowInScope ctx)

matchNone :: Rule
matchNone = const []

justIf :: a -> Bool -> Maybe a
justIf x True = Just x
justIf _ False = Nothing

mkSecond :: (a -> b) -> a -> (a, b)
mkSecond f a = (a, f a)

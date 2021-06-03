"use strict";

function get(selector) { return document.querySelectorAll(selector); };
var menuLinks = get('nav > ul > li > a');

for (var i = 0; i < menuLinks.length; i++) {
	menuLinks[i].onclick = function(){
		for (var j = 0; j < menuLinks.length; j++) {
			menuLinks[j ].classList.remove('active');
		}
		this.classList.add('active');
	};
}

var x = window.location;
if ( x.href.indexOf("#") < 0 ) {
	var anchor = document.getElementsByTagName("section")[0].id
	window.location = window.location + "#" + anchor;
	menuLinks[0].classList.add('active');
}
else {
	get('a[href="' + window.location.hash + '"]')[0].classList.add('active');
}

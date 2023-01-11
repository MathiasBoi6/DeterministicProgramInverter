
var navbar = document.getElementsByClassName("front")[0];
var nbLeft = navbar.getElementsByTagName("a");
var nbRight = navbar.getElementsByTagName("c");

var spLeft = []
spLeft[0] = document.getElementById("grammarText");
spLeft[1] = document.getElementById("syntaxGrammar");

var spRight = []
spRight[0] = document.getElementById("invertedGrammar");
spRight[1] = document.getElementById("uncompressedGrammar");
spRight[2] = document.getElementById("interpret1");
spRight[3] = document.getElementById("interpret2");
spRight[4] = document.getElementById("interpret3");
spRight[5] = document.getElementById("interpret4");
spRight[6] = document.getElementById("notes");

console.log(nbLeft);


function StripActive(nb) {
	for (let item of nb) {
    	item.classList.remove("active");
	}
}

function DisplayLeft(obj){
	if (obj.innerHTML == "Show Syntax") {
		for (let item of spLeft) {
    		item.style.display = "none";
		}
		spLeft[1].style.display = "block";
		compile();
	}
	if (obj.innerHTML == "Editor") {
		for (let item of spLeft) {
    		item.style.display = "none";
		}
		spLeft[0].style.display = "block";
	}
}

function DisplayRight(obj){
	if (obj.innerHTML == "Uncompressed Grammar") {
		for (let item of spRight) {
    		item.style.display = "none";
		}
		spRight[1].style.display = "block";
	}
	if (obj.innerHTML == "Inverse Grammar") {
		for (let item of spRight) {
    		item.style.display = "none";
		}
		spRight[0].style.display = "block";
	}
	if (obj.innerHTML == "Interpret") {
		for (let item of spRight) {
    		item.style.display = "none";
		}
		spRight[2].style.display = "block";
		spRight[3].style.display = "block";
		spRight[4].style.display = "block";
		spRight[5].style.display = "block";
	}
	if (obj.innerHTML == "Notes") {
		for (let item of spRight) {
    		item.style.display = "none";
		}
		spRight[6].style.display = "block";
	}
}

function ActivateNB(obj){
	var elem = obj.firstChild;
	if (!elem.classList.contains("active")) {
		
		if (elem.tagName == "A") {
			StripActive(nbLeft);
			DisplayLeft(elem);
		}
		else {
			StripActive(nbRight);
			DisplayRight(elem);
		}
		elem.classList.add("active")
	}
}
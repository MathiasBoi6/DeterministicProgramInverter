const words = ['CaseNum', 'Case']
const types = ['MakeNum', 'Make', 'Nonterminal']
const variables = ['Error']

function ColorSyntax(grammar) {
  var newHTML = grammar.innerHTML
  for (var j=0; j < words.length; j++) {
    var regex = new RegExp("\\b" + words[j] + "\\b", "g");
      newHTML = newHTML.replaceAll(regex, `<span style="color: blue;">${words[j]}</span>`)
  }
  for (var j=0; j < types.length; j++) {
    var regex = new RegExp("\\b" + types[j] + "\\b", "g");
      newHTML = newHTML.replaceAll(regex, `<span style="color: green;">${types[j]}</span>`)
  }
  for (var j=0; j < variables.length; j++) {
    var regex = new RegExp("\\b" + variables[j] + "\\b", "g");
      newHTML = newHTML.replaceAll(regex, `<span style="color: red;">${variables[j]}</span>`)
  }
  var regex = new RegExp('\\\(\\\*.*?\\\*\\\)',"g")
  newHTML = newHTML.replaceAll(regex, `<span style="background-color: #b3b3b3; color: white;">${"$&"}</span>`)

  grammar.innerHTML = newHTML
}


function compile() {
  var grammarText = document.getElementById("grammarText");
  //var css = document.getElementById("css");
  //var js = document.getElementById("js");
  var result = document.getElementById("syntaxGrammar");

  result.innerHTML = grammarText.value;
  ColorSyntax(result);
}

compile();
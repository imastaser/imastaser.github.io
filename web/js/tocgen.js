
$("#toc ul").children("li").children("a").preprend('<i class="fa fa-chevron-right></i>');

window.onload = function() {
    var anchors = document.getElementsByClassName('sourceLine');
    for(var i = 0; i < anchors.length; i++) {
        var anchor = anchors[i];
        anchor.onclick = codeLineClick;

    }
}


function codeLineClick() {
  console.log(this.id);
  var copyText = document.getElementById(this.id).text;
  console.log(copyText);

  copy(copyText);
     this.title = copyText;
 }


function copy(text) {
    var input = document.createElement('input');
    input.setAttribute('value', text);
    document.body.appendChild(input);
    input.select();
    var result = document.execCommand('copy');
    document.body.removeChild(input)
    return result;
 }

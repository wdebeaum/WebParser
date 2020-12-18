var xhtmlNS = "http://www.w3.org/1999/xhtml";
var svgNS = "http://www.w3.org/2000/svg";
var extscontents;
var extsformat;
var tagsformat;
var treecontents;
var treeformat;
var lfformat;
var debugCheckbox;
var debugDiv;
var displayCSSRules;
var hypInput;
var hypStyleSheet;
var correctHyp;

function initVars() {
  extscontents = document.getElementById("extscontents");
  extsformat = document.getElementById("extsformat");
  tagsformat = document.getElementById("tagsformat");
  treecontents = document.getElementById("treecontents");
  treeformat = document.getElementById("treeformat");
  lfformat = document.getElementById("lfformat");
  debugCheckbox = document.getElementById("debug-checkbox");
  debugDiv = document.getElementById("debug-div");
  displayCSSRules = document.styleSheets[0].cssRules;
  hypInput = document.getElementById("hyp");
  hypStyleSheet = document.styleSheets[1];
}

function initHypDisplay() {
  // detect if this is a saved parse, and extract correctHyp from the filename
  var path = new URL(window.location.href).pathname;
  var m = path.match(/\/\d{8}T\d{6}-\w+-\w+-(\d+|none)-.*?\.xml$/);
  if (m) {
    correctHyp = (m[1] == 'none' ? m[1] : parseInt(m[1]));
  }
  // initially show the correct hyp, if any; otherwise the first one
  var initialHyp = (('number' == typeof correctHyp) ? correctHyp : 0);
  var i;
  for (i = 0; document.getElementsByClassName("hyp-" + i).length > 0; i++) {
    hypStyleSheet.insertRule(
      ".hyp-" + i + ((i == initialHyp) ? " { }" : " { display: none; }"),
      hypStyleSheet.cssRules.length
    );
    if (undefined !== correctHyp) {
      var lfHeadings = document.querySelectorAll('div.hyp-' + i + ' h2.lf');
      for (var j = 0; j < lfHeadings.length; j++) {
	lfHeadings[j].classList.add(
	  (i === correctHyp) ? 'correct' : 'incorrect'
	);
      }
    }
  }
  if (i == 0 && // no .hyp-0 means there's only one hyp, and it's not in a div
      undefined !== correctHyp) {
    var lfHeadings = document.querySelectorAll('h2.lf');
    for (var j = 0; j < lfHeadings.length; j++) {
      lfHeadings[j].classList.add(
	(i === correctHyp) ? 'correct' : 'incorrect'
      );
    }
  }
  if (hypInput) {
    hypInput.value = initialHyp;
    if (undefined !== correctHyp) {
      hypInput.parentNode.className = ((initialHyp === correctHyp) ? 'correct' : 'incorrect');
    }
  }
}

function displayHyp(hyp) {
  for (var i = 0; i < hypStyleSheet.cssRules.length; i++) {
    hypStyleSheet.cssRules[i].style.display = ((i == hyp) ? '' : 'none');
  }
  if (hypInput && undefined !== correctHyp) {
    hypInput.parentNode.className = ((hyp === correctHyp) ? 'correct' : 'incorrect');
  }
}

function setDisplay(node, displayOn) {
  node.style.display = displayOn? "" : "none"
}

function setAllDisplay() {
  var extsSel = '.exts-' + (extscontents===null ? '' : (extscontents.value + '-')) + (extsformat===null ? 'none' : extsformat.value);
  var tagsSel = '.tags-' + tagsformat.value;
  var treeSel = '.tree-' + (treecontents===null ? 'none' : (treecontents.value + '-' + treeformat.value));
  var lfSel = '.lf-' + (lfformat===null ? 'none' : lfformat.value);
  for (var i = 0; i < displayCSSRules.length; i++) {
    if (extsSel == displayCSSRules[i].selectorText ||
        tagsSel == displayCSSRules[i].selectorText ||
	treeSel == displayCSSRules[i].selectorText ||
	lfSel == displayCSSRules[i].selectorText) {
      displayCSSRules[i].style.display = '';
    } else if (/^#/.test(displayCSSRules[i].selectorText)) {
      // do nothing (display of these nodes is handled elsewhere)
    } else {
      displayCSSRules[i].style.display = 'none';
    }
  }
  if (debugDiv) {
    debugDiv.style.display = (debugCheckbox.checked ? '' : 'none');
  }
}

function toggleDisplay(id) {
  for (var i = 0; i < displayCSSRules.length; i++) {
    if ('#' + id == displayCSSRules[i].selectorText) {
      if (displayCSSRules[i].style.display == 'none') {
	displayCSSRules[i].style.display = '';
      } else {
	displayCSSRules[i].style.display = 'none';
      }
    }
  }
}

function getTagType() {
  var checkboxes = document.getElementById('tag-type-checkboxes').getElementsByTagName('input');
  var ret = '(or';
  var prev = '';
  for (var i = 0; i < checkboxes.length; i++) {
    if (checkboxes[i].checked) {
      var ul = checkboxes[i].parentNode.parentNode.parentNode
      if (ul.id == 'tag-type-checkboxes') {
	ret += checkboxes[i].nextSibling.data;
      } else {
	ret += ' (and' + ul.parentNode.firstChild.lastChild.data + checkboxes[i].nextSibling.data + ')';
      }
    }
    prev = checkboxes[i].nextSibling.data;
  }
  ret += ')';
  if (ret === '(or)') {
    ret = '';
  }
  return ret;
}

function getSOFPP() {
  var checkboxes = document.getElementById('senses-only-for-penn-poss-checkboxes').getElementsByTagName('input');
  var ret = '';
  for (var i = 0; i < checkboxes.length; i++) {
    if (checkboxes[i].checked) {
      var label = checkboxes[i].nextSibling.data;
      if (/^ all /.test(label)) {
	var ul = checkboxes[i].parentNode.parentNode.getElementsByTagName('ul')[0];
	var subcheckboxes = ul.getElementsByTagName('input');
	for (var j = 0; j < subcheckboxes.length; j++) {
	  if (ret != '') {
	    ret += ',';
	  }
	  ret += subcheckboxes[j].nextSibling.data;
	}
      } else {
	if (ret != '') {
	  ret += ',';
	}
	ret += label;
      }
    }
  }
  return ret;
}

function handleCheckboxChange(evt) {
  if (evt.target.tagName === 'input' && evt.target.type === 'checkbox') {
    var ul_ancestor = evt.target.parentNode.parentNode.parentNode;
    if (!ul_ancestor.id) {
      ul_ancestor = ul_ancestor.parentNode.parentNode;
    }
    var input_id = ul_ancestor.id.replace(/-checkboxes$/,'');
    var input = document.getElementById(input_id);
    if (input_id == 'tag-type') {
      input.value = getTagType();
    } else if (input_id == 'senses-only-for-penn-poss') {
      input.value = getSOFPP();
    }
  }
}

function downcaseHack() {
  var links = document.getElementsByTagName("a");
  for (var i = 0; i < links.length; i++) {
    links[i].href = links[i].href.replace(/::.*?\./, function(str) { return str.toLowerCase() });
  }
}

function linkifyError() {
  var errors = document.getElementsByClassName('error');
  for (var i = 0; i < errors.length; i++) {
    errors[i].innerHTML =
      errors[i].innerHTML.
      replace(/\[[^\]]+\]/g, function(m) {
	var l = m.substring(1, m.length - 1);
	return "<a href=\"" + l + "\">" + l + "</a>";
      });
  }
}

function dotToSVG(dotPRE, simple) {
  var dotText = dotPRE.textContent;
  var params = "dot=" + encodeURIComponent(dotText);
  if (simple) params += "&simple=on";
  var xhr = new XMLHttpRequest();
  xhr.open("POST", "../cgi/dot-to-svg.pl", true);
  xhr.setRequestHeader("Content-type", "application/x-www-form-urlencoded");
  xhr.setRequestHeader("Content-length", params.length);
  xhr.setRequestHeader("Connection", "close");
  xhr.onreadystatechange = function() {
    if (xhr.readyState == 4 && xhr.status == 200) { // got a response
      var svg = xhr.responseXML; // get somewhat bogus SVG
      // find the min and max coordinates of the background polygon
      var polyPoints = svg.getElementsByTagNameNS(svgNS, "polygon")[0].points;
      var x1,y1,x2,y2;
      for (var i = 0; i < polyPoints.numberOfItems; i++) {
	var point = polyPoints.getItem(i);
	if (x1 === undefined || point.x < x1) { x1 = point.x }
	if (y1 === undefined || point.y < y1) { y1 = point.y }
	if (x2 === undefined || point.x > x2) { x2 = point.x }
	if (y2 === undefined || point.y > y2) { y2 = point.y }
      }
      var width = x2 - x1;
      var height = y2 - y1;
      // make the viewBox
      var viewBox =  '' + x1 + ' ' + y1 + ' ' + width + ' ' + height;
      svg.documentElement.setAttributeNS(null, "viewBox", viewBox);
      // adjust width/height
      svg.documentElement.setAttributeNS(null, "width", '' + width + 'px');
      svg.documentElement.setAttributeNS(null, "height", '' + height + 'px');
      // get rid of bogus transform
      ( svg.getElementById("graph1") ||
	svg.getElementById("graph0")
      ).removeAttributeNS(null, "transform");
      
      // replace the <pre> element containing the dot code with the now
      // less bogus SVG
      dotPRE.parentNode.replaceChild(
	document.adoptNode(svg.documentElement),
	dotPRE
      );
    }
  };
  xhr.send(params);
}

function dotsToSVG(simple) {
  var PREs = document.getElementsByTagNameNS(xhtmlNS, "pre");
  for (var i = 0; i < PREs.length; i++) {
    if (/\bdot\b/.test(PREs[i].className)) {
      dotToSVG(PREs[i], simple);
    }
  }
}

function bodyLoaded() {
  initVars();
  initHypDisplay();
  downcaseHack();
  linkifyError();
  setAllDisplay();
  dotsToSVG();
  document.onchange = handleCheckboxChange;
}


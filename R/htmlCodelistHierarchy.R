#' Export a SNOMEDcodelist hierarchy to HTML
#'
#' Exports a codelist with hierarchy as HTML for easy viewing.
#'
#' @param x a SNOMEDcodelist, codelistHierarchy (output of
#'   showCodelistHierarchy), or an object which can be coerced to
#'   a SNOMEDcodelist (such as a SNOMEDconcept vector).
#' @param file filename to export to. If NULL, no file is written
#' @param title title of HTML document
#' @param description paragraph of description text (excluding
#'   <p></p> tags)
#' @param extracols character vector of additional columns of 
#'   codelist_with_hierarchy to include in HTML output
#' @param SNOMED environment containing the SNOMED dictionary to use
#' @param ... extra arguments to pass to as.SNOMEDcodelist
#' @return a character vector containing HTML output
#' @export
#' @seealso showCodelistHierarchy
#' @examples
#' SNOMED <- sampleSNOMED()
#'
#' my_concepts <- SNOMEDconcept('Acute heart failure')
#' my_codelist <- SNOMEDcodelist(data.frame(conceptId = my_concepts,
#'   include_desc = TRUE))
#' htmlCodelistHierarchy(my_codelist, file = paste0(tempdir(),
#'   'codelist.html'))
#' # The codelist.html file can now be viewed in a web browser
#'
#' # Clean up temporary file
#' file.remove(paste0(tempdir(), 'codelist.html'))
htmlCodelistHierarchy <- function(x, file = NULL, title = NULL,
	description = NULL, extracols = NULL, SNOMED = getSNOMED(), ...){

	included <- out <- rowid <- conceptId <- NULL
	roworder <- checked <- NULL
	
	if (!('codelistHierarchy' %in% class(x))){
		x <- showCodelistHierarchy(as.SNOMEDcodelist(x, SNOMED = SNOMED,
			...), SNOMED = SNOMED)
	}
	x <- data.table::copy(x)[order(roworder)]
	if (!is.null(extracols)){
		extracols <- intersect(colnames(x), extracols)
	}
	if (length(extracols) == 0){
		extracols <- NULL
	}

	x[, checked := as.logical(NA)]
	x[, comment := '...']
# TODO
# Columns:
# 1. 'Expand/Contract' button (toggle) with pressed / unpressed style
# 2. Term (red if deselected, bold if has children)
# 3. 'Included' column (Y / N) - innerHTML this is the definitive selection
# 4. Buttons for select / deselect, select/deselect tree

# row[ID] = according to whether expanded, and highlighted
# term[ID] = change colour according to whether selected
# include[ID] = change colour and text (Y/N) according to whether selected

# On hover, draw thick black border around button
# After every button click:
# - Remove highlighting
# - Make changes to cell contents
# - Make changes to formatting or buttons
# - Highlight changed rows

# Buttons for expand / contract = affect row id (hide/show)

# Buttons for include / exclude = affect all terms with relevant
# concepts
# Red text for exclude / include

# Function to export the entire codelist (included terms only)
# ('exportall')
# Loop through conceptIds (marked as a javascript vector),
# for each one look up the relevant term cell by row id
# to see if it is selected

# Highlight recently modified rows

	concept_dict <- paste(x[, list(dict = paste0('"',
		conceptId[1], '":', min(rowid))),
		by = conceptId]$dict, collapse = ', ')

	top <- paste0('<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01//EN"
"http://www.w3.org/TR/html4/strict.dtd"><html><head><title>',
ifelse(is.null(title), 'SNOMED CT codelist', title), '</title>',
'<meta http-equiv="content-type" content="text/html; charset=iso-8859-1">
<style type="text/css">
table {border-spacing: 0px; font-family:Arial;}
td, th {border-bottom: 1px solid #ddd; padding: 1px; text-align: left}
.button {border: 1px solid white}
.button:hover {border: 1px solid black}
.tree {color: black}
.add {background-color: green; color: white}
.remove {background-color: red; color: white}
tr:hover {background-color: #D6EEEE;}</style>',
'<script type="text/javascript">
const c_allrows = [', paste(x$rowid, collapse = ','), '];
function showuncheckedrows(rows){
  if (document.getElementById("showuncheckedbutton").innerHTML ==
      "Show unchecked concepts only"){
    clearhighlight();
    let all_checked = true;
    rows.forEach(function(thisrow) {
      if (document.getElementById("checked".concat(thisrow)).innerHTML == "Y"){
        hiderow(thisrow);
      } else {
        all_checked = false;
        showrow(thisrow);
      }
    })
    if (all_checked == true){
      document.getElementById("showuncheckedbutton").innerHTML =
        "<strong>ALL CONCEPTS CHECKED!</strong>";
    }
  }
}
function toggle(thisrow, childrows, descendantrows){
  clearhighlight();
  if (document.getElementById("buttree".concat(thisrow)).innerHTML == "Expand"){
    childrows.forEach(showrow);
    childrows.forEach(highlightrow);
    highlightrow(thisrow);
    changebuttoncontract(thisrow);
  } else {
    childrows.forEach(changebuttonexpand);
    descendantrows.forEach(hiderow);
    highlightrow(thisrow);
    changebuttonexpand(thisrow);
  }
}
function changebuttonexpand(rownum){
  var mybutton = document.getElementById("buttree".concat(rownum));
  if (typeof(mybutton) != "undefined" && mybutton != null){
    mybutton.innerHTML = "Expand"
  }
}
function changebuttoncontract(rownum){
  var mybutton = document.getElementById("buttree".concat(rownum));
  if (typeof(mybutton) != "undefined" && mybutton != null){
    mybutton.innerHTML = "Contract"
  }
}
function clearhighlight(){
  /* Clears all highlights on all rows */
  c_allrows.forEach(backwhite);
}
function selectrow(rownum){
  document.getElementById("term".concat(rownum)
    ).style.color = "black";
  document.getElementById("include".concat(rownum)
    ).style.color = "black";
  document.getElementById("checked".concat(rownum)
    ).style.color = "black";
  document.getElementById("include".concat(rownum)
    ).innerHTML = "Y";
}
function checkrow(rownum){
  document.getElementById("checked".concat(rownum)
    ).innerHTML = "Y";
}
function checkrows(rows_to_check){
  clearhighlight();
  rows_to_check.forEach(checkrow);
  rows_to_check.forEach(highlightrow);
  /* Check if all are checked */
  var all_checked = true
  c_allrows.forEach(function(i) {
    if (document.getElementById("checked".concat(i)).innerHTML != "Y"){
      all_checked = false;
    }
  })
  if (all_checked == true){
    document.getElementById("showuncheckedbutton").innerHTML =
      "<strong>ALL CONCEPTS CHECKED!</strong>";
  }
}
function selectrows(thisrow, rows_to_select){
  clearhighlight();
  rows_to_select.forEach(selectrow);
  rows_to_select.forEach(checkrow);
  rows_to_select.forEach(highlightrow);
}
function deselectrow(rownum){
  document.getElementById("term".concat(rownum)
    ).style.color = "red";
  document.getElementById("include".concat(rownum)
    ).style.color = "red";
  document.getElementById("checked".concat(rownum)
    ).style.color = "red";
  document.getElementById("include".concat(rownum)
    ).innerHTML = "N";
}
function deselectrows(thisrow, rows_to_deselect){
  clearhighlight();
  rows_to_deselect.forEach(deselectrow);
  rows_to_deselect.forEach(checkrow);
  rows_to_deselect.forEach(highlightrow);
}
function uncheckrow(rownum){
  document.getElementById("checked".concat(rownum)
    ).innerHTML = "N";
  document.getElementById("showuncheckedbutton").innerHTML =
    "Show unchecked concepts only"
}
function uncheckrows(rows_to_uncheck){
  clearhighlight();
  rows_to_uncheck.forEach(uncheckrow);
  rows_to_uncheck.forEach(highlightrow);
}
function highlightrow(rownum){
  document.getElementById("row".concat(rownum)
    ).style.backgroundColor = "#D6EEEE";
}
function backwhite(rownum){
  document.getElementById("row".concat(rownum)
    ).style.backgroundColor = "white";
}
function hiderow(rownum){
  document.getElementById("row".concat(rownum)
    ).style.display = "none";
}
function showrow(rownum){
  document.getElementById("row".concat(rownum)
    ).style.display = "";
}
function hideall(allrows, alldescendantrows){
  clearhighlight();
  allrows.forEach(changebuttonexpand);
  alldescendantrows.forEach(hiderow);
}
function showall(rows){
  clearhighlight();
  rows.forEach(changebuttoncontract);
  rows.forEach(showrow);
}
function addcomment(thisrow_ids){
  /* Get comment from user */
  var comment = prompt("Comment", "").replace("<", "").replace(">", "");
  if (comment == "") {comment = "...";}
  thisrow_ids.forEach(function(i) {
    var cells = document.getElementById("comment".concat(i)
      ).innerHTML.split(">");
    document.getElementById("comment".concat(i)
      ).innerHTML = cells[0] + ">" + comment + "</a>";
  })
}
function exportall(){
  /* Get filename to export to */
  var filename = document.getElementById("exportfilename").value;
  /* Exports all selected terms to a text document */
  const dictionary = {', concept_dict, '}
  var output = "conceptId,term,include_desc,included,checked,comment\\r\\n"
  var space = document.getElementById("sample").innerHTML
  var term
  var included
  var comment
  var checked
  /* Loop through concepts, check the relevant included field */
  for (var conceptId in dictionary) {
    if (dictionary.hasOwnProperty(conceptId)) {
      if (document.getElementById("include".concat(
        dictionary[conceptId])).innerHTML == "Y"){
        included = "TRUE"
      } else {
        included = "FALSE"
      }
      /* parse comment */
      comment = document.getElementById("comment".concat(
        dictionary[conceptId])).innerHTML.replace("</a>",
        "").replace("<a href", "").split(">")[1]
      if (comment == "..."){
        comment = ""
      }
      if (document.getElementById("checked".concat(
        dictionary[conceptId])).innerHTML == "Y"){
        checked = "TRUE"
      } else {
        checked = "FALSE"
      }
      /* parse term */
      term = document.getElementById("term".concat(dictionary[conceptId])).innerHTML
      term = term.replace("<strong>",
        "").replace("</strong>", "").split(space
        ).join("").split("\\"").join(",")
      /* to print to file */
      output = output + conceptId + ",\\"" + term + "\\",FALSE," +
        included + "," + checked + ",\\"" + comment + "\\"\\r\\n";
    }
  }
  
  /* To debug output */
  // console.log(output)
  
  /* Alternative approach */
  var link = document.createElement("a");
  if (filename == ""){
    filename = "codelist.csv"
  }
  if (/\\.csv$/.test(filename) == false){
    filename = filename + ".csv"
  }
  link.setAttribute("href",
    "data:text/csv;charset=utf-8,%EF%BB%BF" +
      encodeURIComponent(output));
  link.setAttribute("download", filename);
  document.body.appendChild(link);
  link.click();
  document.body.removeChild(link);
}
</script>
</head><body><h1>',
ifelse(is.null(title), 'SNOMED CT codelist', title), '</h1>',
ifelse(is.null(description), '', paste0('<h2>Description</h2><p>',
	description, '</p>')),
'<h2>Instructions</h2>
<p>This HTML document presents a hierarchy of SNOMED CT concepts.
In SNOMED CT, each concept has a distinct meaning and can
be linked to more general terms (ancestors) and more specific terms (descendants). The buttons allow you to explore the codelist at
different levels of the hierarchy, and mark whether or not you agree
with the inclusion of individual concepts or concept hierarchies. When
you have finished your review, you can download your final selection as a .CSV file by clicking the <strong>Export</strong> button below.</p>

<h3>Key to buttons for each concept</h3>
<ul>
<li><button class="button tree">Expand</button> Show descendants of this concept</li>
<li><button class="button tree">Contract</button> Hide descendants of this concept</li>
<li><button class="button tree"><strong>?</strong></button> Mark as unchecked</li>
<li><button class="button add"><strong>+</strong></button> Add a concept</li>
<li><button class="button remove"><strong>-</strong></button> Remove a concept</li>
<li><button class="button add"><strong>++</strong></button> Add a concept and all descendants</li>
<li><button class="button remove"><strong>--</strong></button> Remove a concept and all descendants</li>
</ul>

<h2>Reviewing tools</h2>
<p><button class="button tree" onclick="hideall(c_allrows, [',
paste(unique(unlist(x$descendantrowid)), collapse = ','), ']);">',
'Show top-level concepts only</button> ',
'<button class="button tree" onclick="showall(c_allrows);">',
'Show all concepts</button></p>
<p><button class="button add" onclick="checkrows(c_allrows);">',
'<strong>Mark all concepts as "checked"</strong></button> ',
'<button class="button remove" onclick="uncheckrows(c_allrows);">',
'<strong>Mark all concepts as "unchecked"</strong></button> ',
'<button id="showuncheckedbutton" class="button tree"
onclick="showuncheckedrows(c_allrows);">',
'Show unchecked concepts only</button></p>
<p><button id="exportbutton" class="button tree" onclick="exportall()">',
'<strong>Export</strong></button> to
<input id="exportfilename" value="', title, '"> .csv
</p>

<table style="width:100%">
<tr><th>Expand</th><th>SNOMED CT concept</th>',
ifelse(is.null(extracols), '',
paste0('<th>', extracols, '</th>', collapse = '')),
'<th>Comment</th><th>Checked</th><th>Included</th><th></th></tr>\n',
collapse = '')

	expand_buttons <- function(i){
		# Button for contracting
		if (length(x[i]$childrowid[[1]]) > 0){
			return(paste0(
			'<button class="button tree" background-color="#D6D6D6" id="buttree', x[i]$rowid,
			'" onclick="toggle(', x[i]$rowid, 
			', [', paste(unlist(x[i]$childrowid[[1]]), collapse = ','),
			'], [', paste(unlist(x[i]$descendantrowid[[1]]), collapse = ','),
			'])">Contract</button>'))
		} else {
			return('')
		}
	}

	select_buttons <- function(i){
		# Produces HTML code for buttons for appropriate row
		out <- paste0(
			'<button class="button tree" onclick="uncheckrows([',
			paste(unlist(x[i]$allthisrowid[[1]]),
			collapse = ','),'])"><strong>?</strong></button>',
			'<button class="button add" onclick="selectrows(', x[i]$rowid, 
			', [', paste(unlist(x[i]$allthisrowid[[1]]),
			collapse = ','),'])"><strong>+</strong></button>',
			'<button class="button remove" onclick="deselectrows(',
			x[i]$rowid, ', [', paste(unlist(x[i]$allthisrowid[[1]]),
			collapse = ','),'])"><strong>-</strong></button>'
		)
		
		# If this term has children
		if (length(x[i]$childrowid[[1]]) > 0){
			out <- paste0(out,
			'<button class="button add" onclick="selectrows(', x[i]$rowid, 
			', [', paste(unlist(x[i]$alldescendantrowid[[1]]),
			collapse = ','),'])"><strong>++</strong></button>',
			'<button class="button remove" onclick="deselectrows(',
			x[i]$rowid, ', [', paste(unlist(x[i]$alldescendantrowid[[1]]),
			collapse = ','),'])"><strong>--</strong></button>'
			)
		}
		out
	}

	middle <- lapply(1:nrow(x), function(i){
		# Creating each table row
		# Columns: Expand/Contract (button), Term, Extracols, Comment,
		# Checked (Y/N/blank), Include (Y/N), Select (buttons)
		paste(c('<tr id="row', x$rowid[i],
			'" style="background-color:white;"><td>',
			expand_buttons(i),
			'</td><td id="term', x$rowid[i], '" ',
			# style information
			ifelse(x$included[i] == TRUE, 'style="color:black;"',
				'style="color:red;"'
			), '>', rep('&middot;&emsp;', x$gen[i] - 1), # indent
			#), ' text-indent=', 2 * (x$gen[i] - 1), 'em>', # indent
			# content
			ifelse(length(x[i]$childrowid[[1]]) > 0,
				# this term has children
				paste0('<strong>', x$term[i], '</strong>'),
				# this term does not have children
				x$term[i]
			),
			ifelse(is.null(extracols), '', 
				paste0(sapply(extracols, function(y){
					paste0('<td>', x[i][[y]], '</td>')
			}), collapse = '')),
			'</td><td id="comment', x$rowid[i], '">',
			'<a href="#" onclick="addcomment([',
			paste(unlist(x[i]$allthisrowid[[1]]), collapse = ','),'])">',
			x$comment[i],
			'</a>',
			'</td><td id="checked', x$rowid[i], '" ',
			ifelse(x$included[i] == TRUE,
				'style="color:black;">',
				'style="color:red;">'
			),
			ifelse(is.na(x$checked[i]), '',
				ifelse(x$checked[i] == TRUE, 'Y', 'N')),
			'</td><td id="include', x$rowid[i], '" ',
			ifelse(x$included[i] == TRUE,
				'style="color:black;">Y',
				'style="color:red;">N'
			),
			'</td><td>',
			select_buttons(i),
			'</td></tr>\n'),
		collapse = '')
	})

	bottom <- paste0('\n</table><p id="sample"',
		'style="display:none">&middot;&emsp;</p></body></html>')

	if (!is.null(file)){
		write(paste0(c(top, unlist(middle), bottom)), file = file, ncolumns = 1)
	}
	invisible(out)
}

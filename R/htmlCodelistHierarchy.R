#' Export a SNOMEDcodelist hierarchy to HTML
#'
#' Exports a codelist with hierarchy as HTML for easy viewing.
#'
#' @param codelist_with_hierarchy output of showCodelistHierarchy
#' @param file filename to export to. If NULL, no file is written
#' @param title title of HTML document
#' @param description paragraph of description text to fit within
#'   <p></p> HTML tags 
#' @return a character vector containing HTML output
#' @export
#' @seealso showCodelistHierarchy
#' @examples
#' SNOMED <- sampleSNOMED()
#'
#' my_concepts <- SNOMEDconcept('Heart failure')
#' my_codelist <- SNOMEDcodelist(data.frame(conceptId = my_concepts,
#'   include_desc = TRUE))
#' codelist_with_hierarchy <- showCodelistHierarchy(my_codelist)
#' htmlCodelistHierarchy(codelist_with_hierarchy)
htmlCodelistHierarchy <- function(codelist_with_hierarchy,
	file = NULL, title = NULL, description = NULL){

	included <- out <- NULL
	x <- codelist_with_hierarchy

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

	top <- paste('<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01//EN"
"http://www.w3.org/TR/html4/strict.dtd"><html><head><title>',
ifelse(is.null(title), 'SNOMED CT codelist', title), '</title>',
'<meta http-equiv="content-type" content="text/html; charset=iso-8859-1">
<style type="text/css">
table {border-spacing: 0px; font-family:Arial;}
td, th {border-bottom: 1px solid #ddd; padding: 1px}
.button {border: 1px solid white}
.button:hover {border: 1px solid black}
.buttree {background-color: grey}
.add {background-color: green; color: white}
.remove {background-color: red; color: white}
tr:hover {background-color: #D6EEEE;}</style>',
'<script type="text/javascript">
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
    mybutton.backgroundColor = "yellow"
  }
}
function changebuttoncontract(rownum){
  var mybutton = document.getElementById("buttree".concat(rownum));
  if (typeof(mybutton) != "undefined" && mybutton != null){
    mybutton.innerHTML = "Contract"
    mybutton.backgroundColor = "grey"
  }
}
function clearhighlight(){
  /* Clears all highlights on all rows */
  [', paste(x$rowid, collapse = ','), '].forEach(backwhite);
}
function selectrow(rownum){
  document.getElementById("term".concat(rownum)
    ).style.color = "black";
  document.getElementById("include".concat(rownum)
    ).style.color = "black";
  document.getElementById("include".concat(rownum)
    ).innerHTML = "Y";
}
function selectrows(thisrow, rows_to_select){
  clearhighlight();
  rows_to_select.forEach(selectrow);
  rows_to_select.forEach(highlightrow);
}
function deselectrow(rownum){
  document.getElementById("term".concat(rownum)
    ).style.color = "red";
  document.getElementById("include".concat(rownum)
    ).style.color = "red";
  document.getElementById("include".concat(rownum)
    ).innerHTML = "N";
}
function deselectrows(thisrow, rows_to_deselect){
  clearhighlight();
  rows_to_deselect.forEach(deselectrow);
  rows_to_deselect.forEach(highlightrow);
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
function exportall(){
  /* Exports all selected terms to a text document */
  const dictionary = {', concept_dict, '}
  /* Loop through concepts, check the relevant included field */
  for (var key in dictionary) {
    if (dictionary.hasOwnProperty(key)) {
      if (document.getElementById("include".concat(
        dictionary[key])).innerHTML == "Y"){
          /* to print to file */
          console.log(key, document.getElementById("term".concat(
            dictionary[key])).innerHTML.replace("<strong>",
            "").replace("</strong>", "").replace("· ", ""));
      }
    }
  }
  
  /* Export the list of SNOMED CT concepts and descriptions */
}
</script>
</head><body><h1>',
ifelse(is.null(title), 'SNOMED CT codelist', title), '</h1><p>',
ifelse(is.null(description), '', description),
'</p><p> <a href="#" onclick="hideall([',
paste(x$rowid, collapse = ','), '], [',
paste(unique(unlist(x$descendantrowid)), collapse = ','),
']);">Show top-level concepts only</a> | <a href="#" onclick="showall([',
paste(x$rowid, collapse = ','),
']);">Show all concepts</a> |
<a href="#" onclick="exportall()">Export selection</a>
</p>
<table style="width:100%">
<tr><th>Expand</th><th>SNOMED CT concept</th>
<th>Include</th><th>Select</th></tr>\n', collapse = '')

	expand_buttons <- function(i){
		# Button for contracting
		if (length(x[i]$childrowid[[1]]) > 0){
			return(paste0(
			'<button class="button tree" color="white" id="buttree', x[i]$rowid,
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
		# Columns: Expand/Contract (button), Term, Include (Y/N), Select (buttons)
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
			'</td><td id="include', x$rowid[i], '" ',
			ifelse(x$included[i] == TRUE,
				'style="color:black;">Y',
				'style="color:red;">N'
			), '</td><td>',
			select_buttons(i),
			'</td></tr>\n'),
		collapse = '')
	})

	bottom <- '\n</table></body></html>'

	if (!is.null(file)){
		write(paste0(c(top, unlist(middle), bottom)), file = file, ncolumns = 1)
	}
	invisible(out)
}

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
htmlCodelistHierarchyOLD <- function(codelist_with_hierarchy,
	file = NULL, title = NULL, description = NULL){

	included <- out <- NULL
	x <- codelist_with_hierarchy


# TODO
# Red text for excluded
# Buttons for expand / contract / include / exclude
# Function to export the entire codelist (included terms only)

	top <- paste('<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01//EN"
"http://www.w3.org/TR/html4/strict.dtd"><html><head><title>',
ifelse(is.null(title), 'SNOMED CT codelist', title), '</title>',
'<meta http-equiv="content-type" content="text/html; charset=iso-8859-1">
<style type="text/css">
table {border-spacing: 0px; font-family:Arial;}
td, th {border-bottom: 1px solid #ddd; padding: 1px}
tr:hover {background-color: #D6EEEE;}</style>',
'<script type="text/javascript">
function toggle(thisrow, childrows, descendantrows){
  if (document.getElementById("row".concat(thisrow)).style.backgroundColor === "yellow"){
    backwhite(thisrow);
    childrows.forEach(showrow);
  } else {
    backyellow(thisrow);
    descendantrows.forEach(hiderow);
  }
}
function backyellow(rownum){
  document.getElementById("row".concat(rownum )).style.backgroundColor = "yellow";
}
function backwhite(rownum){
  document.getElementById("row".concat(rownum)).style.backgroundColor = "white"
}
function hiderow(rownum){
  document.getElementById("row".concat(rownum)).style.display = "none";
}
function showrow(rownum){
  document.getElementById("row".concat(rownum)).style.display = "";
}
function hideall(rows){
  rows.forEach(hiderow);
}
function showall(rows){
  rows.forEach(showrow);
  rows.forEach(backwhite);
}</script>
</head><body><h1>',
ifelse(is.null(title), 'SNOMED CT codelist', title), '</h1><p>',
ifelse(is.null(description), '', description),
'</p><p> <a href="#" onclick="hideall([',
paste(unique(unlist(x$descendantrowid)), collapse = ','),
']);">Show top-level concepts only</a> | <a href="#" onclick="showall([',
paste(x$rowid, collapse = ','),
']);">Show all concepts</a> | <a href="#" onclick="hideall([',
paste(x[included == FALSE]$rowid, collapse = ','),
']);">Hide excluded concepts</a></p>
<table style="width:100%">
<tr><th style="font-size:1%; color:white;">SNOMED CT concept ID</th><th>Concept description</th></tr>\n', collapse = '')

	middle <- lapply(1:nrow(x), function(i){
		# Creating each table row
		paste(c('<tr id="row', x$rowid[i],
			'" ', ifelse(length(x[i]$childrowid[[1]]) > 0,
				# this term has children
				'style="background-color:yellow;"',
				# this term does not have children
				''
			),
			'><td style="font-size:1%; color:white;">',
			as.character(x[i]$conceptId), '</td><td ',
			# style information
			ifelse(x$included[i] == TRUE, '',
				'style="background-color:grey;"'
			), '>', rep('&sdot;    ', x$gen[i] - 1), # indent
			# content
			ifelse(length(x[i]$childrowid[[1]]) > 0,
				# this term has children
				paste0('<a href="#row', x[i]$rowid,
					'" onclick="toggle(', x$rowid[i],
					', [', paste(unlist(x[i]$childrowid[[1]]),
					collapse = ','),'], [',
					paste(unlist(x[i]$descendantrowid[[1]]),
					collapse = ','), ']);"><strong>', 
					x$term[i], '</strong></a>'),
				# this term does not have children
				x$term[i]
			),
			'</td></tr>\n'),
		collapse = '')
	})

	bottom <- '\n</table></body></html>'

	if (!is.null(file)){
		write(paste0(c(top, unlist(middle), bottom)), file = file, ncolumns = 1)
	}
	invisible(out)
}

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
# Red text for excluded
# Buttons for expand / contract / include / exclude
# Function to export the entire codelist (included terms only)
# Highlight recently modified rows

	top <- paste('<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01//EN"
"http://www.w3.org/TR/html4/strict.dtd"><html><head><title>',
ifelse(is.null(title), 'SNOMED CT codelist', title), '</title>',
'<meta http-equiv="content-type" content="text/html; charset=iso-8859-1">
<style type="text/css">
table {border-spacing: 0px; font-family:Arial;}
td, th {border-bottom: 1px solid #ddd; padding: 1px}
.tree {background-color: white; color: black}
.add {background-color: green; color: white}
.remove {background-color: red; color: white}
tr:hover {background-color: #D6EEEE;}</style>',
'<script type="text/javascript">
function toggle(thisrow, childrows, descendantrows){
  if (document.getElementById("row".concat(thisrow)).style.backgroundColor === "yellow"){
    backwhite(thisrow);
    childrows.forEach(showrow);
  } else {
    backyellow(thisrow);
    descendantrows.forEach(hiderow);
  }
}
function showtree(thisrow, childrows){
  backwhite(thisrow);
  childrows.forEach(showrow);
  document.getElementById("butshow".concat(thisrow)).style.color = "white";
  document.getElementById("buthide".concat(thisrow)).style.color = "black";
}
function hidetree(thisrow, descendantrows){
  backyellow(thisrow);
  descendantrows.forEach(hiderow);
  document.getElementById("buthide".concat(thisrow)).style.color = "white";
  document.getElementById("butshow".concat(thisrow)).style.color = "black";
}
function selectrow(rownum){
  document.getElementById("row".concat(rownum )).style.color = "black";
}
function selecttree(treerows){
  treerows.forEach(selectrow);
}
function deselectrow(rownum){
  document.getElementById("row".concat(rownum )).style.color = "red";
}
function deselecttree(treerows){
  treerows.forEach(deselectrow);
}
function backyellow(rownum){
  document.getElementById("row".concat(rownum )).style.backgroundColor = "yellow";
}
function highlightrow(rownum){
  document.getElementById("row".concat(rownum )).style.backgroundColor = "#D6EEEE";
}
function backwhite(rownum){
  document.getElementById("row".concat(rownum )).style.backgroundColor = "white"
}
function hiderow(rownum){
  document.getElementById("row".concat(rownum )).style.display = "none";
}
function showrow(rownum){
  document.getElementById("row".concat(rownum )).style.display = "";
}
function hideall(rows){
  rows.forEach(hiderow);
}
function showall(rows){
  rows.forEach(showrow);
  rows.forEach(backwhite);
}</script>
</head><body><h1>',
ifelse(is.null(title), 'SNOMED CT codelist', title), '</h1><p>',
ifelse(is.null(description), '', description),
'</p><p> <a href="#" onclick="hideall([',
paste(unique(unlist(x$descendantrowid)), collapse = ','),
']);">Show top-level concepts only</a> | <a href="#" onclick="showall([',
paste(x$rowid, collapse = ','),
']);">Show all concepts</a> | <a href="#" onclick="hideall([',
paste(x[included == FALSE]$rowid, collapse = ','),
']);">Hide excluded concepts</a></p>
<table style="width:100%">
<tr><th>Tree</th><th>Select</th><th style="font-size:1%; color:white;">SNOMED CT concept ID</th><th>SNOMED CT concept description</th></tr>\n', collapse = '')

	buttons <- function(i){
		# If this term has children
		if (length(x[i]$childrowid[[1]]) > 0){
			out <- paste0(
			'<button style="tree" id="butshow', x[i]$rowid,
			'" onclick="showtree(', x[i]$rowid, 
			', [', paste(unlist(x[i]$childrowid[[1]]),
			collapse = ','),'])">+</button>',
			'<button style="tree" color="white" id="buthide', x[i]$rowid,
			'" onclick="hidetree(', x[i]$rowid, 
			', [', paste(unlist(x[i]$descendantrowid[[1]]),
			collapse = ','),'])">-</button></td><td>')
		} else {
			out <- '</td><td>'
		}
		
		
		# Produces HTML code for buttons for appropriate row
		out <- paste0(out,
			'<button style="add" onclick="selectrow(', x[i]$rowid, 
			')">+</button>',
			'<button style="remove" onclick="deselectrow(', x[i]$rowid, 
			')">-</button>'
		)
		
		# If this term has children
		if (length(x[i]$childrowid[[1]]) > 0){
			out <- paste0(out,
			'<button style="add" onclick="selecttree([', x[i]$rowid, 
			',', paste(unlist(x[i]$alldescendantrowid[[1]]),
			collapse = ','),'])">++</button>',
			'<button style="remove" onclick="deselecttree([', x[i]$rowid, 
			',', paste(unlist(x[i]$alldescendantrowid[[1]]),
			collapse = ','),'])">--</button>'
			)
		}
		out
	}

	middle <- lapply(1:nrow(x), function(i){
		# Creating each table row
		paste(c('<tr id="row', x$rowid[i],
			'" style="background-color:white;"><td>',
			buttons(i),
			'</td><td style="font-size:1%; color:white;">',
			as.character(x[i]$conceptId), '</td><td ',
			# style information
			ifelse(x$included[i] == TRUE, 'style="color:black;"',
				'style="color:red;"'
			), '>', rep('&emsp;', x$gen[i] - 1), # indent
			# content
			ifelse(length(x[i]$childrowid[[1]]) > 0,
				# this term has children
				paste0('<a href="#row', x[i]$rowid,
					'" onclick="toggle(', x$rowid[i],
					', [', paste(unlist(x[i]$childrowid[[1]]),
					collapse = ','),'], [',
					paste(unlist(x[i]$descendantrowid[[1]]),
					collapse = ','), ']);"><strong>', 
					x$term[i], '</strong></a>'),
				# this term does not have children
				x$term[i]
			),
			'</td></tr>\n'),
		collapse = '')
	})

	bottom <- '\n</table></body></html>'

	if (!is.null(file)){
		write(paste0(c(top, unlist(middle), bottom)), file = file, ncolumns = 1)
	}
	invisible(out)
}

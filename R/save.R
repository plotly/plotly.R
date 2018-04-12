writePlotToFileStringReplacement <- function(p, x, file, width = NULL, height = NULL) {
  html <- htmlwidgets:::toHTML(p)
  rendered <- htmltools::renderTags(html)

  print(rendered$html)
  print('foo')
  print(rendered$htmlwidget)

  id <- substr(rendered$html, 10, 20)

  linebreak <- regexpr('\n', rendered$html)
  print(linebreak)
  rendered$htmlwidget <- paste0("  ", substr(rendered$html, 1, linebreak))

  #  print(rendered$htmlwidget)

  # substitute width and height in htmlwidget as well
  pos1 <- regexpr('width:', rendered$htmlwidget) + 5 # after width:
  pos2 <- regexpr('height:', rendered$htmlwidget) + 6 # after height:
  pos3 <- pos2 + 6 # remains

  rendered$htmlwidget <- paste0(
    substr(rendered$htmlwidget, 1, pos1),
    width %||% x$width %||% "100%",
    substr(rendered$htmlwidget, pos1+5, pos2),
    height %||% x$height %||% 400,
    "px",
    substr(rendered$htmlwidget, pos3, nchar(rendered$htmlwidget))
  )
  
  #  print(rendered$htmlwidget)
  
  rendered$htmlwidget_sizing <- paste0(
    "<script type=\"application/htmlwidget-sizing\" data-for=\"",
    id,
    "\">{\"viewer\":{\"width\":",
    width %||% x$width %||% "100%", 
    ",\"height\":",
    height %||% x$height %||% 400,
    ",\"padding\":15,\"fill\":true},\"browser\":{\"width\":",
    width %||% x$width %||% "100%", 
    ",\"height\":",
    height %||% x$height %||% 400,
    ",\"padding\":40,\"fill\":true}}</script>"
  )
  
  html <- c(
    precompiled_html_header,
    "<body style=\"background-color:white;\">",
    "<div id=\"htmlwidget_container\">",
    rendered$htmlwidget,
    "</div>",
    substr(rendered$html, linebreak+1, nchar(rendered$html)),
    rendered$htmlwidget_sizing,
    "</body>",
    "</html>"
  )
  
  #  print(tmp)
  writeLines(html, file)
}

#library(xml2)

writePlotToFileXML2 <- function(p, x, file, width = NULL, height = NULL) {
  html <- htmlwidgets:::toHTML(p)
  rendered <- htmltools::renderTags(html)

  # We read the html to render with xml2.
  # read_html converts rendered$html to xml2 format and adds html and body tags
  html_full <- xml2::read_html(rendered$html)

  html_body <- xml2::xml_child( html_full )
  xml2::xml_attr( html_body, 'style') <- "background-color:white;"

  # Select child of body tag
  # This child contains the style attribute we want to edit
  html_body_div_htmlwidget <- xml2::xml_child(html_body)

  attr_width <- width %||% x$width
  if( is.null( attr_width ) ){
    attr_width <- "100%"
  }else{
    attr_width <- paste0( attr_width, "px")
  }
  attr_height <- height %||% x$height %||% "401"
  attr_height <- paste0( attr_height, "px")
  xml2::xml_attr(html_body_div_htmlwidget, 'style') <- paste0( "width:", attr_width, ";", "height:", attr_height, ";")
#  print(xml_child(html_body))

  xml2::xml_add_parent( html_body_div_htmlwidget, "div", id="htmlwidget_container")

  id <- xml2::xml_attr(html_body_div_htmlwidget, 'id')
  sizing_txt <- paste0('{"viewer":{"width":"', attr_width, '","height":"', attr_height, '","padding":15,"fill":true},"browser":{"width":"', attr_width, '","height":"', attr_height, '","padding":40,"fill":true}}')
  xml2::xml_add_child( html_body,
                 "script",
                 sizing_txt,
                 type="application/htmlwidget-sizing",
                 'data-for'=id
                 )


  html <- c(
    precompiled_html_header,
    as.character(html_body),
    "</html>"
  )

  #  print(tmp)
  writeLines(html, file)
#  write_xml( html_body, 'foo')
}

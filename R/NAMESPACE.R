#' @importFrom stringr str_count str_detect str_locate str_length
#' @importFrom xml2 read_html write_xml
#' @importFrom rvest html_attr html_nodes html_text repair_encoding guess_encoding
#' @importFrom tidyr gather spread separate
#' @importFrom rmarkdown render
#' @importFrom knitr knit knit_child
NULL 


#' @export
rmarkdown::render

#' @export
rvest::repair_encoding

#' @export
rvest::guess_encoding

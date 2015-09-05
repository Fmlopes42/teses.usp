#' @export
baixar_links <- function() {
  n <- pegar_num_pags()
  d <- dplyr::data_frame(pag = 1:n) %>%
    dplyr::group_by(pag) %>%
    dplyr::do(baixar_pagina(.$pag)) %>%
    dplyr::ungroup()
  d
}

pegar_num_pags <- function() {
  link <- 'http://www.teses.usp.br/index.php?'
  parm1 <- 'option=com_jumi&fileid=19&Itemid=87'
  parm2 <- '&lang=pt-br&g=1&b5=Faculdade%20de%20Direito&c5=u&o5=AND'
  u <- sprintf('%s%s%s&pagina=%d', link, parm1, parm2, 1)
  num <- httr::GET(u) %>%
    httr::content('text') %>%
    xml2::read_html() %>%
    rvest::html_nodes('.dadosLinha') %>%
    dplyr::first() %>%
    rvest::html_text() %>%
    stringr::str_extract('[0-9]+$')
  return(as.numeric(num))
}

baixar_pagina <- function(pag) {
  link <- 'http://www.teses.usp.br/index.php?'
  parm1 <- 'option=com_jumi&fileid=19&Itemid=87'
  parm2 <- '&lang=pt-br&g=1&b5=Faculdade%20de%20Direito&c5=u&o5=AND'
  u <- sprintf('%s%s%s&pagina=%d', link, parm1, parm2, pag)
  r <- httr::GET(u) %>%
    httr::content('text') %>%
    xml2::read_html() %>%
    rvest::html_nodes('.dadosLinha') %>%
    lapply(rvest::html_nodes, 'div')
  r <- r[sapply(r, length) > 0]
  d <- r %>%
    lapply(parse_node) %>%
    dplyr::bind_rows()
  return(d)
}

parse_node <- function(node) {
  nomes <- node %>%
    rvest::html_text()
  link <- node %>%
    rvest::html_nodes('a') %>%
    rvest::html_attr('href') %>%
    dplyr::first()
  d <- c(nomes, link) %>%
    as.data.frame(stringsAsFactors = FALSE) %>%
    t() %>%
    as.data.frame(stringsAsFactors = FALSE) %>%
    setNames(c('nome', 'titulo', 'area',
               'documento', 'unidade', 'ano', 'link')) %>%
    dplyr::tbl_df()
  d
}

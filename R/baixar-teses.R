#' baixa teses a partir do arquivo de links
#'
#' @export
baixar_teses <- function(links) {
  f <- dplyr::failwith(dplyr::data_frame(documento = 'erro'),
                       baixar_teses_um,
                       quiet = TRUE)
  d <- dplyr::data_frame(link = links) %>%
    dplyr::group_by(link) %>%
    dplyr::do(f(.$link)) %>%
    dplyr::ungroup()
  d
}

baixar_teses_um <- function(link) {
  txt <- link %>%
    httr::GET() %>%
    httr::content('text', encoding = 'UTF-8')
  if(is.na(txt)) {
    txt <- link %>%
      httr::GET() %>%
      httr::content('text')
  }
  txt <- txt %>%
    xml2::read_html() %>%
    rvest::html_node('#CorpoConteudoDocumento') %>%
    rvest::html_nodes('div > div') %>%
    rvest::html_text()
  nm <- txt[as.logical(1 - (seq_along(txt) %% 2))][1:16]
  txt <- txt[as.logical(seq_along(txt) %% 2)][-1][1:16]
  nm <- tjsp::arrumar_nomes(nm)
  d <- dplyr::data_frame(nm = nm, um = 1, txt = txt) %>%
    tidyr::spread(nm, txt)
  d
}

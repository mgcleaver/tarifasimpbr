#' Download de tarifas de importação do Brasil do site oficial da
#' Camara de Comerio Exterior (Camex)
#'
#' @description
#' Essa função faz o download do arquivo de tarifas vigentes do Brasil em um diretorio
#' temporario.
#'
#' @return o arquivo de tarifas é obtido do site da Camex

download_tarifas <- function() {
  link_camex <-
    "https://www.gov.br/mdic/pt-br/assuntos/camex/se-camex/strat/tarifas/vigentes"

  conteudo_html <- rvest::read_html(link_camex)

  link_arquivo_tarifas <- conteudo_html |>
    rvest::html_nodes("#content-core") |>
    rvest::html_nodes("p.callout") |>
    rvest::html_nodes("a") |>
    rvest::html_attr("href") |>
    stringr::str_subset("anexos-i")

  temp_path <- file.path(tempdir(), "tarifas_raw.xlsx")

  teste_download <- try(
    utils::download.file(
      link_arquivo_tarifas,
      destfile = temp_path,
      mode = 'wb'))

  if(class(teste_download) == "try-error") {
    stop("Download do arquivo de tarifas falhou")
  }

  return(temp_path)


}

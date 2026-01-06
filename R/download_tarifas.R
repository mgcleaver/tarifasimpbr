#' Download de tarifas de importação do Brasil do site oficial da
#' Camara de Comerio Exterior (Camex)
#'
#' @description
#' Essa função faz o download do arquivo de tarifas vigentes do Brasil em um diretorio
#' temporario.
#'
#' @return o caminho temporário, onde o arquivo oficial de tarifas foi salvo
#'
#' @export
download_tarifas <- function() {
  message("Iniciando download de arquivo de tarifas")
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

  if(inherits(teste_download, "try-error")) {
    stop("Download do arquivo de tarifas falhou")
  }

  message("Download realizado com sucesso")

  return(temp_path)


}

#' Download de tarifas de importação do Brasil do site oficial da
#' Camara de Comercio Exterior (Camex)
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

  link_arquivo_tarifas <- obter_link_arquivo_tarifas()

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

#' Obter o link do arquivo oficial de tarifas
#'
#' Funcao auxiliar para consultar a pagina oficial de tarifas vigentes e
#' identificar o link do arquivo Excel com os anexos tarifarios.
#'
#' @return
#' Uma string com o link do arquivo oficial de tarifas.
#'
#' @keywords internal
obter_link_arquivo_tarifas <- function() {
  link_camex <-
    "https://www.gov.br/mdic/pt-br/assuntos/camex/se-camex/strat/tarifas/vigentes"

  conteudo_html <- rvest::read_html(link_camex)

  link_arquivo_tarifas <- conteudo_html |>
    rvest::html_nodes("#content-core") |>
    rvest::html_nodes("p.callout") |>
    rvest::html_nodes("a") |>
    rvest::html_attr("href") |>
    stringr::str_subset("anexos-i")

  if (length(link_arquivo_tarifas) == 0) {
    stop("Link do arquivo de tarifas nao encontrado")
  }

  return(link_arquivo_tarifas[[1]])
}

#' Obter a data do arquivo oficial de tarifas
#'
#' @description
#' Consulta o link do arquivo oficial de tarifas vigentes do Brasil no site da
#' Camex e retorna a data presente no nome do arquivo.
#'
#' @return
#' Um objeto de classe `Date` com a data encontrada no link do arquivo oficial.
#'
#' @examples
#' \dontrun{
#' data_arquivo_tarifas()
#' }
#'
#' @export
data_arquivo_tarifas <- function() {
  link_arquivo_tarifas <- obter_link_arquivo_tarifas()

  extrair_data_link_tarifas(link_arquivo_tarifas)
}

#' Extrair a data do link do arquivo oficial de tarifas
#'
#' Funcao auxiliar para extrair a data no formato `dd-mm-aaaa` presente no link
#' do arquivo oficial de tarifas.
#'
#' @param link String com o link do arquivo oficial de tarifas.
#'
#' @return
#' Um objeto de classe `Date` com a data encontrada no link.
#'
#' @keywords internal
extrair_data_link_tarifas <- function(link) {
  data_link <- link |>
    stringr::str_extract("(?<=arquivos-listas/)\\d{2}-\\d{2}-\\d{4}(?=-)")

  if (length(data_link) == 0 || any(is.na(data_link))) {
    stop("Nao foi possivel identificar a data no link do arquivo de tarifas")
  }

  as.Date(data_link, format = "%d-%m-%Y")
}

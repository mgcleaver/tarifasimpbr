#' Adivinha numero de linhas a serem puladas para leitura correta do arquivo
#' @keywords internal
#' @noRd
obter_linha_cabecalho <- function(path, aba = NULL) {

  # Le tudo sem nomes de coluna
  tmp <- suppressMessages(
    readxl::read_excel(
      path,
      sheet = aba,
      col_names = FALSE
    ))

  # Procura a linha onde aparece "NCM"
  linha_cabecalho <- tmp |>
    dplyr::mutate(row = dplyr::row_number()) |>
    tidyr::pivot_longer(-row) |>
    dplyr::filter(toupper(as.character(.data$value)) == "NCM") |>
    dplyr::pull(row) |>
    unique()

  if (length(linha_cabecalho) == 0) {
    stop("Erro de leitura, nao ha nenhuma coluna 'NCM' na planilha.")
  }

  return(linha_cabecalho - 1)

}

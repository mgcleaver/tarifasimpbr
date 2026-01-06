#' Filtra as tarifas vigentes de listas de exceção
#'
#' Retorna apenas os registros cuja tarifa está **vigente hoje**,
#' considerando o intervalo entre `inicio_de_vigencia` e `termino_de_vigencia`.
#' A depender da lista consultada e da data pode ou não haver medidas em datas
#' futuras. No caso de não haver medidas em datas futuras, não haverá diferença
#' em aplicar ou não está função, uma vez que os dados serão idênticos.
#'
#' @param x uma lista de exceção obtida a partir da função `ler_anexo_formatado`.
#'   A função utiliza as colunas `inicio_de_vigencia` e `termino_de_vigencia`
#'   no formato `Date` para obter as tarifas vigentes.
#'
#' @return
#' O mesmo objeto de entrada, porém filtrado para conter apenas
#' as tarifas vigentes na data de execução da função.
#'
#' @examples
#' \dontrun{
#' x <- download_tarifas()
#' desabastecimento <- ler_anexo_formatado(x, "iv") # ler anexoi iv - desabastecimento
#' tarifas_vigentes(desabastecimento)
#' }
#'
#' @export
tarifas_vigentes <- function(x) {
  hoje <- Sys.Date()

  x |>
    dplyr::mutate(ref = hoje) |>
    dplyr::filter(
      dplyr::between(
        .data$ref,
        .data$inicio_de_vigencia,
        .data$termino_de_vigencia
      )
    ) |>
    dplyr::select(-.data$ref)
}

#' Filtra as tarifas futuras de listas de exceção
#'
#' Retorna apenas os registros cuja tarifa entrará em vigência no futuro. Compara
#' a data da execução da função com a data na coluna `inicio_de_vigencia` para
#' filtrar as tarifas futuras presentes em listas de exceção.
#'  No caso de não haver medidas em datas futuras, a função retornará um tibble
#'  vazio.
#'
#' @param x uma lista de exceção obtida a partir da função `ler_anexo_formatado`.
#'   A função utiliza as colunas `inicio_de_vigencia` e `termino_de_vigencia`
#'   no formato `Date` para obter as tarifas vigentes.
#'
#' @return o mesmo objeto de entrada, porém filtrado para conter apenas
#' as tarifas vigentes na data de execução da função.
#'
#' @examples
#' \dontrun{
#' x <- download_tarifas()
#' desabastecimento <- ler_anexo_formatado(x, "iv") # ler anexoi iv - desabastecimento
#' tarifas_futuras(desabastecimento)
#' }
#'
#' @export
tarifas_futuras <- function(x) {
  hoje <- Sys.Date()

  x |>
    dplyr::mutate(ref = hoje) |>
    dplyr::filter(
      .data$ref < .data$inicio_de_vigencia
    ) |>
    dplyr::select(-.data$ref)
}

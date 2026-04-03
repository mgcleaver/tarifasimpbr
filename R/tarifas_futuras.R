#' Filtra as tarifas futuras de listas de excecao
#'
#' Retorna apenas os registros cuja tarifa entrara em vigencia no futuro. Compara
#' a data da execucao da funcao com a data na coluna `inicio_de_vigencia` para
#' filtrar as tarifas futuras presentes em listas de excecao.
#'  No caso de nao haver medidas em datas futuras, a funcao retornara um tibble
#'  vazio.
#'
#' @param x uma lista de excecao obtida a partir da funcao `ler_anexo`.
#'   A funcao utiliza a coluna `inicio_de_vigencia` no formato `Date`
#'   para obter as tarifas futuras.
#'
#' @return o mesmo objeto de entrada, porem filtrado para conter apenas
#' as tarifas futuras na data de execucao da funcao.
#'
#' @examples
#' \dontrun{
#' x <- download_tarifas()
#' desabastecimento <- ler_anexo(x, "iv") # ler anexoi iv - desabastecimento
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
    dplyr::select(-ref)
}

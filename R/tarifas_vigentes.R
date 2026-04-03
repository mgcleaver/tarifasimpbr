#' Filtra as tarifas vigentes de listas de excecao
#'
#' Retorna apenas os registros cuja tarifa esta **vigente hoje**,
#' considerando o intervalo entre `inicio_de_vigencia` e `termino_de_vigencia`.
#' A depender da lista consultada e da data pode ou nao haver medidas em datas
#' futuras. No caso de nao haver medidas em datas futuras, nao havera diferenca
#' em aplicar ou nao esta funcao, uma vez que os dados serao identicos.
#'
#' @param x uma lista de excecao obtida a partir da funcao `ler_anexo`.
#'   A funcao utiliza as colunas `inicio_de_vigencia` e `termino_de_vigencia`
#'   no formato `Date` para obter as tarifas vigentes.
#'
#' @return
#' O mesmo objeto de entrada, porem filtrado para conter apenas
#' as tarifas vigentes na data de execucao da funcao.
#'
#' @examples
#' \dontrun{
#' x <- download_tarifas()
#' desabastecimento <- ler_anexo(x, "iv") # ler anexoi iv - desabastecimento
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
    dplyr::select(-ref)
}

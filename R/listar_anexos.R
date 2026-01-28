#' Funcao para consultar nome e numero dos anexos das tarifas de importacao
#'
#' Funcao pode ser utilizada para consulta rapida dos nomes e numeros dos anexos
#' de tarifas ou para verificar qual argumento usar na funcao `ler_anexo`.
#'
#' @return Retorna um tibble com numero do anexo e nome correspondente do anexo
#'
#' @examples
#' listar_anexos()
#'
#' @export
listar_anexos <- function() {
  tibble::tibble(
    numero_anexo = c("i", "ii", "iv", "v", "vi", "viii", "ix", "x"),
    nome_abreviado_anexo = c(
      "Tarifa Externa Comum",
      "Anexo II",
      "Desabastecimento",
      "Letec",
      "Lebitbk",
      "Concessoes OMC",
      "DCC",
      "Automotivos ACE-14"
    )
  )
}

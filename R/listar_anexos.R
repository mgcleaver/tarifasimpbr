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
    numero_anexo = c("i", "ii", "iii", "iv", "v", "vi", "viii", "ix", "x", "xi"),
    nome_abreviado_anexo = c(
      "Tarifa Externa Comum",
      "Anexo II",
      "Setor Aeronautico",
      "Desabastecimento",
      "Letec",
      "Lebitbk",
      "Concessoes OMC",
      "DCC",
      "Automotivos ACE-14",
      "Letec+"
    )
  )
}

# O XI nao existe nas planilhas anteriores a novembro de 2026. Os demais
# anexos continuam obrigatorios; erros de leitura nao devem ser silenciados.
anexos_excecao_disponiveis <- function(x) {
  anexos <- c("iv", "v", "vi", "viii", "ix", "x")
  nome_abas <- readxl::excel_sheets(x) |>
    stringr::str_to_lower() |>
    stringr::str_squish()

  if (any(stringr::str_detect(nome_abas, "^anexo\\s+xi\\b"))) {
    anexos <- c(anexos, "xi")
  }

  anexos
}

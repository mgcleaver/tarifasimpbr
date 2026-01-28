#' Seleciona colunas específicas das listas de exceções tarifárias e filtra
#' observações das listas de exceção que impactam a tarifa aplicada.
#'
#' Usada dentro da função principal `detalhar_listas_excecao_vigentes`.
#'
#' @keywords internal
#' @noRd
seleciona_tarifas <- function(
    x
) {
  padrao <- paste(
    "^ncm$",
    "aliquota",
    "inicio_de_vigencia",
    "termino_de_vigencia",
    "ato_de_inclusao",
    "data_do_ato_de_inclusao",
    "lista",
    sep = "|"
  )

  out <- x |>
    dplyr::filter(.data$altera_tarifa_aplicada == 1) |>
    dplyr::select(dplyr::matches(padrao))

  return(out)
}

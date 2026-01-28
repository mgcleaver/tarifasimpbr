#' Resume informações tarifárias a partir dos indicadores de destaque e quota
#'
#' Usada dentro da função principal `detalhar_listas_excecao_vigentes`.
#'
#' @keywords internal
#' @noRd
resume_tarifas_de_excecao <- function(x) {
  out <- x |>
    dplyr::group_by(.data$ncm, .data$lista) |>
    dplyr::summarise(
      contagem_quota = sum(.data$tem_quota == 1),
      contagem_ex = sum(.data$tem_destaque_ex == 1),
      tem_quota = any(.data$tem_quota == 1),
      tem_destaque_ex = any(.data$tem_destaque_ex == 1),
      ncm_integral = any(.data$ncm_integral == 1),
      .groups = 'drop'
    )

  return(out)
}

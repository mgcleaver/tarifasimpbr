#' Adiciona indicador de destaque (ex) e quota
#'
#' Usada dentro da função principal `detalhar_listas_excecao_vigentes` para
#' adicionar um indicador de destaque (ex) de quota e de presença integral de uma
#' NCM nos anexo iv, v, vi, xiii, ix, x.
#'
#' @param x deve ser o resultado de `tarifas_download()`. No caso o argumento
#'    é passado para esta função interna por meio do argumento x proveniente de
#'    `detalhar_listas_excecao_vigentes`.
#'
#' @param n_anexo é o número romano de um anexo relacionado com uma lista de
#'    exceção. As opções possíveis são iv, v, vi, viii, ix, x.
#'
#' @return
#' O mesmo objeto de entrada, porém com três colunas adicionais: quota, destaque_ex
#' e ncm_integral.
#'
#' @keywords internal
#' @noRd
adiciona_indicador_ex_quota <- function(
    x,
    n_anexo = c("iv", "v", "vi", "viii", "ix", "x")
) {
  n_anexo <- match.arg(
    n_anexo
  )

  suprime_texto <- function(y) {
    suppressMessages(
      suppressWarnings(
        y
      )
    )
  }

  out <- suprime_texto(
    ler_anexo(x = x, n_anexo = n_anexo) |>
      dplyr::mutate(
        quota = as.numeric(.data$quota),
        no_ex = as.numeric(.data$no_ex)
      ) |>
      dplyr::mutate(
        tem_quota = dplyr::if_else(is.na(.data$quota), 0, 1),
        tem_destaque_ex = dplyr::if_else(is.na(.data$no_ex), 0, 1),
        ncm_integral = dplyr::if_else(
          is.na(.data$no_ex),
          1,
          0
        ),
        altera_tarifa_aplicada = dplyr::if_else(
          is.na(.data$quota) & is.na(.data$no_ex),
          1,
          0
        )
      )
  )

  return(out)
}

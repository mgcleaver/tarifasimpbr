#' Adiciona indicador de destaque (ex) e quota
#'
#' Usada dentro da função principal `detalhar_listas_excecao_vigentes` para
#' adicionar, por NCM, a contagem de destaques (ex), a contagem de quotas, um indicador de presença
#' integral de NCM em lista de exceção e um indicador de alteração de tarifa aplicada
#' nos anexos iv, v, vi, viii, ix e x.
#'
#' @param x deve ser o resultado de `download_tarifas()`.
#'
#' @param n_anexo é o número romano de um anexo relacionado com uma lista de
#'    exceção. As opções possíveis são iv, v, vi, viii, ix, x.
#'
#' @return um tibble com as colunas originais e as colunas adicionadas
#' tem_quota, tem_destaque_ex, ncm_integral e altera_tarifa_aplicada.
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

  tem_valor <- function(y) {
    y <- y |>
      as.character() |>
      stringr::str_trim()

    !is.na(y) & y != "" & y != "-"
  }

  out <- suprime_texto(
    ler_anexo(x = x, n_anexo = n_anexo) |>
      dplyr::mutate(
        tem_quota = as.integer(tem_valor(.data$quota)),
        tem_destaque_ex = as.integer(tem_valor(.data$no_ex)),
        ncm_integral = dplyr::if_else(
          .data$tem_destaque_ex == 0,
          1,
          0
        ),
        altera_tarifa_aplicada = dplyr::if_else(
          .data$tem_quota == 0 & .data$tem_destaque_ex == 0,
          1,
          0
        )
      )
  )

  return(out)
}

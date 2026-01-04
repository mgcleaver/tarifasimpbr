detalhar_listas_excecao_vigentes <- function(x) {

  anexos <- c("iv", "v", "vi", "viii", "ix", "x")

  listas_detalhadas_vigentes <- purrr::map(
    anexos,
    ~ adiciona_indicador_ex_cota(x = x, n_anexo = .x) |>
      tarifas_vigentes()
  )

  resumo_tarifas_excecao <- purrr::map(
    listas_detalhadas_vigentes,
    ~ resume_tarifas_de_excecao(.x)
  ) |>
    dplyr::bind_rows() |>
    dplyr::group_by(ncm) |>
    dplyr::summarise(
      contagem_cota = sum(contagem_cota),
      contagem_ex = sum(contagem_ex),
      cota = as.numeric(any(cota)),
      destaque_ex = as.numeric(any(destaque_ex)),
      ncm_integral = as.numeric(any(ncm_integral)),
      lista = {
        n_lista <- unique(lista)
        paste0(n_lista, collapse = ", ")
      })

  return(resumo_tarifas_excecao)
}


adiciona_indicador_ex_cota <- function(
    x,
    n_anexo = c("iv", "v", "vi", "viii", "ix", "x")) {

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
    ler_anexo_formatado(x = x, n_anexo = n_anexo) |>
      dplyr::mutate(
        quota = as.numeric(quota),
        no_ex = as.numeric(no_ex)
      )  |>
      dplyr::mutate(
        cota = dplyr::if_else(is.na(quota), 0, 1),
        destaque_ex = dplyr::if_else(is.na(no_ex), 0, 1),
        ncm_integral = dplyr::if_else(
          is.na(quota) & is.na(no_ex),
          1,
          0))
  )

  return(out)

}

resume_tarifas_de_excecao <- function(x) {
  out <- x |>
    dplyr::group_by(ncm, lista) |>
    dplyr::summarise(
      contagem_cota = sum(cota == 1),
      contagem_ex = sum(destaque_ex == 1),
      cota = any(cota == 1),
      destaque_ex = any(destaque_ex == 1),
      ncm_integral = any(ncm_integral == 1),
      .groups = 'drop')

  return(out)
}

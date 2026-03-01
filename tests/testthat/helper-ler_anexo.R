tarifas_path <- NULL

get_tarifas_path <- function() {
  testthat::skip_on_cran()
  testthat::skip_if_offline()

  if (is.null(tarifas_path) || !file.exists(tarifas_path)) {
    tarifas_path <<- download_tarifas()
  }

  tarifas_path
}

expect_ncm_oito_digitos <- function(df) {
  testthat::expect_true(is.character(df$ncm))
  testthat::expect_true(all(stringr::str_detect(df$ncm, "^\\d{8}$")))
}

expect_colunas_anexo_lista <- function(df) {
  colunas_esperadas <- c(
    "ncm",
    "no_ex",
    "aliquota",
    "quota",
    "unidade_quota",
    "inicio_de_vigencia",
    "termino_de_vigencia",
    "lista"
  )
  testthat::expect_true(all(colunas_esperadas %in% names(df)))
}

expect_lista_correta <- function(df, n_anexo) {
  nome_lista_esperado <- listar_anexos() |>
    dplyr::filter(.data$numero_anexo == n_anexo) |>
    dplyr::pull(.data$nome_abreviado_anexo)

  testthat::expect_true(all(df$lista == nome_lista_esperado))
}

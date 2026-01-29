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

testthat::test_that("ler_anexo i retorna estrutura esperada", {
  x <- get_tarifas_path()

  anexo_i <- ler_anexo(x, "i")

  testthat::expect_true(tibble::is_tibble(anexo_i))
  testthat::expect_true(
    all(
      c(
        "ncm",
        "descricao_tec",
        "descricao_tec_concatenada",
        "tec",
        "bkbit",
        "resolucoes"
      ) %in% names(anexo_i)
    )
  )
  testthat::expect_true(is.numeric(anexo_i$tec))
  testthat::expect_true(all(anexo_i$tec >= 0, na.rm = TRUE))
  testthat::expect_true(
    all(is.na(anexo_i$bkbit) | anexo_i$bkbit %in% c("BIT", "BK"))
  )
  expect_ncm_oito_digitos(anexo_i)
})

testthat::test_that("ler_anexo iv e validacao basica", {
  x <- get_tarifas_path()

  anexo_iv <- ler_anexo(x, "iv")

  expect_ncm_oito_digitos(anexo_iv)
  expect_colunas_anexo_lista(anexo_iv)
})

testthat::test_that("ler_anexo v e validacao basica", {
  x <- get_tarifas_path()

  anexo_v <- ler_anexo(x, "v")

  expect_ncm_oito_digitos(anexo_v)
  expect_colunas_anexo_lista(anexo_v)
})

testthat::test_that("ler_anexo vi e validacao basica", {
  x <- get_tarifas_path()

  anexo_vi <- ler_anexo(x, "vi")

  expect_ncm_oito_digitos(anexo_vi)
  expect_colunas_anexo_lista(anexo_vi)
})

testthat::test_that("ler_anexo viii e validacao basica", {
  x <- get_tarifas_path()

  anexo_viii <- ler_anexo(x, "viii")

  expect_ncm_oito_digitos(anexo_viii)
  expect_colunas_anexo_lista(anexo_viii)
})

testthat::test_that("ler_anexo ix e validacao basica", {
  x <- get_tarifas_path()

  anexo_ix <- ler_anexo(x, "ix")

  expect_ncm_oito_digitos(anexo_ix)
  expect_colunas_anexo_lista(anexo_ix)
})

testthat::test_that("ler_anexo x e validacao basica", {
  x <- get_tarifas_path()

  anexo_x <- ler_anexo(x, "x")

  expect_ncm_oito_digitos(anexo_x)
  expect_colunas_anexo_lista(anexo_x)
})

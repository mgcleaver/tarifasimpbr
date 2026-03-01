testthat::test_that("integration: ler_anexo i retorna estrutura esperada", {
  x <- get_tarifas_path()

  anexo_i <- ler_anexo(x, "i")

  testthat::expect_s3_class(anexo_i, "tbl_df")
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

testthat::test_that("integration: ler_anexo ii padroniza estrutura e ncm", {
  x <- get_tarifas_path()

  anexo_ii <- ler_anexo(x, "ii")

  testthat::expect_s3_class(anexo_ii, "tbl_df")
  testthat::expect_gt(nrow(anexo_ii), 0)
  testthat::expect_true(
    all(c("ncm", "tec", "aliquota_aplicada") %in% names(anexo_ii))
  )
  testthat::expect_false(
    any(c("tec_percent", "aliquota_aplicada_percent") %in% names(anexo_ii))
  )
  expect_ncm_oito_digitos(anexo_ii)
  testthat::expect_false(any(stringr::str_detect(anexo_ii$ncm, "\\.")))
})

testthat::test_that("integration: ler_anexo iv e validacao basica", {
  x <- get_tarifas_path()

  anexo_iv <- ler_anexo(x, "iv")

  expect_ncm_oito_digitos(anexo_iv)
  expect_colunas_anexo_lista(anexo_iv)
  expect_lista_correta(anexo_iv, "iv")
})

testthat::test_that("integration: ler_anexo v e validacao basica", {
  x <- get_tarifas_path()

  anexo_v <- ler_anexo(x, "v")

  expect_ncm_oito_digitos(anexo_v)
  expect_colunas_anexo_lista(anexo_v)
  expect_lista_correta(anexo_v, "v")
})

testthat::test_that("integration: ler_anexo vi e validacao basica", {
  x <- get_tarifas_path()

  anexo_vi <- ler_anexo(x, "vi")

  expect_ncm_oito_digitos(anexo_vi)
  expect_colunas_anexo_lista(anexo_vi)
  expect_lista_correta(anexo_vi, "vi")
})

testthat::test_that("integration: ler_anexo viii e validacao basica", {
  x <- get_tarifas_path()

  anexo_viii <- ler_anexo(x, "viii")

  expect_ncm_oito_digitos(anexo_viii)
  expect_colunas_anexo_lista(anexo_viii)
  expect_lista_correta(anexo_viii, "viii")
})

testthat::test_that("integration: ler_anexo ix e validacao basica", {
  x <- get_tarifas_path()

  anexo_ix <- ler_anexo(x, "ix")

  expect_ncm_oito_digitos(anexo_ix)
  expect_colunas_anexo_lista(anexo_ix)
  expect_lista_correta(anexo_ix, "ix")
})

testthat::test_that("integration: ler_anexo x e validacao basica", {
  x <- get_tarifas_path()

  anexo_x <- ler_anexo(x, "x")

  expect_ncm_oito_digitos(anexo_x)
  expect_colunas_anexo_lista(anexo_x)
  expect_lista_correta(anexo_x, "x")
})

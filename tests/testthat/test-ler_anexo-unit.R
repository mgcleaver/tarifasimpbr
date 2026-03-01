testthat::test_that("unit: ler_anexo falha para n_anexo invalido", {
  testthat::expect_error(ler_anexo("qualquer_arquivo.xlsx", "foo"))
})

testthat::test_that("unit: ler_anexo falha quando arquivo nao existe", {
  testthat::expect_error(ler_anexo("arquivo_que_nao_existe.xlsx", "ii"))
})

testthat::test_that("unit: ler_anexo ii padroniza nomes e ncm", {
  testthat::local_mocked_bindings(
    excel_sheets = function(...) c("Anexo II"),
    read_excel = function(...) {
      tibble::tibble(
        NCM = c("12.34.56.78", "8765.43.21"),
        TEC_percent = c("10", "5"),
        Aliquota_Aplicada_percent = c("8", "4")
      )
    },
    .package = "readxl"
  )

  testthat::local_mocked_bindings(
    obter_linha_cabecalho = function(...) 0L,
    .package = "tarifasimpbr"
  )

  anexo_ii <- ler_anexo("arquivo_mock.xlsx", "ii")

  testthat::expect_s3_class(anexo_ii, "tbl_df")
  testthat::expect_gt(nrow(anexo_ii), 0)
  testthat::expect_true(all(c("ncm", "tec", "aliquota_aplicada") %in% names(anexo_ii)))
  testthat::expect_false(
    any(c("tec_percent", "aliquota_aplicada_percent") %in% names(anexo_ii))
  )
  expect_ncm_oito_digitos(anexo_ii)
  testthat::expect_false(any(stringr::str_detect(anexo_ii$ncm, "\\.")))
})

testthat::test_that("unit: ler_anexo iv aplica regras de padronizacao", {
  testthat::local_mocked_bindings(
    excel_sheets = function(...) c("Anexo IV - Desabastecimento"),
    read_excel = function(...) {
      tibble::tibble(
        ncm = c("12.34.56.78", "8765.43.21"),
        no_ex = c("1", "-"),
        aliquota_percent = c("2", "3"),
        quota = c("-", "100"),
        unidade_da_quota = c("-", "kg"),
        inicio_da_vigencia = c("01/01/2025", "02/01/2025"),
        termino_de_vigencia = c("-", "31/12/2030"),
        data_do_ato_de_inclusao = c("01/01/2025", "02/01/2025"),
        obs_geral = c("  - ", " Observacao ")
      )
    },
    .package = "readxl"
  )

  testthat::local_mocked_bindings(
    listar_anexos = function() {
      tibble::tibble(
        numero_anexo = c("iv"),
        nome_abreviado_anexo = c("Desabastecimento")
      )
    },
    obter_linha_cabecalho = function(...) 0L,
    .package = "tarifasimpbr"
  )

  anexo_iv <- ler_anexo("arquivo_mock.xlsx", "iv")

  expect_ncm_oito_digitos(anexo_iv)
  expect_colunas_anexo_lista(anexo_iv)
  testthat::expect_equal(anexo_iv$no_ex[[1]], "001")
  testthat::expect_true(is.na(anexo_iv$no_ex[[2]]))
  testthat::expect_true(is.na(anexo_iv$quota[[1]]))
  testthat::expect_true(is.na(anexo_iv$unidade_quota[[1]]))
  testthat::expect_equal(as.character(anexo_iv$termino_de_vigencia[[1]]), "9999-12-31")
  testthat::expect_equal(anexo_iv$lista[[1]], "Desabastecimento")
  testthat::expect_s3_class(anexo_iv$inicio_de_vigencia, "Date")
  testthat::expect_s3_class(anexo_iv$termino_de_vigencia, "Date")
  testthat::expect_s3_class(anexo_iv$data_do_ato_de_inclusao, "Date")
  testthat::expect_true(is.na(anexo_iv$obs[[1]]))
  testthat::expect_equal(anexo_iv$obs[[2]], "Observacao")
})

testthat::test_that("unit: ler_anexo vi remove coluna x10", {
  testthat::local_mocked_bindings(
    excel_sheets = function(...) c("Anexo VI - Lebitbk"),
    read_excel = function(...) {
      tibble::tibble(
        ncm = c("12.34.56.78"),
        no_ex = c("7"),
        aliquota_percent = c("4"),
        inicio_de_vigencia = c("01/01/2025"),
        termino_de_vigencia = c("31/12/2030"),
        x10 = c("coluna_ruido")
      )
    },
    .package = "readxl"
  )

  testthat::local_mocked_bindings(
    listar_anexos = function() {
      tibble::tibble(
        numero_anexo = c("vi"),
        nome_abreviado_anexo = c("Lebitbk")
      )
    },
    obter_linha_cabecalho = function(...) 0L,
    .package = "tarifasimpbr"
  )

  anexo_vi <- ler_anexo("arquivo_mock.xlsx", "vi")

  testthat::expect_false("x10" %in% names(anexo_vi))
  testthat::expect_equal(anexo_vi$no_ex[[1]], "007")
  testthat::expect_equal(anexo_vi$lista[[1]], "Lebitbk")
  expect_ncm_oito_digitos(anexo_vi)
})

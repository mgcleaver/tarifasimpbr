testthat::test_that("unit: ex_tarifarios filtra linhas com ex e padroniza colunas", {
  testthat::local_mocked_bindings(
    ler_anexo = function(x, n_anexo) {
      if (n_anexo == "iv") {
        return(
          tibble::tibble(
            ncm = c("12345678", "87654321"),
            no_ex = c("001", NA_character_),
            descricao = c("Produto A", "Produto B"),
            aliquota = c("2", "4"),
            quota = c("100", NA_character_),
            unidade_quota = c("kg", NA_character_),
            ato_de_inclusao = c("Res. 1", "Res. 2"),
            inicio_de_vigencia = as.Date(c("2020-01-01", "2020-01-01")),
            termino_de_vigencia = as.Date(c("2100-12-31", "2020-12-31")),
            lista = c("Desabastecimento", "Desabastecimento")
          )
        )
      }

      if (n_anexo %in% c("vi", "viii", "ix", "x")) {
        return(
          tibble::tibble(
            ncm = c("99999999"),
            no_ex = c(NA_character_),
            descricao = c("Sem ex"),
            aliquota = c("8"),
            inicio_de_vigencia = as.Date("2020-01-01"),
            termino_de_vigencia = as.Date("2100-12-31"),
            lista = c("Outra lista")
          )
        )
      }

      tibble::tibble(
        ncm = c("11112222"),
        no_ex = c("007"),
        descricao = c("Produto C"),
        aliquota = c("0"),
        quota = c(NA_character_),
        unidade_quota = c(NA_character_),
        ato_de_inclusao = c("Res. 3"),
        data_do_ato_de_inclusao = as.Date("2024-05-10"),
        obs = c("Observacao"),
        inicio_de_vigencia = as.Date("2024-01-01"),
        termino_de_vigencia = as.Date("2100-12-31"),
        lista = c("Letec")
      )
    },
    .package = "tarifasimpbr"
  )

  ex_tarifarios <- ex_tarifarios(x = "arquivo_mock.xlsx")

  testthat::expect_s3_class(ex_tarifarios, "tbl_df")
  testthat::expect_equal(nrow(ex_tarifarios), 2)
  testthat::expect_true(
    all(
      c(
        "lista",
        "ncm",
        "descricao",
        "no_ex",
        "tarifa_ex",
        "quota",
        "unidade_quota",
        "ato_de_inclusao",
        "data_do_ato_de_inclusao",
        "inicio_de_vigencia",
        "termino_de_vigencia",
        "vigente_hoje",
        "obs"
      ) %in% names(ex_tarifarios)
    )
  )
  testthat::expect_equal(ex_tarifarios$lista, c("Desabastecimento", "Letec"))
  testthat::expect_equal(ex_tarifarios$no_ex, c("001", "007"))
  testthat::expect_equal(ex_tarifarios$tarifa_ex, c("2", "0"))
  testthat::expect_equal(ex_tarifarios$quota, c("100", NA_character_))
  testthat::expect_true(is.na(ex_tarifarios$data_do_ato_de_inclusao[[1]]))
  testthat::expect_equal(ex_tarifarios$obs[[2]], "Observacao")
  testthat::expect_true(is.na(ex_tarifarios$obs[[1]]))
  testthat::expect_true(all(ex_tarifarios$vigente_hoje))
})

testthat::test_that("integration: ex_tarifarios retorna ex-tarifarios dos anexos IV a X", {
  x <- get_tarifas_path()

  ex_tarifarios <- ex_tarifarios(x)

  testthat::expect_s3_class(ex_tarifarios, "tbl_df")
  testthat::expect_gt(nrow(ex_tarifarios), 0)
  expect_ncm_oito_digitos(ex_tarifarios)
  testthat::expect_true(all(stringr::str_detect(ex_tarifarios$no_ex, "^\\d{3}$")))
  testthat::expect_true(
    all(
      ex_tarifarios$lista %in% c(
        "Desabastecimento",
        "Letec",
        "Lebitbk",
        "Concessoes OMC",
        "DCC",
        "Automotivos ACE-14"
      )
    )
  )
  testthat::expect_true(all(!is.na(ex_tarifarios$tarifa_ex)))
  testthat::expect_true(is.logical(ex_tarifarios$vigente_hoje))
  testthat::expect_true(all(ex_tarifarios$inicio_de_vigencia <= ex_tarifarios$termino_de_vigencia))
})

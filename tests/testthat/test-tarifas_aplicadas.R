testthat::test_that("unit: indicadores reconhecem quota e Ex preenchidos", {
  testthat::local_mocked_bindings(
    ler_anexo = function(x, n_anexo) {
      tibble::tibble(
        ncm = c("1", "2", "3", "4", "5"),
        quota = c("60.000 toneladas", NA, "", "-", "100"),
        no_ex = c(NA, "Ex 001", "", "-", "002")
      )
    },
    .package = "tarifasimpbr"
  )

  resultado <- tarifasimpbr:::adiciona_indicador_ex_quota(
    "arquivo_mock.xlsx",
    "iv"
  )

  testthat::expect_equal(resultado$tem_quota, c(1L, 0L, 0L, 0L, 1L))
  testthat::expect_equal(resultado$tem_destaque_ex, c(0L, 1L, 0L, 0L, 1L))
  testthat::expect_equal(resultado$ncm_integral, c(1L, 0L, 1L, 1L, 0L))
  testthat::expect_equal(
    resultado$altera_tarifa_aplicada,
    c(0L, 0L, 1L, 1L, 0L)
  )
  testthat::expect_equal(resultado$quota[[1]], "60.000 toneladas")
  testthat::expect_equal(resultado$no_ex[[2]], "Ex 001")
})

testthat::test_that("unit: indicadores aceitam quota e Ex numericos", {
  testthat::local_mocked_bindings(
    ler_anexo = function(x, n_anexo) {
      tibble::tibble(
        ncm = c("1", "2"),
        quota = c(100, NA_real_),
        no_ex = c(NA_real_, 1)
      )
    },
    .package = "tarifasimpbr"
  )

  resultado <- tarifasimpbr:::adiciona_indicador_ex_quota(
    "arquivo_mock.xlsx",
    "iv"
  )

  testthat::expect_equal(resultado$tem_quota, c(1L, 0L))
  testthat::expect_equal(resultado$tem_destaque_ex, c(0L, 1L))
  testthat::expect_equal(resultado$altera_tarifa_aplicada, c(0L, 0L))
})

testthat::test_that("unit: detalhamento nao transforma tarifas ausentes em zero", {
  testthat::local_mocked_bindings(
    ler_anexo = function(x, n_anexo) {
      if (n_anexo == "i") {
        return(
          tibble::tibble(
            ncm = c("00000000", "11111111"),
            descricao_tec = c("Ausente", "Preenchida"),
            descricao_tec_concatenada = c("Ausente.", "Preenchida."),
            tec = c(NA_real_, 10),
            bkbit = NA_character_,
            resolucoes = NA_character_
          )
        )
      }

      tibble::tibble(
        ncm = character(),
        aliquota_aplicada = numeric()
      )
    },
    detalhar_listas_excecao_vigentes = function(x) {
      tibble::tibble(
        ncm = character(),
        contagem_quota = integer(),
        contagem_ex = integer(),
        ncm_integral = integer(),
        lista = character(),
        tarifa_aplicada = numeric()
      )
    },
    .package = "tarifasimpbr"
  )

  simples <- tarifas_aplicadas("arquivo_mock.xlsx", detalhar = FALSE)
  detalhado <- tarifas_aplicadas("arquivo_mock.xlsx", detalhar = TRUE)

  testthat::expect_identical(detalhado$tec, simples$tec)
  testthat::expect_identical(detalhado$teb, simples$teb)
  testthat::expect_identical(
    detalhado$tarifa_aplicada,
    simples$tarifa_aplicada
  )
  testthat::expect_true(is.na(detalhado$tarifa_aplicada[[1]]))
  testthat::expect_equal(detalhado$contagem_quota, c(0L, 0L))
  testthat::expect_equal(detalhado$contagem_ex, c(0L, 0L))
  testthat::expect_equal(detalhado$ncm_integral, c(0L, 0L))
})

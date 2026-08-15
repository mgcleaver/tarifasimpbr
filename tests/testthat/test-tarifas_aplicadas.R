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
        tarifa_aplicada = numeric(),
        inicio_de_vigencia = as.Date(character()),
        termino_de_vigencia = as.Date(character()),
        ato_de_inclusao = character(),
        data_do_ato_de_inclusao = as.Date(character())
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
  testthat::expect_s3_class(detalhado$inicio_de_vigencia, "Date")
  testthat::expect_s3_class(detalhado$termino_de_vigencia, "Date")
  testthat::expect_type(detalhado$ato_de_inclusao, "character")
  testthat::expect_s3_class(detalhado$data_do_ato_de_inclusao, "Date")
  testthat::expect_true(all(is.na(detalhado$inicio_de_vigencia)))
  testthat::expect_true(all(is.na(detalhado$termino_de_vigencia)))
  testthat::expect_true(all(is.na(detalhado$ato_de_inclusao)))
  testthat::expect_true(all(is.na(detalhado$data_do_ato_de_inclusao)))
})

testthat::test_that("unit: metadados pertencem somente a NCMs integrais", {
  dados_listas <- tibble::tibble(
    n_anexo = c("iv", "iv", "v", "v", "ix"),
    ncm = c("10000000", "30000000", "20000000", "40000000", "10000000"),
    no_ex = c("001", "002", NA, NA, NA),
    aliquota = c("0", "0", "8", "4", "12"),
    quota = c(NA, NA, NA, "100 toneladas", NA),
    inicio_de_vigencia = as.Date(c(
      "2025-01-01", "2025-02-01", "2025-03-01", "2025-04-01", "2025-05-01"
    )),
    termino_de_vigencia = as.Date(c(
      "2025-12-31", "2025-12-31", "2026-03-01", "2026-04-01", "2026-05-01"
    )),
    ato_de_inclusao = c(
      "Res. Ex combinada",
      "Res. Ex isolada",
      "Res. Letec integral",
      "Res. Letec quota",
      "Res. DCC integral"
    ),
    data_do_ato_de_inclusao = as.Date(c(
      "2024-12-01", "2025-01-01", "2025-02-15", "2025-03-15", "2025-04-15"
    )),
    lista = c("Desabastecimento", "Desabastecimento", "Letec", "Letec", "DCC"),
    tem_quota = c(0L, 0L, 0L, 1L, 0L),
    tem_destaque_ex = c(1L, 1L, 0L, 0L, 0L),
    ncm_integral = c(0L, 0L, 1L, 1L, 1L),
    altera_tarifa_aplicada = c(0L, 0L, 1L, 0L, 1L)
  )

  testthat::local_mocked_bindings(
    adiciona_indicador_ex_quota = function(x, n_anexo) {
      dados_listas |>
        dplyr::filter(.data$n_anexo == .env$n_anexo) |>
        dplyr::select(-"n_anexo")
    },
    tarifas_vigentes = function(x) x,
    .package = "tarifasimpbr"
  )

  resultado <- detalhar_listas_excecao_vigentes("arquivo_mock.xlsx") |>
    dplyr::arrange(.data$ncm)

  testthat::expect_equal(
    resultado$lista,
    c("Desabastecimento, DCC", "Letec", "Desabastecimento", "Letec")
  )
  testthat::expect_equal(resultado$tarifa_aplicada, c(12, 8, NA, NA))
  testthat::expect_equal(
    resultado$inicio_de_vigencia,
    as.Date(c("2025-05-01", "2025-03-01", NA, NA))
  )
  testthat::expect_equal(
    resultado$termino_de_vigencia,
    as.Date(c("2026-05-01", "2026-03-01", NA, NA))
  )
  testthat::expect_equal(
    resultado$ato_de_inclusao,
    c("Res. DCC integral", "Res. Letec integral", NA, NA)
  )
  testthat::expect_equal(
    resultado$data_do_ato_de_inclusao,
    as.Date(c("2025-04-15", "2025-02-15", NA, NA))
  )
})

testthat::test_that("unit: tarifas detalhadas incluem metadados integrais", {
  testthat::local_mocked_bindings(
    ler_anexo = function(x, n_anexo) {
      if (n_anexo == "i") {
        return(
          tibble::tibble(
            ncm = c("10000000", "20000000"),
            descricao_tec = c("Integral", "Sem excecao"),
            descricao_tec_concatenada = c("Integral.", "Sem excecao."),
            tec = c(14, 16),
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
        ncm = "10000000",
        contagem_quota = 0L,
        contagem_ex = 0L,
        ncm_integral = 1L,
        lista = "DCC",
        tarifa_aplicada = 12,
        inicio_de_vigencia = as.Date("2025-05-01"),
        termino_de_vigencia = as.Date("2026-05-01"),
        ato_de_inclusao = "Res. DCC integral",
        data_do_ato_de_inclusao = as.Date("2025-04-15")
      )
    },
    .package = "tarifasimpbr"
  )

  simples <- tarifas_aplicadas("arquivo_mock.xlsx")
  detalhado <- tarifas_aplicadas("arquivo_mock.xlsx", detalhar = TRUE)
  metadados <- c(
    "inicio_de_vigencia",
    "termino_de_vigencia",
    "ato_de_inclusao",
    "data_do_ato_de_inclusao"
  )

  testthat::expect_false(any(metadados %in% names(simples)))
  testthat::expect_true(all(metadados %in% names(detalhado)))
  testthat::expect_equal(detalhado$inicio_de_vigencia[[1]], as.Date("2025-05-01"))
  testthat::expect_equal(detalhado$termino_de_vigencia[[1]], as.Date("2026-05-01"))
  testthat::expect_equal(detalhado$ato_de_inclusao[[1]], "Res. DCC integral")
  testthat::expect_equal(
    detalhado$data_do_ato_de_inclusao[[1]],
    as.Date("2025-04-15")
  )
  testthat::expect_true(all(is.na(detalhado[2, metadados])))
})

testthat::test_that("unit: NCM integral vigente duplicada gera erro", {
  dados_duplicados <- tibble::tibble(
    n_anexo = c("v", "ix"),
    ncm = "10000000",
    aliquota = "10",
    lista = c("Letec", "DCC"),
    tem_quota = 0L,
    tem_destaque_ex = 0L,
    ncm_integral = 1L,
    altera_tarifa_aplicada = 1L
  )

  testthat::local_mocked_bindings(
    adiciona_indicador_ex_quota = function(x, n_anexo) {
      dados_duplicados |>
        dplyr::filter(.data$n_anexo == .env$n_anexo) |>
        dplyr::select(-"n_anexo")
    },
    tarifas_vigentes = function(x) x,
    .package = "tarifasimpbr"
  )

  testthat::expect_error(
    detalhar_listas_excecao_vigentes("arquivo_mock.xlsx"),
    "mais de um registro integral vigente"
  )
})

testthat::test_that("integration: metadados detalhados correspondem aos anexos", {
  x <- get_tarifas_path()
  anexos <- c("iv", "v", "vi", "viii", "ix", "x")
  metadados <- c(
    "inicio_de_vigencia",
    "termino_de_vigencia",
    "ato_de_inclusao",
    "data_do_ato_de_inclusao"
  )

  esperado <- purrr::map(
    anexos,
    ~ tarifasimpbr:::adiciona_indicador_ex_quota(x, .x) |>
      tarifas_vigentes() |>
      tarifasimpbr:::seleciona_tarifas()
  ) |>
    dplyr::bind_rows() |>
    dplyr::select("ncm", dplyr::all_of(metadados)) |>
    dplyr::arrange(.data$ncm)

  resultado <- tarifas_aplicadas(x, detalhar = TRUE) |>
    dplyr::select("ncm", dplyr::all_of(metadados))

  resultado_integral <- resultado |>
    dplyr::semi_join(esperado, by = "ncm") |>
    dplyr::arrange(.data$ncm)

  resultado_sem_integral <- resultado |>
    dplyr::anti_join(esperado, by = "ncm")

  testthat::expect_equal(resultado_integral, esperado)
  testthat::expect_true(all(is.na(resultado_sem_integral[metadados])))
})

# Controla a data na copia local da funcao, sem alterar o relogio da sessao.
fim_vigencia_na_data <- function(data, ...) {
  consulta <- fim_vigencia
  environment(consulta) <- list2env(
    list(Sys.Date = function() as.Date(data)), parent = environment(consulta)
  )
  consulta(...)
}

testthat::test_that("unit: fim_vigencia respeita meses, limites e inclusao de hoje", {
  arquivo <- testthat::test_path("fixtures", "anexos-cache.xlsx")
  dados <- tibble::tibble(
    pleito = c(
      "ontem", "hoje", "inicio_futuro", "fim_mes", "proximo_mes",
      "fim_proximo_mes", "mes_seguinte", "fim_mes_seguinte", "depois",
      "ausente", "aberto"
    ),
    inicio_de_vigencia = as.Date(c(
      "2020-01-01", "2020-01-01", "2026-10-27", rep("2020-01-01", 8)
    )),
    termino_de_vigencia = as.Date(c(
      "2026-10-25", "2026-10-26", "2026-10-27", "2026-10-31",
      "2026-11-01", "2026-11-30", "2026-12-01", "2026-12-31",
      "2027-01-01", NA, "9999-12-31"
    ))
  )
  testthat::local_mocked_bindings(
    ler_anexo = function(x, n_anexo) {
      if (n_anexo == "iv") dados else dados[0, ]
    },
    .package = "tarifasimpbr"
  )

  corrente <- fim_vigencia_na_data("2026-10-26", arquivo)
  testthat::expect_identical(corrente$pleito, c("inicio_futuro", "fim_mes"))
  testthat::expect_identical(
    fim_vigencia_na_data("2026-10-26", arquivo, meses = 0), corrente
  )
  testthat::expect_identical(
    fim_vigencia_na_data("2026-10-26", arquivo, meses = 1)$pleito,
    c("inicio_futuro", "fim_mes", "proximo_mes", "fim_proximo_mes")
  )
  testthat::expect_identical(
    fim_vigencia_na_data("2026-10-26", arquivo, meses = 2)$pleito,
    c(
      "inicio_futuro", "fim_mes", "proximo_mes", "fim_proximo_mes",
      "mes_seguinte", "fim_mes_seguinte"
    )
  )
  testthat::expect_identical(
    fim_vigencia_na_data("2026-10-26", arquivo, incluir_hoje = TRUE)$pleito,
    c("hoje", "inicio_futuro", "fim_mes")
  )
  testthat::expect_equal(nrow(fim_vigencia_na_data("2026-10-31", arquivo)), 0L)
  testthat::expect_identical(
    fim_vigencia_na_data("2026-10-31", arquivo, incluir_hoje = TRUE)$pleito,
    "fim_mes"
  )
})

testthat::test_that("unit: fim_vigencia usa meses de calendario e anos bissextos", {
  arquivo <- testthat::test_path("fixtures", "anexos-cache.xlsx")
  cenarios <- tibble::tibble(
    hoje = c(
      "2026-04-15", "2026-01-31", "2028-01-31", "2026-12-31", "2026-03-31"
    ),
    meses = c(0, 1, 1, 1, 1),
    limite = as.Date(c(
      "2026-04-30", "2026-02-28", "2028-02-29", "2027-01-31", "2026-04-30"
    ))
  )
  dados <- NULL
  testthat::local_mocked_bindings(
    ler_anexo = function(x, n_anexo) {
      if (n_anexo == "iv") dados else dados[0, ]
    },
    .package = "tarifasimpbr"
  )

  for (i in seq_len(nrow(cenarios))) {
    dados <- tibble::tibble(
      pleito = c("no_limite", "apos_limite"),
      termino_de_vigencia = cenarios$limite[[i]] + c(0, 1)
    )
    resultado <- fim_vigencia_na_data(
      cenarios$hoje[[i]], arquivo, meses = cenarios$meses[[i]]
    )
    testthat::expect_identical(resultado$pleito, "no_limite", info = cenarios$hoje[[i]])
    testthat::expect_identical(resultado$termino_de_vigencia, cenarios$limite[i])
  }
})

testthat::test_that("unit: fim_vigencia preserva colunas, tipos, duplicidades e origem", {
  arquivo <- testthat::test_path("fixtures", "anexos-cache.xlsx")
  anexos <- c("iv", "v", "vi", "viii", "ix", "x")
  lidos <- character()
  dados <- tibble::tibble(
    ncm = c("01012100", "01012100", "01012100"),
    no_ex = c(NA_character_, "001", "001"),
    aliquota = c("2", "4", "4"),
    quota = c("100", NA_character_, NA_character_),
    inicio_de_vigencia = as.Date(rep("2020-01-01", 3)),
    termino_de_vigencia = as.Date(c("2026-10-31", "2026-10-30", "2026-10-30"))
  )
  testthat::local_mocked_bindings(
    ler_anexo = function(x, n_anexo) {
      lidos <<- c(lidos, n_anexo)
      anexo <- dados |>
        dplyr::mutate(lista = paste("Lista", n_anexo))
      if (n_anexo == "v") {
        anexo$obs <- "Observacao preservada"
        anexo$data_do_ato_de_inclusao <- as.Date("2020-01-01")
      }
      anexo
    },
    .package = "tarifasimpbr"
  )

  resultado <- fim_vigencia_na_data("2026-10-26", arquivo)
  testthat::expect_identical(lidos, anexos)
  testthat::expect_s3_class(resultado, "tbl_df")
  testthat::expect_equal(nrow(resultado), 18L)
  testthat::expect_identical(resultado$anexo, c(rep(anexos, each = 2), anexos))
  testthat::expect_identical(
    names(resultado),
    c("anexo", names(dados), "lista", "obs", "data_do_ato_de_inclusao")
  )
  testthat::expect_identical(
    resultado |>
      dplyr::filter(.data$anexo == "iv") |>
      dplyr::select(dplyr::all_of(names(dados))),
    dados[c(2, 3, 1), ]
  )
  testthat::expect_true(all(is.na(resultado$obs[resultado$anexo != "v"])))
  testthat::expect_identical(
    resultado$obs[resultado$anexo == "v"], rep("Observacao preservada", 3)
  )
  testthat::expect_s3_class(resultado$data_do_ato_de_inclusao, "Date")
  testthat::expect_true(all(is.na(
    resultado$data_do_ato_de_inclusao[resultado$anexo != "v"]
  )))

  testthat::expect_silent(vazio <- fim_vigencia_na_data("2030-01-01", arquivo))
  testthat::expect_identical(vazio, resultado[0, ])
})

testthat::test_that("unit: fim_vigencia valida argumentos antes de ler anexos", {
  arquivo <- testthat::test_path("fixtures", "anexos-cache.xlsx")
  testthat::local_mocked_bindings(
    ler_anexo = function(...) stop("A leitura nao deveria ser chamada"),
    .package = "tarifasimpbr"
  )

  testthat::expect_error(fim_vigencia(), "x deve ser o caminho de um arquivo existente")
  caminhos_invalidos <- list(
    NULL, NA_character_, TRUE, character(), c(arquivo, arquivo),
    "", " ", tempfile(), tempdir()
  )
  for (x in caminhos_invalidos) {
    testthat::expect_error(fim_vigencia(x), "x deve ser o caminho de um arquivo existente")
  }
  for (meses in list(-1, 0.5, NA_real_, Inf, NaN, "1", TRUE, NULL, c(1, 2), 1 + 1i)) {
    testthat::expect_error(
      fim_vigencia(arquivo, meses),
      "meses deve ser um numero inteiro, finito e nao negativo"
    )
  }
  for (incluir_hoje in list(NA, 0, "TRUE", logical(), c(TRUE, FALSE), NULL)) {
    testthat::expect_error(
      fim_vigencia(arquivo, incluir_hoje = incluir_hoje),
      "incluir_hoje deve ser TRUE ou FALSE"
    )
  }
  testthat::expect_error(fim_vigencia(arquivo, meses = 1e100), "meses produz um limite de data invalido")
})

testthat::test_that("unit: fim_vigencia rejeita meses excessivos antes de calcular datas", {
  arquivo <- testthat::test_path("fixtures", "anexos-cache.xlsx")
  testthat::local_mocked_bindings(
    period = function(...) stop("O calculo de datas nao deveria ser chamado"),
    .package = "lubridate"
  )

  for (meses in c(
    .Machine$integer.max, .Machine$integer.max + 1, 2^53, 1e100,
    .Machine$double.xmax
  )) {
    testthat::expect_error(
      fim_vigencia(arquivo, meses = meses),
      "meses produz um limite de data invalido"
    )
  }
})

testthat::test_that("unit: fim_vigencia propaga falhas de leitura sem resultado parcial", {
  arquivo <- testthat::test_path("fixtures", "anexos-cache.xlsx")
  testthat::local_mocked_bindings(
    ler_anexo = function(x, n_anexo) {
      if (n_anexo == "vi") stop("Falha ao ler Anexo VI")
      tibble::tibble(termino_de_vigencia = as.Date("2026-10-31"))
    },
    .package = "tarifasimpbr"
  )
  testthat::expect_error(
    fim_vigencia_na_data("2026-10-26", arquivo), "Falha ao ler Anexo VI"
  )
})

testthat::test_that("unit: fim_vigencia consulta o relogio uma unica vez", {
  arquivo <- testthat::test_path("fixtures", "anexos-cache.xlsx")
  chamadas <- 0L
  consulta <- fim_vigencia
  environment(consulta) <- list2env(
    list(Sys.Date = function() {
      chamadas <<- chamadas + 1L
      as.Date("2026-10-26")
    }),
    parent = environment(consulta)
  )
  testthat::local_mocked_bindings(
    ler_anexo = function(x, n_anexo) {
      tibble::tibble(termino_de_vigencia = as.Date("2026-10-31"))
    },
    .package = "tarifasimpbr"
  )
  testthat::expect_equal(nrow(consulta(arquivo)), 6L)
  testthat::expect_equal(chamadas, 1L)
})

testthat::test_that("unit: fim_vigencia le planilha real com cache frio e quente", {
  limpar_cache_tarifas()
  on.exit(limpar_cache_tarifas(), add = TRUE)
  arquivo <- testthat::test_path("fixtures", "anexos-cache.xlsx")
  anexos <- c("iv", "v", "vi", "viii", "ix", "x")
  leituras <- character()
  leitor <- tarifasimpbr:::ler_anexo_sem_cache
  testthat::local_mocked_bindings(
    ler_anexo_sem_cache = function(x, n_anexo) {
      leituras <<- c(leituras, n_anexo)
      leitor(x, n_anexo)
    },
    .package = "tarifasimpbr"
  )

  frio <- fim_vigencia_na_data("2029-12-01", arquivo)
  testthat::expect_identical(frio$anexo, "v")
  testthat::expect_identical(frio$ncm, "01012100")
  testthat::expect_identical(frio$termino_de_vigencia, as.Date("2029-12-31"))
  testthat::expect_identical(fim_vigencia_na_data("2029-12-01", arquivo), frio)
  testthat::expect_identical(leituras, anexos)

  # A mesma leitura em cache deve respeitar uma nova data de consulta.
  testthat::expect_equal(nrow(fim_vigencia_na_data("2030-01-01", arquivo)), 0L)
  testthat::expect_identical(leituras, anexos)
  limpar_cache_tarifas()
  testthat::expect_identical(fim_vigencia_na_data("2029-12-01", arquivo), frio)
  testthat::expect_identical(leituras, rep(anexos, 2))
})

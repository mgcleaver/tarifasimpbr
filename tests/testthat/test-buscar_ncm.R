colunas_buscar_ncm <- c(
  "anexo",
  "nome_anexo",
  "ncm",
  "no_ex",
  "descricao",
  "descricao_tec_concatenada",
  "bkbit",
  "aliquota",
  "quota",
  "unidade_quota",
  "inicio_de_vigencia",
  "termino_de_vigencia",
  "ato",
  "data_do_ato",
  "obs"
)

mock_listar_anexos_buscar_ncm <- function() {
  tibble::tibble(
    numero_anexo = c("i", "ii", "iii", "iv", "v", "vi", "viii", "ix", "x"),
    nome_abreviado_anexo = c(
      "Tarifa Externa Comum",
      "Anexo II",
      "Setor Aeronautico",
      "Desabastecimento",
      "Letec",
      "Lebitbk",
      "Concessoes OMC",
      "DCC",
      "Automotivos ACE-14"
    )
  )
}

testthat::test_that("unit: buscar_ncm retorna anexo i e ii padronizados", {
  testthat::local_mocked_bindings(
    listar_anexos = mock_listar_anexos_buscar_ncm,
    ler_anexo = function(x, n_anexo) {
      switch(
        n_anexo,
        i = tibble::tibble(
          ncm = c("11090000", "22090000"),
          descricao_tec = c("Gluten de trigo", "Vinagre"),
          descricao_tec_concatenada = c("Produtos vegetais. Gluten de trigo.", "Vinagre."),
          tec = c(10, 12),
          bkbit = c(NA_character_, NA_character_),
          resolucoes = c("Res. TEC", "Res. Outro")
        ),
        ii = tibble::tibble(
          ncm = "11090000",
          tec = "10",
          aliquota_aplicada = "8",
          atos_de_inclusao = "Res. TEB"
        ),
        iii = tibble::tibble(ncm = character(), regra = character(), obs = character()),
        iv = tibble::tibble(ncm = character()),
        v = tibble::tibble(ncm = character()),
        vi = tibble::tibble(ncm = character()),
        viii = tibble::tibble(ncm = character()),
        ix = tibble::tibble(ncm = character()),
        x = tibble::tibble(ncm = character())
      )
    },
    .package = "tarifasimpbr"
  )

  resultado <- buscar_ncm(x = "arquivo_mock.xlsx", ncm = "1109.00.00")

  testthat::expect_s3_class(resultado, "tbl_df")
  testthat::expect_identical(names(resultado), colunas_buscar_ncm)
  testthat::expect_equal(resultado$anexo, c("i", "ii"))
  testthat::expect_equal(resultado$nome_anexo, c("Tarifa Externa Comum", "Anexo II"))
  testthat::expect_equal(resultado$ncm, c("11090000", "11090000"))
  testthat::expect_equal(resultado$aliquota, c(10, 8))
  testthat::expect_equal(resultado$ato, c("Res. TEC", "Res. TEB"))
  testthat::expect_equal(resultado$descricao, c("Gluten de trigo", "Gluten de trigo"))
  testthat::expect_equal(
    resultado$descricao_tec_concatenada,
    rep("Produtos vegetais. Gluten de trigo.", 2)
  )
})

testthat::test_that("unit: buscar_ncm preserva multiplas linhas em anexos de excecao", {
  testthat::local_mocked_bindings(
    listar_anexos = mock_listar_anexos_buscar_ncm,
    ler_anexo = function(x, n_anexo) {
      switch(
        n_anexo,
        i = tibble::tibble(
          ncm = "11090000",
          descricao_tec = "Gluten de trigo",
          descricao_tec_concatenada = "Produtos vegetais. Gluten de trigo.",
          tec = 10,
          bkbit = NA_character_,
          resolucoes = "Res. TEC"
        ),
        ii = tibble::tibble(ncm = character()),
        iii = tibble::tibble(ncm = character(), regra = character(), obs = character()),
        iv = tibble::tibble(ncm = character()),
        v = tibble::tibble(
          ncm = c("11090000", "11090000"),
          no_ex = c("001", "002"),
          aliquota = c("0", "2"),
          quota = c(NA_character_, "100"),
          unidade_quota = c(NA_character_, "kg"),
          inicio_de_vigencia = as.Date(c("2025-01-01", "2025-06-01")),
          termino_de_vigencia = as.Date(c("2025-12-31", "2026-12-31")),
          ato_de_inclusao = c("Res. 1", "Res. 2"),
          data_do_ato_de_inclusao = as.Date(c("2024-12-01", "2025-05-01")),
          obs = c(NA_character_, "Observacao"),
          lista = "Letec"
        ),
        vi = tibble::tibble(ncm = character()),
        viii = tibble::tibble(ncm = character()),
        ix = tibble::tibble(ncm = character()),
        x = tibble::tibble(ncm = character())
      )
    },
    .package = "tarifasimpbr"
  )

  resultado <- buscar_ncm(x = "arquivo_mock.xlsx", ncm = "11090000")
  resultado_letec <- resultado |>
    dplyr::filter(.data$anexo == "v")

  testthat::expect_equal(nrow(resultado_letec), 2)
  testthat::expect_equal(resultado_letec$no_ex, c("001", "002"))
  testthat::expect_equal(resultado_letec$aliquota, c(0, 2))
  testthat::expect_equal(resultado_letec$descricao, rep("Gluten de trigo", 2))
  testthat::expect_equal(
    resultado_letec$descricao_tec_concatenada,
    rep("Produtos vegetais. Gluten de trigo.", 2)
  )
  testthat::expect_equal(resultado_letec$ato, c("Res. 1", "Res. 2"))
  testthat::expect_equal(resultado_letec$data_do_ato, as.Date(c("2024-12-01", "2025-05-01")))
})

testthat::test_that("unit: buscar_ncm usa descricao do anexo i nos anexos de excecao", {
  testthat::local_mocked_bindings(
    listar_anexos = mock_listar_anexos_buscar_ncm,
    ler_anexo = function(x, n_anexo) {
      switch(
        n_anexo,
        i = tibble::tibble(
          ncm = "40111000",
          descricao_tec = "Descricao do Anexo I",
          descricao_tec_concatenada = "Descricao concatenada do Anexo I.",
          tec = 16,
          bkbit = NA_character_,
          resolucoes = "Res. TEC"
        ),
        ii = tibble::tibble(ncm = character()),
        iii = tibble::tibble(ncm = character(), regra = character(), obs = character()),
        iv = tibble::tibble(ncm = character()),
        v = tibble::tibble(ncm = character()),
        vi = tibble::tibble(ncm = character()),
        viii = tibble::tibble(ncm = character()),
        ix = tibble::tibble(
          ncm = "40111000",
          descricao = "Descricao propria do Anexo IX",
          no_ex = NA_character_,
          aliquota = "0",
          quota = NA_character_,
          unidade_quota = NA_character_,
          inicio_de_vigencia = as.Date("2025-01-01"),
          termino_de_vigencia = as.Date("2025-12-31"),
          ato_de_inclusao = "Res. DCC",
          data_do_ato_de_inclusao = as.Date("2024-12-01"),
          obs = NA_character_
        ),
        x = tibble::tibble(ncm = character())
      )
    },
    .package = "tarifasimpbr"
  )

  resultado <- buscar_ncm(x = "arquivo_mock.xlsx", ncm = "40111000")
  resultado_ix <- resultado |>
    dplyr::filter(.data$anexo == "ix")

  testthat::expect_equal(resultado_ix$descricao, "Descricao do Anexo I")
  testthat::expect_equal(
    resultado_ix$descricao_tec_concatenada,
    "Descricao concatenada do Anexo I."
  )
})

testthat::test_that("unit: buscar_ncm inclui codigos raiz do anexo iii", {
  testthat::local_mocked_bindings(
    listar_anexos = mock_listar_anexos_buscar_ncm,
    ler_anexo = function(x, n_anexo) {
      switch(
        n_anexo,
        i = tibble::tibble(
          ncm = "40111000",
          descricao_tec = "Pneus novos",
          descricao_tec_concatenada = "Borracha. Pneus novos.",
          tec = 16,
          bkbit = NA_character_,
          resolucoes = "Res. TEC"
        ),
        ii = tibble::tibble(ncm = character()),
        iii = tibble::tibble(
          ncm = c("4011", "401110", "401120", "851714"),
          regra = c("A", "B", "C", "D"),
          obs = c(NA_character_, "Obs", NA_character_, NA_character_)
        ),
        iv = tibble::tibble(ncm = character()),
        v = tibble::tibble(ncm = character()),
        vi = tibble::tibble(ncm = character()),
        viii = tibble::tibble(ncm = character()),
        ix = tibble::tibble(ncm = character()),
        x = tibble::tibble(ncm = character())
      )
    },
    .package = "tarifasimpbr"
  )

  resultado <- buscar_ncm(x = "arquivo_mock.xlsx", ncm = "40111000")
  resultado_anexo_iii <- resultado |>
    dplyr::filter(.data$anexo == "iii")

  testthat::expect_equal(resultado_anexo_iii$ncm, c("4011", "401110"))
  testthat::expect_equal(resultado_anexo_iii$nome_anexo, rep("Setor Aeronautico", 2))
  testthat::expect_true(all(is.na(resultado_anexo_iii$descricao)))
  testthat::expect_true(all(is.na(resultado_anexo_iii$descricao_tec_concatenada)))
  testthat::expect_true(all(is.na(resultado_anexo_iii$aliquota)))
})

testthat::test_that("unit: buscar_ncm retorna vazio com mensagem quando ncm nao existe", {
  testthat::local_mocked_bindings(
    listar_anexos = mock_listar_anexos_buscar_ncm,
    ler_anexo = function(x, n_anexo) {
      tibble::tibble(ncm = character())
    },
    .package = "tarifasimpbr"
  )

  testthat::expect_message(
    resultado <- buscar_ncm(x = "arquivo_mock.xlsx", ncm = "99990000"),
    "NCM não encontrada na busca"
  )

  testthat::expect_s3_class(resultado, "tbl_df")
  testthat::expect_identical(names(resultado), colunas_buscar_ncm)
  testthat::expect_equal(nrow(resultado), 0)
})

testthat::test_that("unit: buscar_ncm falha para ncm invalida", {
  testthat::expect_error(
    buscar_ncm(x = "arquivo_mock.xlsx", ncm = "1109AB00"),
    "ncm deve conter exatamente 8 digitos"
  )
})

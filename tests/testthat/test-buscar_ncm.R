colunas_buscar_ncm <- c(
  "anexo",
  "nome_anexo",
  "ncm",
  "no_ex",
  "descricao_ex",
  "descricao_ncm",
  "descricao_tec_concatenada",
  "bkbit",
  "aliquota",
  "quota",
  "unidade_quota",
  "inicio_de_vigencia",
  "termino_de_vigencia",
  "ato",
  "data_mais_recente_dos_atos",
  "datas_dos_atos_texto",
  "obs"
)

mock_listar_anexos_buscar_ncm <- function() {
  tibble::tibble(
    anexo = c("i", "ii", "iii", "iv", "v", "vi", "viii", "ix", "x"),
    nome_anexo = c(
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
    anexos_excecao_disponiveis = function(x) c("iv", "v", "vi", "viii", "ix", "x"),
    listar_anexos = mock_listar_anexos_buscar_ncm,
    ler_anexo = function(x, n_anexo) {
      switch(
        n_anexo,
        i = tibble::tibble(
          ncm = c("11090000", "22090000"),
          descricao_ncm = c("Gluten de trigo", "Vinagre"),
          descricao_tec_concatenada = c("Produtos vegetais. Gluten de trigo.", "Vinagre."),
          tec = c(10, 12),
          bkbit = c(NA_character_, NA_character_),
          resolucoes = c("Res. TEC", "Res. Outro")
        ),
        ii = tibble::tibble(
          ncm = "11090000",
          tec = "10",
          aliquota = "8",
          ato_de_inclusao = "Res. TEB"
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
  testthat::expect_identical(resultado$descricao_ex, rep(NA_character_, 2))
  testthat::expect_equal(resultado$nome_anexo, c("Tarifa Externa Comum", "Anexo II"))
  testthat::expect_equal(resultado$ncm, c("11090000", "11090000"))
  testthat::expect_equal(resultado$aliquota, c(10, 8))
  testthat::expect_equal(resultado$ato, c("Res. TEC", "Res. TEB"))
  testthat::expect_equal(resultado$descricao_ncm, c("Gluten de trigo", "Gluten de trigo"))
  testthat::expect_equal(
    resultado$descricao_tec_concatenada,
    rep("Produtos vegetais. Gluten de trigo.", 2)
  )
})

testthat::test_that("unit: buscar_ncm preserva multiplas linhas em anexos de excecao", {
  testthat::local_mocked_bindings(
    anexos_excecao_disponiveis = function(x) c("iv", "v", "vi", "viii", "ix", "x"),
    listar_anexos = mock_listar_anexos_buscar_ncm,
    ler_anexo = function(x, n_anexo) {
      switch(
        n_anexo,
        i = tibble::tibble(
          ncm = "11090000",
          descricao_ncm = "Gluten de trigo",
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
          data_mais_recente_dos_atos = as.Date(c("2024-12-01", "2025-05-01")),
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
  testthat::expect_equal(resultado_letec$descricao_ncm, rep("Gluten de trigo", 2))
  testthat::expect_equal(
    resultado_letec$descricao_tec_concatenada,
    rep("Produtos vegetais. Gluten de trigo.", 2)
  )
  testthat::expect_equal(resultado_letec$ato, c("Res. 1", "Res. 2"))
  testthat::expect_equal(resultado_letec$data_mais_recente_dos_atos, as.Date(c("2024-12-01", "2025-05-01")))
})

testthat::test_that("unit: buscar_ncm usa descricao do anexo i nos anexos de excecao", {
  testthat::local_mocked_bindings(
    anexos_excecao_disponiveis = function(x) c("iv", "v", "vi", "viii", "ix", "x"),
    listar_anexos = mock_listar_anexos_buscar_ncm,
    ler_anexo = function(x, n_anexo) {
      switch(
        n_anexo,
        i = tibble::tibble(
          ncm = "40111000",
          descricao_ncm = "Descricao do Anexo I",
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
          data_mais_recente_dos_atos = as.Date("2024-12-01"),
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

  testthat::expect_equal(resultado_ix$descricao_ncm, "Descricao do Anexo I")
  testthat::expect_identical(resultado_ix$descricao_ex, NA_character_)
  testthat::expect_equal(
    resultado_ix$descricao_tec_concatenada,
    "Descricao concatenada do Anexo I."
  )
})

testthat::test_that("unit: buscar_ncm inclui codigos raiz do anexo iii", {
  testthat::local_mocked_bindings(
    anexos_excecao_disponiveis = function(x) c("iv", "v", "vi", "viii", "ix", "x"),
    listar_anexos = mock_listar_anexos_buscar_ncm,
    ler_anexo = function(x, n_anexo) {
      switch(
        n_anexo,
        i = tibble::tibble(
          ncm = "40111000",
          descricao_ncm = "Pneus novos",
          descricao_tec_concatenada = "Borracha. Pneus novos.",
          tec = 16,
          bkbit = NA_character_,
          resolucoes = "Res. TEC"
        ),
        ii = tibble::tibble(ncm = character()),
        iii = tibble::tibble(
          ncm = c("4011", "401110", "401110", "401120", "851714"),
          regra = c("A", "B", "B2", "C", "D"),
          obs = c(
            NA_character_,
            "Obs",
            "Outra observacao",
            NA_character_,
            NA_character_
          )
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
  testthat::expect_equal(nrow(resultado_anexo_iii), 2L)
  testthat::expect_equal(resultado_anexo_iii$nome_anexo, rep("Setor Aeronautico", 2))
  testthat::expect_true(all(is.na(resultado_anexo_iii$descricao_ncm)))
  testthat::expect_identical(resultado_anexo_iii$descricao_ex, rep(NA_character_, 2))
  testthat::expect_true(all(is.na(resultado_anexo_iii$descricao_tec_concatenada)))
  testthat::expect_true(all(is.na(resultado_anexo_iii$aliquota)))
})

testthat::test_that("integration: buscar_ncm deduplica raiz 851714 do anexo iii", {
  arquivo <- get_tarifas_path()

  for (codigo in c("85171431", "85171432")) {
    resultado_anexo_iii <- buscar_ncm(arquivo, codigo) |>
      dplyr::filter(.data$anexo == "iii")

    testthat::expect_equal(
      nrow(resultado_anexo_iii),
      1L,
      info = paste("codigo pesquisado:", codigo)
    )
    testthat::expect_equal(
      resultado_anexo_iii$ncm,
      "851714",
      info = paste("codigo pesquisado:", codigo)
    )
  }
})

testthat::test_that("unit: buscar_ncm retorna vazio com warning quando ncm nao existe", {
  testthat::local_mocked_bindings(
    anexos_excecao_disponiveis = function(x) c("iv", "v", "vi", "viii", "ix", "x"),
    listar_anexos = mock_listar_anexos_buscar_ncm,
    ler_anexo = function(x, n_anexo) {
      tibble::tibble(ncm = character())
    },
    .package = "tarifasimpbr"
  )

  testthat::expect_warning(
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

testthat::test_that("integration: buscas preservam treonina apenas no Ex e mantem descricao NCM", {
  x <- get_tarifas_path()
  anexo <- if ("xi" %in% tarifasimpbr:::anexos_excecao_disponiveis(x)) "xi" else "v"
  fonte <- ler_anexo(x, anexo) |>
    dplyr::filter(.data$ncm == "29225099")
  testthat::skip_if(!any(fonte$no_ex == "002", na.rm = TRUE), "Ex 002 ausente nesta publicacao")
  por_ncm <- buscar_ncm(x, "2922.50.99") |>
    dplyr::filter(.data$anexo == .env$anexo)
  por_ato <- buscar_ato(x, 705) |>
    dplyr::filter(.data$anexo == .env$anexo, .data$ncm == "29225099")

  for (resultado in list(por_ncm, por_ato)) {
    testthat::expect_identical(resultado$descricao_ex, ifelse(is.na(fonte$no_ex), NA_character_, fonte$descricao))
    testthat::expect_identical(resultado$descricao_ncm, rep("Outros", nrow(fonte)))
    testthat::expect_identical(match("descricao_ex", names(resultado)) + 1L, match("descricao_ncm", names(resultado)))
    ex <- resultado |> dplyr::filter(.data$no_ex == "002")
    integral <- resultado |> dplyr::filter(is.na(.data$no_ex))
    testthat::expect_identical(ex$descricao_ex, "Outros, exceto treonina")
    testthat::expect_equal(ex$aliquota, 0)
    testthat::expect_identical(integral$descricao_ex, NA_character_)
    testthat::expect_equal(integral$aliquota, 10.8)
    testthat::expect_true(all(resultado$termino_de_vigencia == as.Date("9999-12-31")))
  }
})

testthat::test_that("unit: buscas preservam descricao do Ex mesmo sem NCM correspondente no I", {
  testthat::local_mocked_bindings(
    anexos_excecao_disponiveis = function(x) "xi",
    ler_anexo = function(x, n_anexo) {
      if (n_anexo == "xi") {
        return(tibble::tibble(
          ncm = "29225099", no_ex = "002",
          descricao = "Outros, exceto treonina", ato_de_inclusao = "Res. 705/2025"
        ))
      }
      tibble::tibble(ncm = character())
    },
    .package = "tarifasimpbr"
  )
  for (resultado in list(buscar_ncm("mock", "29225099"), buscar_ato("mock", 705))) {
    testthat::expect_identical(resultado$descricao_ex, "Outros, exceto treonina")
    testthat::expect_identical(resultado$descricao_ncm, NA_character_)
    testthat::expect_identical(resultado$descricao_tec_concatenada, NA_character_)
  }
})

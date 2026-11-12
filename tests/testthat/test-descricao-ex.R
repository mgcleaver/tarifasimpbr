testthat::test_that("unit: descricao_ex usa somente a lista nas linhas com Ex", {
  dados <- tibble::tibble(
    ncm = "29225099",
    no_ex = c(NA, "", " ", "-", " - ", "\u00a0", "001", "002", "003", "004", "0", "000", "Ex 005"),
    descricao = c(rep("Descricao sem Ex", 6), "Restricao 1", NA, "Restricao com quota", "Restricao 4", "Zero informado", "Zeros informados", "Restricao 5"),
    aliquota = "0", lista = "Letec+", quota = NA_character_, unidade_quota = "kg",
    inicio_de_vigencia = as.Date("2020-01-01"), termino_de_vigencia = as.Date("2099-12-31"),
    ato_de_inclusao = "Teste 978", obs = sprintf("linha-%02d", seq_len(13))
  )
  dados$quota[c(1, 9)] <- "100"
  testthat::local_mocked_bindings(
    anexos_excecao_disponiveis = function(x) "xi",
    ler_anexo = function(x, n_anexo) switch(n_anexo,
      i = tibble::tibble(ncm = "29225099", descricao_ncm = "NCM do Anexo I",
                        descricao_tec_concatenada = "Hierarquia do Anexo I.", tec = 10,
                        bkbit = NA_character_, resolucoes = "Teste 978"),
      ii = tibble::tibble(ncm = "29225099", descricao_ncm = "NCM propria do II",
                         aliquota = "8", ato_de_inclusao = "Teste 978"),
      iii = tibble::tibble(ncm = character(), regra = character(), obs = character()),
      xi = dados
    ),
    .package = "tarifasimpbr"
  )
  esperado <- c(rep(NA_character_, 6), dados$descricao[7:13])
  for (busca in list(buscar_ncm("mock", "29225099"), buscar_ato("mock", "Teste 978"))) {
    testthat::expect_identical(busca$descricao_ex[busca$anexo == "xi"], esperado)
    testthat::expect_true(all(is.na(busca$descricao_ex[busca$anexo %in% c("i", "ii")])))
    testthat::expect_identical(busca$descricao_ncm, rep("NCM do Anexo I", nrow(busca)))
    testthat::expect_identical(busca$descricao_tec_concatenada, rep("Hierarquia do Anexo I.", nrow(busca)))
    testthat::expect_identical(match("descricao_ex", names(busca)) + 1L, match("descricao_ncm", names(busca)))
    testthat::expect_false("descricao_medida" %in% names(busca))
  }
  ex <- ex_tarifarios("mock")
  testthat::expect_equal(nrow(ex), 7L)
  testthat::expect_identical(ex$descricao_ex, dados$descricao[match(ex$obs, dados$obs)])
  testthat::expect_identical(ex$descricao_ex[ex$no_ex == "002"], NA_character_)
  testthat::expect_identical(match("descricao_ex", names(ex)), 3L)

  # Ausencia da coluna inteira tambem nao autoriza usar a descricao do I.
  dados$descricao <- NULL
  for (saida in list(buscar_ncm("mock", "29225099"), buscar_ato("mock", "Teste 978"), ex_tarifarios("mock"))) {
    testthat::expect_true(all(is.na(saida$descricao_ex)))
    testthat::expect_type(saida$descricao_ex, "character")
  }

  # Sem Ex, preserva o esquema e o tipo da coluna na saida vazia de Ex.
  dados <- dados[1:6, ]
  vazio <- ex_tarifarios("mock")
  testthat::expect_identical(names(vazio), names(ex))
  testthat::expect_identical(vazio$descricao_ex, character())
  testthat::expect_equal(nrow(vazio), 0L)
  for (saida in list(suppressWarnings(buscar_ncm("mock", "99999999")), suppressWarnings(buscar_ato("mock", "Ato inexistente")))) {
    testthat::expect_identical(saida$descricao_ex, character())
    testthat::expect_identical(match("descricao_ex", names(saida)) + 1L, match("descricao_ncm", names(saida)))
  }
})

testthat::test_that("integration: II preserva descricao NCM da fonte e sua posicao", {
  x <- get_tarifas_path()
  aba <- tarifasimpbr:::localizar_aba_anexo(readxl::excel_sheets(x), "ii")
  fonte <- readxl::read_excel(x, sheet = aba,
    skip = tarifasimpbr:::obter_linha_cabecalho(x, aba), col_types = "text"
  ) |> janitor::clean_names()
  resultado <- ler_anexo(x, "ii")
  testthat::expect_identical(resultado$descricao_ncm, fonte$descricao)
  testthat::expect_identical(match("descricao_ncm", names(resultado)), match("descricao", names(fonte)))
  testthat::expect_false(any(c("descricao", "descricao_medida", "descricao_ex") %in% names(resultado)))
})

testthat::test_that("integration: quota sem Ex nao recebe descricao_ex e preserva descricao na lista", {
  x <- get_tarifas_path()
  lista <- ler_anexo(x, "v") |> dplyr::filter(.data$ncm == "03035300")
  testthat::skip_if(nrow(lista) == 0L, "NCM de exemplo ausente desta publicacao")
  testthat::expect_true(all(is.na(lista$no_ex)))
  testthat::expect_true(any(!is.na(lista$quota)))
  testthat::expect_true(any(!is.na(lista$descricao)))
  busca <- buscar_ncm(x, "03035300") |> dplyr::filter(.data$anexo == "v")
  testthat::expect_identical(busca$descricao_ex, rep(NA_character_, nrow(lista)))
  testthat::expect_true("descricao" %in% names(fim_vigencia(x)))
  testthat::expect_false(any(c("descricao_medida", "descricao_ex") %in% names(fim_vigencia(x))))
})

# Injeta o relogio apenas em copias locais das funcoes que consultam a data.
local_data_xi <- function(relogio, .env = parent.frame()) {
  funcoes <- list(
    tarifas_vigentes = tarifas_vigentes,
    tarifas_futuras = tarifas_futuras,
    fim_vigencia = fim_vigencia
  )
  funcoes <- lapply(funcoes, function(f) {
    environment(f) <- list2env(list(Sys.Date = relogio), parent = environment(f))
    f
  })
  do.call(testthat::local_mocked_bindings, c(funcoes, list(.package = "tarifasimpbr", .env = .env)))
}

testthat::test_that("unit: XI usa o leitor comum e preserva as condicoes da medida", {
  x <- testthat::test_path("fixtures", "anexos-xi.xlsx")
  xi <- ler_anexo(x, "xi")
  expect_colunas_anexo_lista(xi)
  expect_ncm_oito_digitos(xi)
  expect_lista_correta(xi, "xi")
  testthat::expect_equal(nrow(xi), 6L)
  testthat::expect_equal(xi$no_ex, c(NA, NA, "002", NA, "003", "004"))
  testthat::expect_equal(xi$descricao[[3]], "Produto B, exceto tipo especial")
  testthat::expect_equal(xi$termino_de_vigencia[[3]], as.Date("9999-12-31"))
  testthat::expect_equal(xi$setor_do_produto_divisao_isic, rep("01", 6))
  testthat::expect_equal(xi$inicio_de_vigencia[[1]], as.Date("2026-11-10"))
  testthat::expect_silent(testthat::expect_identical(ler_anexo(x, "xi"), xi))
  testthat::expect_false("vii" %in% listar_anexos()$numero_anexo)
})

testthat::test_that("unit: XI e opcional somente nas consultas consolidadas", {
  antigo <- testthat::test_path("fixtures", "anexos-cache.xlsx")
  novo <- testthat::test_path("fixtures", "anexos-xi.xlsx")
  anteriores <- c("iv", "v", "vi", "viii", "ix", "x")
  testthat::expect_identical(tarifasimpbr:::anexos_excecao_disponiveis(antigo), anteriores)
  testthat::expect_identical(tarifasimpbr:::anexos_excecao_disponiveis(novo), c(anteriores, "xi"))
  testthat::expect_error(ler_anexo(antigo, "xi"), "Aba do Anexo XI.*encontrada")
  testthat::expect_equal(ler_anexo(novo, "x")$lista, "Automotivos ACE-14")
})

testthat::test_that("unit: reconhecimento do XI respeita numeral e rejeita duplicidade", {
  abas <- c("Anexo X", "Anexo XII", "Copia Anexo XI")
  testthat::local_mocked_bindings(excel_sheets = function(path) abas, .package = "readxl")
  testthat::expect_false("xi" %in% tarifasimpbr:::anexos_excecao_disponiveis("mock"))
  abas <- c(abas, "  ANEXO   XI-LETEC +  ")
  testthat::expect_true("xi" %in% tarifasimpbr:::anexos_excecao_disponiveis("mock"))
  testthat::expect_equal(tarifasimpbr:::localizar_aba_anexo(abas, "xi"), 4L)
  abas <- c(abas, "Anexo XI - duplicado")
  testthat::expect_error(
    tarifasimpbr:::ler_anexo_sem_cache("mock", "xi"),
    "Mais de uma aba encontrada para o Anexo XI"
  )
})

testthat::test_that("unit: XI altera somente tarifa integral sem quota", {
  x <- testthat::test_path("fixtures", "anexos-xi.xlsx")
  local_data_xi(function() as.Date("2026-11-12"))
  resultado <- tarifas_aplicadas(x, detalhar = TRUE)
  linhas <- resultado[match(c("01012900", "01013000", "01014000", "01015000"), resultado$ncm), ]
  testthat::expect_equal(linhas$tarifa_aplicada, c(20, 12, 12, 12))
  testthat::expect_equal(linhas$contagem_quota, c(0L, 1L, 0L, 1L))
  testthat::expect_equal(linhas$contagem_ex, c(6L, 0L, 1L, 1L))
  testthat::expect_equal(linhas$ncm_integral, c(1L, 1L, 0L, 0L))
  testthat::expect_equal(linhas$ato_de_inclusao, c("Res. 978/2026", NA, NA, NA))
  testthat::expect_equal(linhas$lista[[1]], "Desabastecimento, Lebitbk, Concessoes OMC, DCC, Automotivos ACE-14, Letec+")
  testthat::expect_equal(linhas$lista[-1], rep("Letec+", 3))
})

testthat::test_that("unit: consultas por NCM, ato, Ex e vencimento incluem XI", {
  x <- testthat::test_path("fixtures", "anexos-xi.xlsx")
  local_data_xi(function() as.Date("2026-11-12"))
  ncm <- buscar_ncm(x, "0101.29.00")
  testthat::expect_equal(ncm$aliquota[ncm$anexo == "xi"], c(20, 22, 0))
  ato <- buscar_ato(x, 978)
  testthat::expect_equal(nrow(ato), 6L)
  testthat::expect_true(all(ato$anexo == "xi"))
  testthat::expect_equal(buscar_ato(x, 705)$no_ex, "002")
  ex <- ex_tarifarios(x)
  testthat::expect_equal(ex$no_ex[ex$lista == "Letec+"], c("002", "003", "004"))
  testthat::expect_true("Produto B, exceto tipo especial" %in% ex$descricao)
  vencimentos <- fim_vigencia(x, meses = 1)
  testthat::expect_equal(vencimentos$anexo, "xi")
  testthat::expect_equal(vencimentos$termino_de_vigencia, as.Date("2026-12-04"))
})

testthat::test_that("unit: vigencias do XI sao inclusivas e reavaliadas com cache quente", {
  x <- testthat::test_path("fixtures", "anexos-xi.xlsx")
  data_consulta <- as.Date("2026-11-09")
  local_data_xi(function() data_consulta)
  xi <- ler_anexo(x, "xi")
  cenarios <- c("2026-11-09", "2026-11-10", "2026-12-04", "2026-12-05", "2027-12-04", "2027-12-05")
  esperadas <- c(10, 20, 20, 22, 22, 10)
  for (i in seq_along(cenarios)) {
    data_consulta <- as.Date(cenarios[[i]])
    resultado <- tarifas_aplicadas(x)
    testthat::expect_equal(resultado$tarifa_aplicada[resultado$ncm == "01012900"], esperadas[[i]], info = cenarios[[i]])
    testthat::expect_identical(ler_anexo(x, "xi"), xi)
  }
  data_consulta <- as.Date("2026-12-04")
  testthat::expect_equal(tarifas_futuras(xi)$aliquota, "22")
  testthat::expect_equal(nrow(fim_vigencia(x, incluir_hoje = FALSE)), 0L)
  testthat::expect_equal(nrow(fim_vigencia(x, incluir_hoje = TRUE)), 1L)
})

testthat::test_that("unit: conflito integral entre XI e outra lista continua sendo erro", {
  x <- testthat::test_path("fixtures", "anexos-xi.xlsx")
  leitor <- tarifasimpbr:::ler_anexo
  local_data_xi(function() as.Date("2026-11-12"))
  testthat::local_mocked_bindings(
    ler_anexo = function(x, n_anexo) {
      dados <- leitor(x, n_anexo)
      if (n_anexo == "v") dados$ncm <- "01012900"
      dados
    },
    .package = "tarifasimpbr"
  )
  testthat::expect_error(tarifas_aplicadas(x), "mais de um registro integral vigente.*01012900")
})

testthat::test_that("integration: XI oficial segue a estrutura das listas de excecao", {
  x <- get_tarifas_path()
  testthat::skip_if(!"xi" %in% tarifasimpbr:::anexos_excecao_disponiveis(x), "Planilha anterior ao XI")
  xi <- ler_anexo(x, "xi")
  expect_ncm_oito_digitos(xi)
  expect_colunas_anexo_lista(xi)
  expect_lista_correta(xi, "xi")
  testthat::expect_true(all(xi$inicio_de_vigencia <= xi$termino_de_vigencia))
})

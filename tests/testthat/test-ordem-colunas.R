testthat::test_that("unit: leitura individual mantem lista por ultimo com ou sem datas na fonte", {
  anexos <- c("iv", "v", "vi", "viii", "ix", "x", "xi")
  tem_datas <- TRUE
  dados <- tibble::tibble(
    ncm = "2922.50.99", no_ex = "-", descricao = "Produto",
    aliquota = "10.8", quota = "-", unidade_da_quota = "-",
    inicio_de_vigencia = "01/03/2025", termino_de_vigencia = "-",
    ato_de_inclusao = "Res. 705/2025"
  )
  testthat::local_mocked_bindings(
    excel_sheets = function(...) paste("Anexo", toupper(anexos)),
    read_excel = function(..., trim_ws = TRUE) {
      if (!trim_ws) return(tibble::tibble(datas = "25/02/2025"))
      fonte <- dados
      if (tem_datas) fonte$data_do_ato_de_inclusao <- "25/02/2025"
      fonte$campo_adicional <- "Preservado"
      fonte
    },
    .package = "readxl"
  )
  testthat::local_mocked_bindings(obter_linha_cabecalho = function(...) 0L, .package = "tarifasimpbr")
  anteriores <- c(
    "ncm", "no_ex", "descricao", "aliquota", "quota", "unidade_quota",
    "inicio_de_vigencia", "termino_de_vigencia", "ato_de_inclusao"
  )
  datas <- c("data_mais_recente_dos_atos", "datas_dos_atos_texto")
  for (presente in c(TRUE, FALSE)) {
    tem_datas <- presente
    esperado <- if (presente) c(anteriores, datas, "campo_adicional", "lista") else
      c(anteriores, "campo_adicional", datas, "lista")
    for (a in anexos) {
      resultado <- ler_anexo("arquivo_mock_ordem_inexistente.xlsx", a)
      testthat::expect_identical(names(resultado), esperado, info = a)
      testthat::expect_identical(resultado$campo_adicional, "Preservado")
      testthat::expect_identical(names(tarifas_vigentes(resultado)), esperado)
      testthat::expect_identical(names(tarifas_futuras(resultado)), esperado)
    }
  }
})

testthat::test_that("unit: saidas derivadas preservam ordem da 0.8.0 com renomeacoes deliberadas", {
  # Esquemas da 0.8.0 para as fixtures. As datas foram renomeadas;
  # texto original e descricao_ex sao acrescimos, sem reordenar as demais.
  tec <- c("ncm", "descricao_tec", "descricao_tec_concatenada", "tec", "bkbit", "resolucoes")
  lista <- c(
    "ncm", "no_ex", "descricao", "aliquota", "quota", "unidade_quota",
    "inicio_de_vigencia", "termino_de_vigencia", "ato_de_inclusao",
    "data_do_ato_de_inclusao", "obs", "lista"
  )
  busca <- c(
    "anexo", "nome_anexo", "ncm", "no_ex", "descricao", "descricao_tec_concatenada",
    "bkbit", "aliquota", "quota", "unidade_quota", "inicio_de_vigencia",
    "termino_de_vigencia", "ato", "data_do_ato", "obs"
  )
  simples <- c("ncm", "descricao_tec", "descricao_tec_concatenada", "bkbit", "lista", "tec", "teb", "tarifa_aplicada")
  metadados <- c("inicio_de_vigencia", "termino_de_vigencia", "ato_de_inclusao", "data_do_ato_de_inclusao")
  contagens <- c("contagem_quota", "contagem_ex", "ncm_integral")
  esquemas <- list(
    listar = c("numero_anexo", "nome_abreviado_anexo"),
    tec = tec,
    aeronautico = c("ncm", "regra", "obs"),
    consolidacao = c("ncm", contagens, "lista", "tarifa_aplicada", metadados),
    simples = simples,
    detalhadas = c(simples, metadados, contagens),
    ex = c("lista", "ncm", "descricao", "no_ex", "tarifa_ex", "quota", "unidade_quota", "ato_de_inclusao", "data_do_ato_de_inclusao", "inicio_de_vigencia", "termino_de_vigencia", "obs"),
    vencimentos = c("anexo", lista),
    busca_ncm = busca, busca_ato = busca, busca_vazia = busca
  )
  renomear <- function(nomes, saida) {
    mapa <- c(descricao_tec = "descricao_ncm", numero_anexo = "anexo", nome_abreviado_anexo = "nome_anexo", tarifa_ex = "aliquota")
    for (antigo in names(mapa)) nomes[nomes == antigo] <- mapa[[antigo]]
    nomes[nomes == "descricao"] <- if (startsWith(saida, "busca")) "descricao_ncm" else if (saida == "ex") "descricao_ex" else "descricao"
    nomes[nomes %in% c("data_do_ato", "data_do_ato_de_inclusao")] <- "data_mais_recente_dos_atos"
    nomes
  }
  for (arquivo in c("anexos-cache.xlsx", "anexos-xi.xlsx")) {
    x <- testthat::test_path("fixtures", arquivo)
    resultados <- list(
      listar = listar_anexos(), tec = ler_anexo(x, "i"), aeronautico = ler_anexo(x, "iii"),
      consolidacao = detalhar_listas_excecao_vigentes(x), simples = tarifas_aplicadas(x),
      detalhadas = tarifas_aplicadas(x, detalhar = TRUE), ex = ex_tarifarios(x),
      vencimentos = fim_vigencia(x), busca_ncm = buscar_ncm(x, "01012900"),
      busca_ato = buscar_ato(x, "Res"), busca_vazia = suppressWarnings(buscar_ncm(x, "99999999"))
    )
    for (nome in names(resultados)) {
      atuais <- names(resultados[[nome]])
      anteriores <- renomear(esquemas[[nome]], nome)
      testthat::expect_identical(
        atuais[atuais %in% anteriores], anteriores, info = paste(arquivo, nome)
      )
      if ("datas_dos_atos_texto" %in% atuais) {
        testthat::expect_identical(match("datas_dos_atos_texto", atuais), match("data_mais_recente_dos_atos", atuais) + 1L)
      }
      if (startsWith(nome, "busca")) testthat::expect_identical(match("descricao_ex", atuais) + 1L, match("descricao_ncm", atuais))
    }
  }
})

testthat::test_that("unit: vencimentos mantem ordem propria quando IV nao tem datas de atos", {
  x <- testthat::test_path("fixtures", "anexos-cache.xlsx")
  limpar_cache_tarifas()
  on.exit(limpar_cache_tarifas(), add = TRUE)
  dados <- tibble::tibble(
    ncm = "29225099", no_ex = NA_character_, aliquota = "10.8",
    inicio_de_vigencia = as.Date("2020-01-01"), termino_de_vigencia = as.Date("2030-01-01"),
    lista = "Letec", data_mais_recente_dos_atos = as.Date(NA_character_), datas_dos_atos_texto = NA_character_
  )
  testthat::local_mocked_bindings(ler_anexo_sem_cache = function(...) dados, .package = "tarifasimpbr")
  individual <- ler_anexo(x, "iv")
  testthat::expect_identical(tail(names(individual), 1), "lista")
  # A uniao historica acrescentava a data depois de lista. Mesmo com cache
  # aquecido pela leitura individual e sem vencimentos, essa ordem permanece.
  testthat::expect_identical(names(fim_vigencia(x)), c("anexo", names(dados)))
  testthat::expect_identical(ler_anexo(x, "iv"), individual)
})

testthat::test_that("integration: listas oficiais terminam em lista e filtros preservam a ordem", {
  x <- get_tarifas_path()
  for (a in tarifasimpbr:::anexos_excecao_disponiveis(x)) {
    dados <- ler_anexo(x, a)
    testthat::expect_identical(tail(names(dados), 1), "lista", info = a)
    testthat::expect_identical(names(tarifas_vigentes(dados)), names(dados))
    testthat::expect_identical(names(tarifas_futuras(dados)), names(dados))
  }
})

testthat::test_that("unit: XLSX separa apresentacao, texto misto e validacao bruta", {
  x <- testthat::test_path("fixtures", "datas-visuais.xlsx")
  esperado <- c(
    "06/11/2026", "06/11/2026", "6/11/2026", "2026-11-06",
    "06/11/2026", "06/11/2026", " 06/11/2026 e 25/02/2025 ",
    "2025-02-25; 2026-11-06", "06/11/2026 e 31/02/2026", "46332", "46332",
    NA, "-", "46332", "06/11/2026", "29/02/1900", "28/02/1900", NA
  )
  datas <- as.Date(c(rep("2026-11-06", 8), NA, rep("2026-11-06", 2),
                     NA, NA, "2026-11-06", NA, NA, "1900-02-28", NA))
  for (a in c("v", "xi")) {
    avisos <- character()
    resultado <- withCallingHandlers(
      tarifasimpbr:::ler_anexo_sem_cache(x, a),
      warning = function(w) {
        avisos <<- c(avisos, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )
    testthat::expect_length(avisos, 2L)
    testthat::expect_match(avisos[[1]], "Formato visual.*14.*Texto bruto preservado")
    testthat::expect_match(avisos[[2]], paste0("Datas de atos invalidas em Anexo ", toupper(a), ".*9, 15, 16"))
    testthat::expect_identical(resultado$datas_dos_atos_texto, esperado)
    testthat::expect_identical(resultado$data_mais_recente_dos_atos, datas)
    testthat::expect_identical(resultado$termino_de_vigencia, rep(as.Date("9999-12-31"), 18))
  }
})

testthat::test_that("unit: datas numericas respeitam base 1904 sem reinterpretar texto", {
  x <- testthat::test_path("fixtures", "datas-visuais-1904.xlsx")
  testthat::expect_warning(resultado <- ler_anexo(x, "xi"), NA)
  testthat::expect_identical(resultado$datas_dos_atos_texto, c("06/11/2026", "44870", "46332", "1904-01-01"))
  testthat::expect_identical(resultado$data_mais_recente_dos_atos, as.Date(c(rep("2026-11-06", 3), "1904-01-01")))
})

testthat::test_that("integration: formatos oficiais e campos equivalentes sao propagados", {
  x <- get_tarifas_path()
  catalogo <- listar_anexos()
  testthat::expect_identical(names(catalogo), c("anexo", "nome_anexo"))
  base <- ler_anexo(x, "i")
  ii <- ler_anexo(x, "ii")
  testthat::expect_true(all(c("descricao_medida", "aliquota", "ato_de_inclusao", "bkbit") %in% names(ii)))
  testthat::expect_false(any(c("descricao", "aliquota_aplicada", "atos_de_inclusao", "bit_bk") %in% names(ii)))
  tarifas <- tarifas_aplicadas(x, detalhar = TRUE)
  testthat::expect_identical(tarifas$descricao_ncm, base$descricao_ncm)
  testthat::expect_identical(match("descricao_ncm", names(tarifas)), 2L)
  testthat::expect_identical(tarifasimpbr:::listar_ex_tarifarios(x), ex_tarifarios(x))
  for (a in tarifasimpbr:::anexos_excecao_disponiveis(x)) {
    lido <- ler_anexo(x, a)
    testthat::expect_false(any(c("descricao", "tarifa_ex") %in% names(lido)))
    testthat::expect_identical(names(lido)[[ncol(lido)]], "lista")
  }
  if (!"xi" %in% tarifasimpbr:::anexos_excecao_disponiveis(x)) return(invisible(NULL))
  xi <- ler_anexo(x, "xi")
  # Apenas as datas numericas sao reformadas; o texto misto mantem a ordem original.
  testthat::expect_identical(xi$datas_dos_atos_texto[xi$ncm == "40111000"], "06/11/2026")
  testthat::expect_identical(xi$datas_dos_atos_texto[xi$ncm == "29225099"], rep("25/02/2025 e 06/11/2026", 2))
  for (busca in list(buscar_ncm(x, "29225099"), buscar_ato(x, 705))) {
    testthat::expect_identical(match("descricao_medida", names(busca)) + 1L, match("descricao_ncm", names(busca)))
    trecho <- busca[busca$anexo == "xi" & !is.na(busca$no_ex) & busca$no_ex == "002", ]
    testthat::expect_identical(trecho$descricao_medida, "Outros, exceto treonina")
    testthat::expect_identical(trecho$descricao_ncm, "Outros")
  }
})

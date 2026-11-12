testthat::test_that("unit: datas de atos usam o maximo cronologico em formatos completos", {
  entrada <- c(
    "25/02/2025 e 06/11/2026", "06/11/2026 e 25/02/2025",
    "30/12/2025; 01/01/2026", "2026-11-06; 2025-02-25",
    "25/02/2025, 2026-11-06", "25/02/2025 | 06/11/2026",
    "25/02/2025\n06/11/2026", "25/02/2025\r\n06/11/2026",
    "25/02/2025 E 06/11/2026", " 25/02/2025 e 06/11/2026 ",
    "06/11/2026 e 06/11/2026", "46332", "46332 e 25/02/2025",
    "1/2/2026", "29/02/2024", "2024-02-29"
  )
  esperado <- as.Date(c(
    "2026-11-06", "2026-11-06", "2026-01-01", rep("2026-11-06", 10),
    "2026-02-01", "2024-02-29", "2024-02-29"
  ))
  testthat::expect_silent(resultado <- tarifasimpbr:::formata_datas_atos(entrada))
  testthat::expect_identical(resultado, esperado)
  testthat::expect_identical(tarifasimpbr:::formata_datas_atos(46332), as.Date("2026-11-06"))
  testthat::expect_identical(tarifasimpbr:::formata_datas_atos(as.Date("2026-11-06")), as.Date("2026-11-06"))
})

testthat::test_that("unit: ausencias em datas de atos nao recebem sentinela nem aviso", {
  testthat::expect_silent(resultado <- tarifasimpbr:::formata_datas_atos(c(NA, "", "  ", "-", " - ")))
  testthat::expect_identical(resultado, rep(as.Date(NA_character_), 5))
  testthat::expect_identical(tarifasimpbr:::formata_datas_atos(character()), as.Date(character()))
})

testthat::test_that("unit: nenhuma data valida encobre uma parte invalida", {
  entradas <- c(
    "31/02/2026", "29/02/2025", "2026-02-31", "00/12/2026", "06/13/2026",
    "25/02/2025 e 31/02/2026", "31/02/2026 e 06/11/2026",
    "25/02/2025 e a definir", "2026-11-06 lixo", "25/02/2025 e",
    "e 25/02/2025", "25/02/2025 e -", "46332.5", "-10", "Inf", "NaN",
    "99999999999999999999", "0000-01-01", "2026-11-06 2025-02-25"
  )
  for (entrada in entradas) {
    testthat::expect_warning(
      resultado <- tarifasimpbr:::formata_datas_atos(entrada, contexto = "Anexo XI"),
      "Datas de atos invalidas em Anexo XI nas linhas de dados: 1"
    )
    testthat::expect_identical(resultado, as.Date(NA_character_), info = entrada)
  }
  testthat::expect_warning(
    misto <- tarifasimpbr:::formata_datas_atos(c("06/11/2026", "31/02/2026", "-")),
    "linhas de dados: 2"
  )
  testthat::expect_identical(misto, as.Date(c("2026-11-06", NA, NA)))
})

testthat::test_that("unit: todas as listas preservam texto e compartilham o esquema de datas", {
  texto <- " 06/11/2026 e 25/02/2025 "
  tem_coluna <- TRUE
  anexos <- c("iv", "v", "vi", "viii", "ix", "x", "xi")
  testthat::local_mocked_bindings(
    excel_sheets = function(...) paste("Anexo", toupper(anexos)),
    read_excel = function(..., trim_ws = TRUE) {
      if (!trim_ws) return(tibble::tibble(datas = texto))
      dados <- tibble::tibble(
        ncm = "2922.50.99", no_ex = "-", aliquota = "10.8",
        inicio_de_vigencia = "01/03/2025", termino_de_vigencia = "-"
      )
      if (tem_coluna) dados$data_do_ato_de_inclusao <- trimws(texto)
      dados
    },
    .package = "readxl"
  )
  testthat::local_mocked_bindings(obter_linha_cabecalho = function(...) 0L, .package = "tarifasimpbr")
  for (anexo in anexos) {
    resultado <- tarifasimpbr:::ler_anexo_sem_cache("mock", anexo)
    testthat::expect_identical(resultado$datas_dos_atos_texto, texto)
    testthat::expect_identical(resultado$data_mais_recente_dos_atos, as.Date("2026-11-06"))
    testthat::expect_false("data_do_ato_de_inclusao" %in% names(resultado))
    testthat::expect_identical(resultado$termino_de_vigencia, as.Date("9999-12-31"))
  }
  tem_coluna <- FALSE
  for (anexo in anexos) {
    resultado <- tarifasimpbr:::ler_anexo_sem_cache("mock", anexo)
    testthat::expect_identical(resultado$datas_dos_atos_texto, NA_character_)
    testthat::expect_identical(resultado$data_mais_recente_dos_atos, as.Date(NA_character_))
  }
  tem_coluna <- TRUE
  texto <- " 06/11/2026 e 31/02/2026 "
  testthat::expect_warning(resultado <- tarifasimpbr:::ler_anexo_sem_cache("mock", "xi"), "Datas de atos invalidas")
  testthat::expect_identical(resultado$datas_dos_atos_texto, texto)
  testthat::expect_identical(resultado$data_mais_recente_dos_atos, as.Date(NA_character_))
})

testthat::test_that("unit: consultas propagam o par de datas e nao atribuem maximo ao ato pesquisado", {
  x <- testthat::test_path("fixtures", "anexos-xi.xlsx")
  ncm <- buscar_ncm(x, "01012900") |> dplyr::filter(.data$anexo == "xi", .data$no_ex == "002")
  ato <- buscar_ato(x, 705)
  ex <- ex_tarifarios(x) |> dplyr::filter(.data$lista == "Letec+", .data$no_ex == "002")
  for (resultado in list(ncm, ato, ex)) {
    testthat::expect_identical(resultado$datas_dos_atos_texto, "25/02/2025 e 06/11/2026")
    testthat::expect_identical(resultado$data_mais_recente_dos_atos, as.Date("2026-11-06"))
    testthat::expect_false(any(c("data_do_ato", "data_do_ato_de_inclusao") %in% names(resultado)))
  }
})

testthat::test_that("unit: tarifas aplicadas nao silenciam avisos de datas invalidas", {
  x <- testthat::test_path("fixtures", "anexos-xi.xlsx")
  leitor <- tarifasimpbr:::ler_anexo
  testthat::local_mocked_bindings(
    ler_anexo = function(x, n_anexo) {
      dados <- leitor(x, n_anexo)
      if (n_anexo == "xi") {
        dados$datas_dos_atos_texto[[1]] <- "06/11/2026 e 31/02/2026"
        dados$data_mais_recente_dos_atos <- tarifasimpbr:::formata_datas_atos(dados$datas_dos_atos_texto)
      }
      dados
    },
    .package = "tarifasimpbr"
  )
  testthat::expect_warning(tarifas_aplicadas(x, detalhar = TRUE), "Datas de atos invalidas")
})

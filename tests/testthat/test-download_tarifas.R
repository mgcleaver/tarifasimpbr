testthat::test_that("unit: extrair_data_link_tarifas retorna data do link", {
  link <- paste0(
    "https://www.gov.br/mdic/pt-br/assuntos/camex/estrategia-comercial/",
    "arquivos-listas/11-05-2026-anexos-i-a-x-resolucao-gecex-272-21-1.xlsx"
  )

  data_link <- tarifasimpbr:::extrair_data_link_tarifas(link)

  testthat::expect_equal(data_link, as.Date("2026-05-11"))
  testthat::expect_s3_class(data_link, "Date")
})

testthat::test_that("unit: extrair_data_link_tarifas falha sem data no link", {
  link <- paste0(
    "https://www.gov.br/mdic/pt-br/assuntos/camex/estrategia-comercial/",
    "arquivos-listas/anexos-i-a-x-resolucao-gecex-272-21-1.xlsx"
  )

  testthat::expect_error(
    tarifasimpbr:::extrair_data_link_tarifas(link),
    "Nao foi possivel identificar a data"
  )

  testthat::expect_error(
    tarifasimpbr:::extrair_data_link_tarifas(character()),
    "Nao foi possivel identificar a data"
  )
})

testthat::test_that("unit: data_arquivo_tarifas usa link oficial de tarifas", {
  testthat::local_mocked_bindings(
    obter_link_arquivo_tarifas = function() {
      paste0(
        "https://www.gov.br/mdic/pt-br/assuntos/camex/estrategia-comercial/",
        "arquivos-listas/11-05-2026-anexos-i-a-x-resolucao-gecex-272-21-1.xlsx"
      )
    },
    .package = "tarifasimpbr"
  )

  data_link <- data_arquivo_tarifas()

  testthat::expect_equal(data_link, as.Date("2026-05-11"))
  testthat::expect_s3_class(data_link, "Date")
})

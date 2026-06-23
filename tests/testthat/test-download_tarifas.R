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

testthat::test_that("unit: download_tarifas usa caminho temporario por padrao", {
  link_esperado <- paste0(
    "https://www.gov.br/mdic/pt-br/assuntos/camex/estrategia-comercial/",
    "arquivos-listas/11-05-2026-anexos-i-a-x-resolucao-gecex-272-21-1.xlsx"
  )
  caminho_temporario <- file.path(tempdir(), "tarifas_raw.xlsx")
  caminho_download <- NULL

  on.exit(unlink(caminho_temporario), add = TRUE)

  testthat::local_mocked_bindings(
    obter_link_arquivo_tarifas = function() link_esperado,
    .package = "tarifasimpbr"
  )

  testthat::local_mocked_bindings(
    download.file = function(url, destfile, mode, ...) {
      caminho_download <<- destfile
      testthat::expect_equal(url, link_esperado)
      testthat::expect_equal(mode, "wb")
      file.create(destfile)
      0L
    },
    .package = "utils"
  )

  caminho <- download_tarifas()

  testthat::expect_equal(caminho, caminho_temporario)
  testthat::expect_equal(caminho_download, caminho_temporario)
  testthat::expect_true(file.exists(caminho_temporario))
})

testthat::test_that("unit: download_tarifas salva no destfile informado", {
  link_esperado <- paste0(
    "https://www.gov.br/mdic/pt-br/assuntos/camex/estrategia-comercial/",
    "arquivos-listas/11-05-2026-anexos-i-a-x-resolucao-gecex-272-21-1.xlsx"
  )
  diretorio_destino <- tempfile("tarifasimpbr-destfile-")
  dir.create(diretorio_destino)
  destfile <- file.path(diretorio_destino, "tarifas.xlsx")
  caminho_download <- NULL

  on.exit(unlink(diretorio_destino, recursive = TRUE), add = TRUE)

  testthat::local_mocked_bindings(
    obter_link_arquivo_tarifas = function() link_esperado,
    .package = "tarifasimpbr"
  )

  testthat::local_mocked_bindings(
    download.file = function(url, destfile, mode, ...) {
      caminho_download <<- destfile
      testthat::expect_equal(url, link_esperado)
      testthat::expect_equal(mode, "wb")
      file.create(destfile)
      0L
    },
    .package = "utils"
  )

  caminho <- download_tarifas(destfile = destfile)

  testthat::expect_equal(caminho, destfile)
  testthat::expect_equal(caminho_download, destfile)
  testthat::expect_true(file.exists(destfile))
})

testthat::test_that("unit: download_tarifas exige nome de arquivo no destfile", {
  testthat::local_mocked_bindings(
    obter_link_arquivo_tarifas = function() {
      stop("Link nao deve ser consultado")
    },
    .package = "tarifasimpbr"
  )

  testthat::expect_error(
    download_tarifas(destfile = tempdir()),
    "nome do arquivo"
  )
})

testthat::test_that("unit: download_tarifas exige diretorio existente no destfile", {
  diretorio_inexistente <- file.path(tempdir(), "tarifasimpbr-inexistente")
  destfile <- file.path(diretorio_inexistente, "tarifas.xlsx")

  testthat::local_mocked_bindings(
    obter_link_arquivo_tarifas = function() {
      stop("Link nao deve ser consultado")
    },
    .package = "tarifasimpbr"
  )

  testthat::expect_error(
    download_tarifas(destfile = destfile),
    "diretorio.*nao existe"
  )
})

testthat::test_that("unit: download_tarifas valida destfile informado", {
  testthat::local_mocked_bindings(
    obter_link_arquivo_tarifas = function() {
      stop("Link nao deve ser consultado")
    },
    .package = "tarifasimpbr"
  )

  testthat::expect_error(
    download_tarifas(destfile = c("a.xlsx", "b.xlsx")),
    "string unica"
  )
  testthat::expect_error(
    download_tarifas(destfile = NA_character_),
    "string unica"
  )
  testthat::expect_error(
    download_tarifas(destfile = ""),
    "string unica"
  )
})

testthat::test_that("unit: download_tarifas trata erro durante download", {
  destfile <- tempfile(fileext = ".xlsx")

  testthat::local_mocked_bindings(
    obter_link_arquivo_tarifas = function() "https://exemplo.com/tarifas.xlsx",
    .package = "tarifasimpbr"
  )

  testthat::local_mocked_bindings(
    download.file = function(...) stop("falha simulada"),
    .package = "utils"
  )

  testthat::expect_error(
    download_tarifas(destfile = destfile),
    "Download do arquivo de tarifas falhou"
  )
})

testthat::test_that("unit: download_tarifas trata retorno diferente de zero", {
  destfile <- tempfile(fileext = ".xlsx")

  testthat::local_mocked_bindings(
    obter_link_arquivo_tarifas = function() "https://exemplo.com/tarifas.xlsx",
    .package = "tarifasimpbr"
  )

  testthat::local_mocked_bindings(
    download.file = function(...) 1L,
    .package = "utils"
  )

  testthat::expect_error(
    download_tarifas(destfile = destfile),
    "Download do arquivo de tarifas falhou"
  )
})

testthat::test_that("unit: download_tarifas exige arquivo apos sucesso", {
  destfile <- tempfile(fileext = ".xlsx")

  testthat::local_mocked_bindings(
    obter_link_arquivo_tarifas = function() "https://exemplo.com/tarifas.xlsx",
    .package = "tarifasimpbr"
  )

  testthat::local_mocked_bindings(
    download.file = function(...) 0L,
    .package = "utils"
  )

  testthat::expect_error(
    download_tarifas(destfile = destfile),
    "Download do arquivo de tarifas falhou"
  )
})

testthat::test_that("unit: todos os anexos reutilizam a leitura real em cache", {
  limpar_cache_tarifas()
  on.exit(limpar_cache_tarifas(), add = TRUE)
  arquivo <- testthat::test_path("fixtures", "anexos-cache.xlsx")
  anexos <- listar_anexos()$numero_anexo
  leituras <- stats::setNames(integer(length(anexos)), anexos)
  leitor <- tarifasimpbr:::ler_anexo_sem_cache
  testthat::local_mocked_bindings(
    ler_anexo_sem_cache = function(x, n_anexo) {
      leituras[[n_anexo]] <<- leituras[[n_anexo]] + 1L
      leitor(x, n_anexo)
    },
    .package = "tarifasimpbr"
  )

  for (anexo in anexos) {
    frio <- ler_anexo(arquivo, anexo)
    testthat::expect_true(nrow(frio) > 0, info = anexo)
    testthat::expect_message(
      quente <- ler_anexo(arquivo, anexo),
      paste0("Obtendo Anexo ", toupper(anexo), " processado do cache")
    )
    testthat::expect_identical(quente, frio, info = anexo)
  }
  testthat::expect_equal(unname(leituras), rep(1L, length(anexos)))
  testthat::expect_true(limpar_cache_tarifas())
  for (anexo in anexos) ler_anexo(arquivo, anexo)
  testthat::expect_equal(unname(leituras), rep(2L, length(anexos)))
})

testthat::test_that("unit: consultas publicas sao equivalentes com cache frio e quente", {
  limpar_cache_tarifas()
  on.exit(limpar_cache_tarifas(), add = TRUE)
  arquivo <- testthat::test_path("fixtures", "anexos-cache.xlsx")
  consultas <- function() list(
    tarifas = tarifas_aplicadas(arquivo, detalhar = TRUE),
    ncm = buscar_ncm(arquivo, "01012100"),
    ato = buscar_ato(arquivo, 68),
    ex = ex_tarifarios(arquivo)
  )
  frio <- consultas()
  testthat::expect_identical(consultas(), frio)
  limpar_cache_tarifas()
  testthat::expect_identical(consultas(), frio)
})

testthat::test_that("unit: cache invalida todos os anexos por tamanho ou mtime", {
  limpar_cache_tarifas()
  on.exit(limpar_cache_tarifas(), add = TRUE)
  arquivo <- tempfile()
  on.exit(unlink(arquivo), add = TRUE)
  writeBin(charToRaw("a"), arquivo)
  instante <- file.info(arquivo)$mtime
  anexos <- listar_anexos()$numero_anexo
  leituras <- 0L
  testthat::local_mocked_bindings(
    ler_anexo_sem_cache = function(x, n_anexo) {
      leituras <<- leituras + 1L
      tibble::tibble(anexo = n_anexo, conteudo = readLines(x, warn = FALSE))
    },
    .package = "tarifasimpbr"
  )
  ler_todos <- function() lapply(anexos, function(anexo) ler_anexo(arquivo, anexo))
  anterior <- ler_todos()
  testthat::expect_identical(ler_todos(), anterior)
  testthat::expect_equal(leituras, length(anexos))

  writeBin(charToRaw("bb"), arquivo)
  Sys.setFileTime(arquivo, instante)
  testthat::expect_equal(file.info(arquivo)$size, 2)
  testthat::expect_equal(as.numeric(file.info(arquivo)$mtime), as.numeric(instante), tolerance = 1e-5)
  por_tamanho <- ler_todos()
  testthat::expect_equal(leituras, 2L * length(anexos))
  testthat::expect_true(all(vapply(por_tamanho, function(x) x$conteudo == "bb", logical(1))))

  writeBin(charToRaw("cc"), arquivo)
  Sys.setFileTime(arquivo, instante + 10)
  testthat::expect_equal(file.info(arquivo)$size, 2)
  testthat::expect_true(file.info(arquivo)$mtime > instante)
  por_mtime <- ler_todos()
  testthat::expect_equal(leituras, 3L * length(anexos))
  testthat::expect_true(all(vapply(por_mtime, function(x) x$conteudo == "cc", logical(1))))
  testthat::expect_identical(ler_todos(), por_mtime)
  testthat::expect_equal(leituras, 3L * length(anexos))
})

testthat::test_that("unit: cache separa arquivos e normaliza caminhos equivalentes", {
  limpar_cache_tarifas()
  on.exit(limpar_cache_tarifas(), add = TRUE)
  arquivos <- c(tempfile(), tempfile())
  file.create(arquivos)
  on.exit(unlink(arquivos), add = TRUE)
  leituras <- 0L
  testthat::local_mocked_bindings(
    ler_anexo_sem_cache = function(x, n_anexo) {
      leituras <<- leituras + 1L
      tibble::tibble(arquivo = normalizePath(x), anexo = n_anexo)
    },
    .package = "tarifasimpbr"
  )
  for (anexo in listar_anexos()$numero_anexo) {
    primeiro <- ler_anexo(arquivos[[1]], anexo)
    segundo <- ler_anexo(arquivos[[2]], anexo)
    testthat::expect_false(identical(primeiro, segundo))
    alias <- file.path(dirname(arquivos[[1]]), ".", basename(arquivos[[1]]))
    testthat::expect_identical(ler_anexo(alias, anexo), primeiro)
  }
  testthat::expect_equal(leituras, 18L)
})

testthat::test_that("unit: falhas na chave ou leitura nao deixam cache invalido", {
  limpar_cache_tarifas()
  on.exit(limpar_cache_tarifas(), add = TRUE)
  arquivo <- tempfile()
  file.create(arquivo)
  on.exit(unlink(arquivo), add = TRUE)
  chamadas <- 0L
  testthat::local_mocked_bindings(
    ler_anexo_sem_cache = function(x, n_anexo) {
      chamadas <<- chamadas + 1L
      if (chamadas == 1L) stop("leitura incompleta")
      tibble::tibble(anexo = n_anexo, chamada = chamadas)
    },
    .package = "tarifasimpbr"
  )
  testthat::expect_error(ler_anexo(arquivo, "ii"), "leitura incompleta")
  completo <- ler_anexo(arquivo, "ii")
  testthat::expect_identical(ler_anexo(arquivo, "ii"), completo)
  testthat::expect_equal(chamadas, 2L)

  testthat::local_mocked_bindings(
    cria_chave_cache_anexo_i = function(x) NA_character_,
    .package = "tarifasimpbr"
  )
  for (anexo in listar_anexos()$numero_anexo) {
    primeiro <- ler_anexo(arquivo, anexo)
    segundo <- ler_anexo(arquivo, anexo)
    testthat::expect_false(identical(primeiro, segundo))
  }
  testthat::expect_equal(chamadas, 20L)
})

testthat::test_that("unit: dados em cache preservam vigencias para consultas posteriores", {
  limpar_cache_tarifas()
  on.exit(limpar_cache_tarifas(), add = TRUE)
  arquivo <- testthat::test_path("fixtures", "anexos-cache.xlsx")
  dados <- ler_anexo(arquivo, "v")
  testthat::expect_equal(nrow(dados), 2L)

  # Injeta a data apenas na copia local da funcao, sem alterar o relogio do R.
  data_consulta <- as.Date("2026-10-26")
  filtro <- tarifas_vigentes
  environment(filtro) <- list2env(
    list(Sys.Date = function() data_consulta), parent = environment(filtro)
  )
  testthat::expect_equal(filtro(dados)$aliquota, "2")
  data_consulta <- as.Date("2030-01-01")
  quente <- ler_anexo(arquivo, "v")
  testthat::expect_identical(quente, dados)
  testthat::expect_equal(filtro(quente)$aliquota, "4")
})

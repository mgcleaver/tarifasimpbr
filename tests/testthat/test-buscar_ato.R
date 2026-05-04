colunas_buscar_ato <- c(
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

mock_listar_anexos_buscar_ato <- function() {
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

mock_ler_anexo_buscar_ato <- function(x, n_anexo) {
  switch(
    n_anexo,
    i = tibble::tibble(
      ncm = c("11090000", "22090000", "33090000"),
      descricao_tec = c("Gluten de trigo", "Vinagre", "Perfumes"),
      descricao_tec_concatenada = c(
        "Produtos vegetais. Gluten de trigo.",
        "Vinagre.",
        "Produtos quimicos. Perfumes."
      ),
      tec = c(10, 12, 18),
      bkbit = c(NA_character_, NA_character_, "BK"),
      resolucoes = c("Res. TEC 68", "Res. TEC 680", "Res. TEC 168")
    ),
    ii = tibble::tibble(
      ncm = c("11090000", "22090000"),
      descricao = c("Descricao propria II", "Outra descricao II"),
      tec = c("10", "12"),
      aliquota_aplicada = c("8", "9"),
      atos_de_inclusao = c("068/2024", "00680/2024")
    ),
    iii = tibble::tibble(
      ncm = "1109",
      regra = "Regra sem ato",
      obs = NA_character_
    ),
    iv = tibble::tibble(ncm = character()),
    v = tibble::tibble(
      ncm = c("11090000", "22090000"),
      descricao = c("Descricao propria V", "Outra descricao V"),
      no_ex = c("001", "002"),
      aliquota = c("0", "2"),
      quota = c(NA_character_, "100"),
      unidade_quota = c(NA_character_, "kg"),
      inicio_de_vigencia = as.Date(c("2025-01-01", "2025-06-01")),
      termino_de_vigencia = as.Date(c("2025-12-31", "2026-12-31")),
      ato_de_inclusao = c("Res. 068/2024", "Res. 168/2024"),
      data_do_ato_de_inclusao = as.Date(c("2024-12-01", "2025-05-01")),
      obs = c(NA_character_, "Observacao")
    ),
    vi = tibble::tibble(ncm = character()),
    viii = tibble::tibble(ncm = character()),
    ix = tibble::tibble(ncm = character()),
    x = tibble::tibble(ncm = character())
  )
}

testthat::test_that("unit: buscar_ato retorna schema igual ao buscar_ncm", {
  testthat::local_mocked_bindings(
    listar_anexos = mock_listar_anexos_buscar_ato,
    ler_anexo = mock_ler_anexo_buscar_ato,
    .package = "tarifasimpbr"
  )

  resultado <- buscar_ato(x = "arquivo_mock.xlsx", ato = 68)

  testthat::expect_s3_class(resultado, "tbl_df")
  testthat::expect_identical(names(resultado), colunas_buscar_ato)
  testthat::expect_equal(
    resultado$ato,
    c("Res. TEC 68", "068/2024", "Res. 068/2024")
  )
  testthat::expect_false("iii" %in% resultado$anexo)
  testthat::expect_equal(resultado$descricao, rep("Gluten de trigo", 3))
})

testthat::test_that("unit: buscar_ato usa descricao do anexo i quando anexo possui descricao propria", {
  testthat::local_mocked_bindings(
    listar_anexos = mock_listar_anexos_buscar_ato,
    ler_anexo = mock_ler_anexo_buscar_ato,
    .package = "tarifasimpbr"
  )

  resultado <- buscar_ato(x = "arquivo_mock.xlsx", ato = 68)

  testthat::expect_equal(resultado$descricao, rep("Gluten de trigo", 3))
  testthat::expect_false("Descricao propria II" %in% resultado$descricao)
  testthat::expect_false("Descricao propria V" %in% resultado$descricao)
})

testthat::test_that("unit: buscar_ato busca numero com zeros a esquerda sem falsos positivos", {
  testthat::local_mocked_bindings(
    listar_anexos = mock_listar_anexos_buscar_ato,
    ler_anexo = mock_ler_anexo_buscar_ato,
    .package = "tarifasimpbr"
  )

  resultado_numero <- buscar_ato(x = "arquivo_mock.xlsx", ato = 68)
  resultado_texto <- buscar_ato(x = "arquivo_mock.xlsx", ato = "68")

  testthat::expect_equal(resultado_numero$ato, resultado_texto$ato)
  testthat::expect_true("Res. TEC 68" %in% resultado_numero$ato)
  testthat::expect_true("068/2024" %in% resultado_numero$ato)
  testthat::expect_true("Res. 068/2024" %in% resultado_numero$ato)
  testthat::expect_false("Res. TEC 680" %in% resultado_numero$ato)
  testthat::expect_false("Res. TEC 168" %in% resultado_numero$ato)
  testthat::expect_false("00680/2024" %in% resultado_numero$ato)
})

testthat::test_that("unit: buscar_ato nao busca numero dentro do ano", {
  testthat::local_mocked_bindings(
    listar_anexos = mock_listar_anexos_buscar_ato,
    ler_anexo = function(x, n_anexo) {
      switch(
        n_anexo,
        i = tibble::tibble(
          ncm = c("11090000", "22090000", "33090000"),
          descricao_tec = c("Gluten de trigo", "Vinagre", "Perfumes"),
          descricao_tec_concatenada = c(
            "Produtos vegetais. Gluten de trigo.",
            "Vinagre.",
            "Produtos quimicos. Perfumes."
          ),
          tec = c(10, 12, 18),
          bkbit = c(NA_character_, NA_character_, "BK"),
          resolucoes = c("100/2024", "202/2024", "202/2017")
        ),
        ii = tibble::tibble(ncm = character()),
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

  resultado <- buscar_ato(x = "arquivo_mock.xlsx", ato = 202)

  testthat::expect_equal(resultado$ato, c("202/2024", "202/2017"))
  testthat::expect_false("100/2024" %in% resultado$ato)
})

testthat::test_that("unit: buscar_ato usa busca textual literal", {
  testthat::local_mocked_bindings(
    listar_anexos = mock_listar_anexos_buscar_ato,
    ler_anexo = function(x, n_anexo) {
      switch(
        n_anexo,
        i = tibble::tibble(
          ncm = c("11090000", "22090000"),
          descricao_tec = c("Gluten de trigo", "Vinagre"),
          descricao_tec_concatenada = c("Produtos vegetais. Gluten de trigo.", "Vinagre."),
          tec = c(10, 12),
          bkbit = c(NA_character_, NA_character_),
          resolucoes = c("Res. 68/2024", "Res. 068/2024")
        ),
        ii = tibble::tibble(ncm = character()),
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

  resultado <- buscar_ato(x = "arquivo_mock.xlsx", ato = "Res. 68")

  testthat::expect_equal(resultado$ato, "Res. 68/2024")
})

testthat::test_that("unit: buscar_ato retorna vazio com warning quando ato nao existe", {
  testthat::local_mocked_bindings(
    listar_anexos = mock_listar_anexos_buscar_ato,
    ler_anexo = mock_ler_anexo_buscar_ato,
    .package = "tarifasimpbr"
  )

  testthat::expect_warning(
    resultado <- buscar_ato(x = "arquivo_mock.xlsx", ato = "Ato inexistente"),
    "Pesquisa n\u00e3o encontrada em atos"
  )

  testthat::expect_s3_class(resultado, "tbl_df")
  testthat::expect_identical(names(resultado), colunas_buscar_ato)
  testthat::expect_equal(nrow(resultado), 0)
})

testthat::test_that("unit: buscar_ato falha para ato invalido", {
  testthat::expect_error(
    buscar_ato(x = "arquivo_mock.xlsx", ato = NA),
    "ato deve conter um texto ou numero de ato valido"
  )
  testthat::expect_error(
    buscar_ato(x = "arquivo_mock.xlsx", ato = c("68", "69")),
    "ato deve conter um texto ou numero de ato valido"
  )
  testthat::expect_error(
    buscar_ato(x = "arquivo_mock.xlsx", ato = " "),
    "ato deve conter um texto ou numero de ato valido"
  )
  testthat::expect_error(
    buscar_ato(x = "arquivo_mock.xlsx", ato = 68.5),
    "ato deve conter um texto ou numero de ato valido"
  )
})

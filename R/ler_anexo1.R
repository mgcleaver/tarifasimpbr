#' Funcao auxiliar usada na funcao ler_anexo
#'
#' Esta funcao faz a leitura e processamento do Anexo I para obtencao
#' da TEC a partir do arquivo oficial de tarifas disponibilizado no site da Camex.
#'
#' Le a aba referente ao Anexo I da planiha excel obtida pela funcao
#'  `download_tarifas`, limpa e organiza as informacoes.
#'
#' @param x Objeto retornado pela funcao `download_tarifas`
#'
#' @return Um tibble que retorna as colunas ncm, descricao_tec,
#' descricao_tec_concatenada, tec, bkbit, resolucoes.
#'
#' @details
#' A funcao cria a coluna descricao_tec_concatenada, a qual apresenta a concatenacao
#' das descricoes que vao da posicao (4 digitos) ate o subitem da NCM (8 digitos).
#' A coluna tec representa a Tarifa Externa Comum e os seus valores estao em
#' porcentagens.
#'
#' @keywords internal
ler_anexo1 <- function(x) {
  message("Processando Anexo I...")

  raw <- suppressMessages(
    readxl::read_excel(
      x,
      sheet = 1,
      col_names = F,
      guess_max = 1e6,
      col_types = c("text")
    )
  )

  resolucoes <- raw |>
    janitor::clean_names() |>
    dplyr::rename(ncm = .data$x1, resolucoes = .data$x4) |>
    dplyr::filter(!is.na(resolucoes)) |>
    dplyr::mutate(ncm = stringr::str_remove_all(.data$ncm, "\\.")) |>
    dplyr::filter(nchar(.data$ncm) == 8) |>
    dplyr::select(.data$ncm, resolucoes)

  df <- raw |>
    dplyr::select(1:3)

  # definir cabecalhos
  cabecalhos <- c("NCM", "DESCRI\u00c7\u00c3O", "TEC(%)")

  # iniciar variaveis para identificar inicio e fim de tabelas
  na_tabela <- FALSE
  inicio_tabela <- 0

  # iniciar tabela vazia
  compila_anexoi <- tibble::tibble()

  # iterar nas linhas do anexo i
  for (i in 1:nrow(df)) {
    linha <- df[i, ]

    if (all(linha == cabecalhos)) {
      # comecar nova tabela
      na_tabela <- TRUE
      inicio_tabela <- i + 1
    }

    if (
      na_tabela &&
        !all(linha == cabecalhos) &&
        !is.na(linha[1]) &&
        all(is.na(linha[, 2:3]))
    ) {
      compila_anexoi <- dplyr::bind_rows(
        compila_anexoi,
        df[inicio_tabela:(i - 1), ]
      )
      na_tabela <- FALSE
    }
  }

  # se arquivo terminar ainda em uma tabela, adiciona as linhas remanescentes
  if (na_tabela) {
    compila_anexoi <- dplyr::bind_rows(
      compila_anexoi,
      df[inicio_tabela:nrow(df), ]
    )
  }

  colnames(compila_anexoi) <- cabecalhos

  compila_anexoi <- compila_anexoi |>
    janitor::clean_names()

  compila_anexoi <- compila_anexoi |>
    dplyr::filter(stringr::str_detect(.data$ncm, "\\.")) |>
    dplyr::mutate(bkbit = stringr::str_extract(.data$tec_percent, "BK|BIT")) |>
    dplyr::mutate(
      ncm = stringr::str_remove_all(.data$ncm, "\\."),
      ncm = stringr::str_remove_all(.data$ncm, ","),
      tec_percent = stringr::str_remove(.data$tec_percent, "BK|BIT"),
      tec_percent = .data$tec_percent |>
        stringr::str_replace(",", "\\.") |>
        as.numeric()
    ) |>
    dplyr::arrange(.data$ncm)

  #### concatena descricoes ####

  # funcao auxiliar para criar tabelas ao nivel de diferentes
  # digitos da ncm
  cria_tabelas <- function(x) {
    compila_anexoi |>
      dplyr::filter(nchar(.data$ncm) == x) |>
      dplyr::select(-.data$tec_percent, -.data$bkbit)
  }

  # cria tabelas de descricao para 4, 5, 6, 7 e 8 digitos
  anexoi_4d <- cria_tabelas(4) |>
    dplyr::rename(sh4 = .data$ncm, descricao_4 = .data$descricao)
  anexoi_5d <- cria_tabelas(5) |>
    dplyr::rename(sh5 = .data$ncm, descricao_5 = .data$descricao)
  anexoi_6d <- cria_tabelas(6) |>
    dplyr::rename(sh6 = .data$ncm, descricao_6 = .data$descricao)
  anexoi_7d <- cria_tabelas(7) |>
    dplyr::rename(sh7 = .data$ncm, descricao_7 = .data$descricao)
  anexoi_8d <- compila_anexoi |>
    dplyr::filter(nchar(.data$ncm) == 8) |>
    dplyr::mutate(sh7 = stringr::str_sub(.data$ncm, 1, 7))

  anexoi_8d <- anexoi_8d |>
    dplyr::rename(descricao_8 = .data$descricao) |>
    dplyr::mutate(
      sh6 = stringr::str_sub(.data$ncm, 1, 6),
      sh5 = stringr::str_sub(.data$ncm, 1, 5),
      sh4 = stringr::str_sub(.data$ncm, 1, 4)
    )

  # funcao auxiliar para concatenar descricoes sh4, sh5, sh6, sh7, sh8

  concatena_texto <- function(...) {
    texts <- c(...) |>
      na.omit()

    if (length(texts) == 0) {
      return(NA_character_)
    }

    texto_concatenado <- ""
    for (i in seq_along(texts)) {
      if (i == 1) {
        texto_concatenado <- texts[i]
      } else if (grepl(":$", texts[i - 1])) {
        texto_concatenado <- paste0(texto_concatenado, " ", texts[i])
      } else {
        texto_concatenado <- paste0(texto_concatenado, ". ", texts[i])
      }
    }
    paste0(texto_concatenado, ".")
  }

  anexoi_8d |>
    dplyr::left_join(anexoi_7d, by = "sh7") |>
    dplyr::left_join(anexoi_6d, by = "sh6") |>
    dplyr::left_join(anexoi_5d, by = "sh5") |>
    dplyr::left_join(anexoi_4d, by = "sh4") |>
    dplyr::rowwise() |>
    dplyr::transmute(
      .data$ncm,
      descricao_tec = .data$descricao_8,
      descricao_tec_concatenada = concatena_texto(
        .data$descricao_4,
        .data$descricao_5,
        .data$descricao_6,
        .data$descricao_7,
        .data$descricao_8
      ),
      .data$tec_percent,
      .data$bkbit
    ) |>
    dplyr::mutate(
      descricao_tec_concatenada = stringr::str_replace_all(
        .data$descricao_tec_concatenada,
        "\\.\\.",
        "\\."
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::left_join(resolucoes, by = "ncm") |>
    dplyr::rename(tec = .data$tec_percent)
}

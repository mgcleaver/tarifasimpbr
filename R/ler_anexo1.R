ler_anexo1 <- function(x) {

  message("Processando Anexo I...")

  raw <- suppressMessages(
    readxl::read_excel(
      x,
      sheet = 1,
      col_names = F,
      guess_max = 1e6,
      col_types = c("text")))

  resolucoes <- raw |>
    janitor::clean_names() |>
    dplyr::rename(ncm = x1, resolucoes = x4) |>
    dplyr::filter(!is.na(resolucoes)) |>
    dplyr::mutate(ncm = stringr::str_remove_all(ncm, "\\.")) |>
    dplyr::filter(nchar(ncm) == 8) |>
    dplyr::select(ncm, resolucoes)

  df <- raw |>
    dplyr::select(1:3)


  # definir cabecalhos
  cabecalhos <- c("NCM", "DESCRIÇÃO", "TEC(%)")

  # iniciar variáveis para identificar inicio e fim de tabelas
  na_tabela <- FALSE
  inicio_tabela <- 0

  # iniciar tabela vazia
  compila_anexoi <- tibble::tibble()

  # iterar nas linhas do anexo i
  for(i in 1:nrow(df)) {
    linha <- df[i, ]

    if(all(linha == cabecalhos)) {
      # começar nova tabela
      na_tabela <- TRUE
      inicio_tabela <- i + 1
    }

    if(na_tabela && !all(linha == cabecalhos) && !is.na(linha[1]) && all(is.na(linha[, 2:3]))) {
      compila_anexoi <- dplyr::bind_rows(compila_anexoi, df[inicio_tabela:(i-1), ])
      na_tabela <- FALSE

    }

  }

  # se arquivo terminar ainda em uma tabela, adiciona as linhas remanescentes
  if(na_tabela) {
    compila_anexoi <- dplyr::bind_rows(
      compila_anexoi,
      df[inicio_tabela:nrow(df), ])
  }

  colnames(compila_anexoi) <- cabecalhos

  compila_anexoi <- compila_anexoi  |>
    janitor::clean_names()

  compila_anexoi <- compila_anexoi |>
    dplyr::filter(stringr::str_detect(ncm, "\\.")) |>
    dplyr::mutate(bkbit = stringr::str_extract(tec_percent, "BK|BIT")) |>
    dplyr::mutate(ncm = stringr::str_remove_all(ncm, "\\."),
                  ncm = stringr::str_remove_all(ncm, ","),
                  tec_percent = stringr::str_remove(tec_percent, "BK|BIT"),
                  tec_percent = tec_percent |>
                    stringr::str_replace(",", "\\.") |>
                    as.numeric()) |>
    dplyr::arrange(ncm)


  #### concatena descricoes ####

  # funcao auxiliar para criar tabelas ao nível de diferentes
  # digitos da ncm
  cria_tabelas <- function(x) {
    compila_anexoi |>
      dplyr::filter(nchar(ncm) == x) |>
      dplyr::select(-tec_percent, -bkbit)
  }

  # cria tabelas de descricao para 4, 5, 6, 7 e 8 digitos
  anexoi_4d <- cria_tabelas(4) |>
    dplyr::rename(sh4 = ncm,
                  descricao_4 = descricao)
  anexoi_5d <- cria_tabelas(5) |>
    dplyr::rename(sh5 = ncm,
                  descricao_5 = descricao)
  anexoi_6d <- cria_tabelas(6) |>
    dplyr::rename(sh6 = ncm,
                  descricao_6 = descricao)
  anexoi_7d <- cria_tabelas(7) |>
    dplyr::rename(sh7 = ncm,
                  descricao_7 = descricao)
  anexoi_8d <- compila_anexoi |>
    dplyr::filter(nchar(ncm) == 8) |>
    dplyr::mutate(sh7 = stringr::str_sub(ncm, 1, 7))

  anexoi_8d <- anexoi_8d |>
    dplyr::rename(descricao_8 = descricao) |>
    dplyr::mutate(
      sh6 = stringr::str_sub(ncm, 1, 6),
      sh5 = stringr::str_sub(ncm, 1, 5),
      sh4 = stringr::str_sub(ncm, 1, 4)
    )

  # funcao auxiliar para concatenar descricoes sh4, sh5, sh6, sh7, sh8

  concatena_texto <- function(...) {
    texts <- c(...) |>
      na.omit()

    if(length(texts) == 0) return(NA_character_)

    texto_concatenado <- ""
    for (i in seq_along(texts)) {
      if (i == 1) {
        texto_concatenado <- texts[i]
      } else if (grepl(":$", texts[i-1])) {
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
      ncm,
      descricao_tec = descricao_8,
      descricao_tec_concatenada = concatena_texto(
        descricao_4, descricao_5, descricao_6, descricao_7, descricao_8
      ),
      tec_percent,
      bkbit) |>
    dplyr::mutate(
      descricao_tec_concatenada = stringr::str_replace_all(
        descricao_tec_concatenada, "\\.\\.", "\\."
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::left_join(resolucoes, by = "ncm")
}


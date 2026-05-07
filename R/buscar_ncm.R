#' Buscar informacoes de uma NCM nos anexos tarifarios
#'
#' Consulta uma NCM nos anexos tarifarios e retorna as ocorrencias encontradas
#' em formato padronizado. A busca usa correspondencia exata nos anexos com NCM
#' completa e, no Anexo III, tambem retorna codigos raiz que abrangem a NCM
#' pesquisada. Quando houver correspondencia exata no Anexo I, suas descricoes
#' sao usadas como referencia para os demais anexos.
#'
#' @param x Caminho do arquivo de tarifas, normalmente o objeto retornado por
#'   [download_tarifas()].
#' @param ncm Codigo NCM com 8 digitos. Pontos, hifens e espacos sao aceitos e
#'   ignorados na busca.
#'
#' @return Um `tibble` com uma linha por ocorrencia encontrada e as colunas
#'   `anexo`, `nome_anexo`, `ncm`, `no_ex`, `descricao`,
#'   `descricao_tec_concatenada`, `bkbit`, `aliquota`, `quota`,
#'   `unidade_quota`, `inicio_de_vigencia`, `termino_de_vigencia`, `ato`,
#'   `data_do_ato` e `obs`. Quando a NCM nao e encontrada, retorna um tibble
#'   vazio com essas colunas e emite um aviso.
#'
#' @examples
#' \dontrun{
#' x <- download_tarifas()
#' buscar_ncm(x, ncm = "11090000")
#' buscar_ncm(x, ncm = "85394900")
#' }
#'
#' @export
buscar_ncm <- function(x, ncm) {
  if (length(ncm) != 1 || is.na(ncm)) {
    stop("ncm deve conter exatamente 8 digitos.", call. = FALSE)
  }

  ncm_pesquisada <- ncm |>
    as.character() |>
    stringr::str_trim() |>
    stringr::str_remove_all("[.\\s-]")

  if (!stringr::str_detect(ncm_pesquisada, "^[0-9]{8}$")) {
    stop("ncm deve conter exatamente 8 digitos.", call. = FALSE)
  }

  colunas_resultado <- c(
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

  resultado_vazio <- tibble::tibble(
    anexo = character(),
    nome_anexo = character(),
    ncm = character(),
    no_ex = character(),
    descricao = character(),
    descricao_tec_concatenada = character(),
    bkbit = character(),
    aliquota = numeric(),
    quota = character(),
    unidade_quota = character(),
    inicio_de_vigencia = as.Date(character()),
    termino_de_vigencia = as.Date(character()),
    ato = character(),
    data_do_ato = as.Date(character()),
    obs = character()
  )

  nomes_anexos <- listar_anexos()
  nome_anexo <- stats::setNames(
    nomes_anexos$nome_abreviado_anexo,
    nomes_anexos$numero_anexo
  )

  anexo_i <- ler_anexo(x, "i") |>
    dplyr::filter(.data$ncm == ncm_pesquisada)

  if (!"descricao_tec" %in% names(anexo_i)) {
    anexo_i$descricao_tec <- rep(NA_character_, nrow(anexo_i))
  }
  if (!"descricao_tec_concatenada" %in% names(anexo_i)) {
    anexo_i$descricao_tec_concatenada <- rep(NA_character_, nrow(anexo_i))
  }
  if (!"tec" %in% names(anexo_i)) {
    anexo_i$tec <- rep(NA_real_, nrow(anexo_i))
  }
  if (!"bkbit" %in% names(anexo_i)) {
    anexo_i$bkbit <- rep(NA_character_, nrow(anexo_i))
  }
  if (!"resolucoes" %in% names(anexo_i)) {
    anexo_i$resolucoes <- rep(NA_character_, nrow(anexo_i))
  }

  descricao <- if (nrow(anexo_i) > 0) {
    as.character(anexo_i$descricao_tec[[1]])
  } else {
    NA_character_
  }

  descricao_tec_concatenada <- if (nrow(anexo_i) > 0) {
    as.character(anexo_i$descricao_tec_concatenada[[1]])
  } else {
    NA_character_
  }

  bkbit <- if (nrow(anexo_i) > 0) {
    as.character(anexo_i$bkbit[[1]])
  } else {
    NA_character_
  }

  resultado <- anexo_i |>
    dplyr::transmute(
      anexo = "i",
      nome_anexo = nome_anexo[["i"]],
      ncm = .data$ncm,
      no_ex = NA_character_,
      descricao = .data$descricao_tec,
      descricao_tec_concatenada = .data$descricao_tec_concatenada,
      bkbit = .data$bkbit,
      aliquota = as.numeric(.data$tec),
      quota = NA_character_,
      unidade_quota = NA_character_,
      inicio_de_vigencia = as.Date(NA_character_),
      termino_de_vigencia = as.Date(NA_character_),
      ato = .data$resolucoes,
      data_do_ato = as.Date(NA_character_),
      obs = NA_character_
    )

  anexo_ii <- ler_anexo(x, "ii") |>
    dplyr::filter(.data$ncm == ncm_pesquisada)

  if (!"aliquota_aplicada" %in% names(anexo_ii)) {
    anexo_ii$aliquota_aplicada <- rep(NA_real_, nrow(anexo_ii))
  }
  if (!"atos_de_inclusao" %in% names(anexo_ii)) {
    anexo_ii$atos_de_inclusao <- rep(NA_character_, nrow(anexo_ii))
  }

  resultado <- dplyr::bind_rows(
    resultado,
    anexo_ii |>
      dplyr::transmute(
        anexo = "ii",
        nome_anexo = nome_anexo[["ii"]],
        ncm = .data$ncm,
        no_ex = NA_character_,
        descricao = .env$descricao,
        descricao_tec_concatenada = .env$descricao_tec_concatenada,
        bkbit = .env$bkbit,
        aliquota = as.numeric(.data$aliquota_aplicada),
        quota = NA_character_,
        unidade_quota = NA_character_,
        inicio_de_vigencia = as.Date(NA_character_),
        termino_de_vigencia = as.Date(NA_character_),
        ato = .data$atos_de_inclusao,
        data_do_ato = as.Date(NA_character_),
        obs = NA_character_
      )
  )

  anexo_iii <- ler_anexo(x, "iii") |>
    dplyr::filter(
      !is.na(.data$ncm),
      .data$ncm != "",
      stringr::str_starts(ncm_pesquisada, .data$ncm)
    )

  resultado <- dplyr::bind_rows(
    resultado,
    anexo_iii |>
      dplyr::transmute(
        anexo = "iii",
        nome_anexo = nome_anexo[["iii"]],
        ncm = .data$ncm,
        no_ex = NA_character_,
        descricao = NA_character_,
        descricao_tec_concatenada = NA_character_,
        bkbit = NA_character_,
        aliquota = NA_real_,
        quota = NA_character_,
        unidade_quota = NA_character_,
        inicio_de_vigencia = as.Date(NA_character_),
        termino_de_vigencia = as.Date(NA_character_),
        ato = NA_character_,
        data_do_ato = as.Date(NA_character_),
        obs = NA_character_
      )
  )

  for (anexo_atual in c("iv", "v", "vi", "viii", "ix", "x")) {
    anexo_lista <- ler_anexo(x, anexo_atual) |>
      dplyr::filter(.data$ncm == ncm_pesquisada)

    if (!"no_ex" %in% names(anexo_lista)) {
      anexo_lista$no_ex <- rep(NA_character_, nrow(anexo_lista))
    }
    if (!"aliquota" %in% names(anexo_lista)) {
      anexo_lista$aliquota <- rep(NA_real_, nrow(anexo_lista))
    }
    if (!"quota" %in% names(anexo_lista)) {
      anexo_lista$quota <- rep(NA_character_, nrow(anexo_lista))
    }
    if (!"unidade_quota" %in% names(anexo_lista)) {
      anexo_lista$unidade_quota <- rep(NA_character_, nrow(anexo_lista))
    }
    if (!"inicio_de_vigencia" %in% names(anexo_lista)) {
      anexo_lista$inicio_de_vigencia <- as.Date(rep(NA_character_, nrow(anexo_lista)))
    }
    if (!"termino_de_vigencia" %in% names(anexo_lista)) {
      anexo_lista$termino_de_vigencia <- as.Date(rep(NA_character_, nrow(anexo_lista)))
    }
    if (!"ato_de_inclusao" %in% names(anexo_lista)) {
      anexo_lista$ato_de_inclusao <- rep(NA_character_, nrow(anexo_lista))
    }
    if (!"data_do_ato_de_inclusao" %in% names(anexo_lista)) {
      anexo_lista$data_do_ato_de_inclusao <- as.Date(rep(NA_character_, nrow(anexo_lista)))
    }
    if (!"obs" %in% names(anexo_lista)) {
      anexo_lista$obs <- rep(NA_character_, nrow(anexo_lista))
    }

    resultado <- dplyr::bind_rows(
      resultado,
      anexo_lista |>
        dplyr::transmute(
          anexo = anexo_atual,
          nome_anexo = nome_anexo[[anexo_atual]],
          ncm = .data$ncm,
          no_ex = .data$no_ex,
          descricao = .env$descricao,
          descricao_tec_concatenada = .env$descricao_tec_concatenada,
          bkbit = .env$bkbit,
          aliquota = as.numeric(.data$aliquota),
          quota = .data$quota,
          unidade_quota = .data$unidade_quota,
          inicio_de_vigencia = .data$inicio_de_vigencia,
          termino_de_vigencia = .data$termino_de_vigencia,
          ato = .data$ato_de_inclusao,
          data_do_ato = .data$data_do_ato_de_inclusao,
          obs = .data$obs
        )
    )
  }

  resultado <- resultado |>
    dplyr::select(dplyr::all_of(colunas_resultado))

  if (nrow(resultado) == 0) {
    warning("NCM n\u00e3o encontrada na busca")
    return(resultado_vazio)
  }

  resultado
}

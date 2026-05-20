#' Buscar informacoes de um ato nos anexos tarifarios
#'
#' Consulta um ato nos anexos tarifarios e retorna as ocorrencias encontradas em
#' formato padronizado. A busca considera os campos de atos disponiveis nos
#' Anexos I, II, IV, V, VI, VIII, IX e X. Entradas numericas buscam o numero do
#' ato com equivalencia de zeros a esquerda, sem buscar dentro do ano do ato.
#' Entradas textuais mistas sao pesquisadas de forma literal, sem diferenciar
#' maiusculas e minusculas.
#'
#' @param x Caminho do arquivo de tarifas, normalmente o objeto retornado por
#'   [download_tarifas()].
#' @param ato Numero ou texto do ato a pesquisar. Valores numericos ou textos
#'   compostos apenas por digitos sao tratados como numero do ato e consideram
#'   zeros a esquerda como equivalentes.
#'
#' @return Um `tibble` com uma linha por ocorrencia encontrada e as colunas
#'   `anexo`, `nome_anexo`, `ncm`, `no_ex`, `descricao`,
#'   `descricao_tec_concatenada`, `bkbit`, `aliquota`, `quota`,
#'   `unidade_quota`, `inicio_de_vigencia`, `termino_de_vigencia`, `ato`,
#'   `data_do_ato` e `obs`. Quando o ato nao e encontrado, retorna um tibble
#'   vazio com essas colunas.
#'
#' @examples
#' \dontrun{
#' x <- download_tarifas()
#' buscar_ato(x, ato = 800)
#' buscar_ato(x, ato = "272")
#' buscar_ato(x, ato = "gecex")
#' }
#'
#' @export
buscar_ato <- function(x, ato) {
  if (length(ato) != 1 || is.na(ato)) {
    stop("ato deve conter um texto ou numero de ato valido.", call. = FALSE)
  }

  if (is.numeric(ato) && (!is.finite(ato) || ato != floor(ato))) {
    stop("ato deve conter um texto ou numero de ato valido.", call. = FALSE)
  }

  ato_pesquisado <- if (is.numeric(ato)) {
    format(ato, scientific = FALSE, trim = TRUE)
  } else {
    as.character(ato)
  }

  ato_pesquisado <- ato_pesquisado |>
    stringr::str_trim()

  if (ato_pesquisado == "") {
    stop("ato deve conter um texto ou numero de ato valido.", call. = FALSE)
  }

  busca_numerica <- stringr::str_detect(ato_pesquisado, "^[0-9]+$")

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

  if (busca_numerica) {
    numero_ato <- ato_pesquisado |>
      stringr::str_remove("^0+")

    if (numero_ato == "") {
      numero_ato <- "0"
    }

    padrao_ato <- paste0("(^|[^0-9/])0*", numero_ato, "(?![0-9])")
  }

  nomes_anexos <- listar_anexos()
  nome_anexo <- stats::setNames(
    nomes_anexos$nome_abreviado_anexo,
    nomes_anexos$numero_anexo
  )

  anexo_i <- ler_anexo(x, "i")

  if (!"ncm" %in% names(anexo_i)) {
    anexo_i$ncm <- rep(NA_character_, nrow(anexo_i))
  }
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

  descricoes_anexo_i <- anexo_i |>
    dplyr::select(
      .data$ncm,
      descricao_anexo_i = .data$descricao_tec,
      descricao_tec_concatenada_anexo_i = .data$descricao_tec_concatenada,
      bkbit_anexo_i = .data$bkbit
    )

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

  anexo_ii <- ler_anexo(x, "ii")

  if (!"ncm" %in% names(anexo_ii)) {
    anexo_ii$ncm <- rep(NA_character_, nrow(anexo_ii))
  }
  if (!"aliquota_aplicada" %in% names(anexo_ii)) {
    anexo_ii$aliquota_aplicada <- rep(NA_real_, nrow(anexo_ii))
  }
  if (!"atos_de_inclusao" %in% names(anexo_ii)) {
    anexo_ii$atos_de_inclusao <- rep(NA_character_, nrow(anexo_ii))
  }

  resultado <- dplyr::bind_rows(
    resultado,
    anexo_ii |>
      dplyr::left_join(descricoes_anexo_i, by = "ncm") |>
      dplyr::transmute(
        anexo = "ii",
        nome_anexo = nome_anexo[["ii"]],
        ncm = .data$ncm,
        no_ex = NA_character_,
        descricao = .data$descricao_anexo_i,
        descricao_tec_concatenada = .data$descricao_tec_concatenada_anexo_i,
        bkbit = .data$bkbit_anexo_i,
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

  for (anexo_atual in c("iv", "v", "vi", "viii", "ix", "x")) {
    anexo_lista <- ler_anexo(x, anexo_atual)

    if (!"ncm" %in% names(anexo_lista)) {
      anexo_lista$ncm <- rep(NA_character_, nrow(anexo_lista))
    }
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
        dplyr::left_join(descricoes_anexo_i, by = "ncm") |>
        dplyr::transmute(
          anexo = anexo_atual,
          nome_anexo = nome_anexo[[anexo_atual]],
          ncm = .data$ncm,
          no_ex = .data$no_ex,
          descricao = .data$descricao_anexo_i,
          descricao_tec_concatenada = .data$descricao_tec_concatenada_anexo_i,
          bkbit = .data$bkbit_anexo_i,
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

  resultado <- if (busca_numerica) {
    resultado |>
      dplyr::filter(
        !is.na(.data$ato),
        stringr::str_detect(.data$ato, stringr::regex(padrao_ato))
      )
  } else {
    resultado |>
      dplyr::filter(
        !is.na(.data$ato),
        stringr::str_detect(
          .data$ato,
          stringr::fixed(ato_pesquisado, ignore_case = TRUE)
        )
      )
  }

  if (nrow(resultado) == 0) {
    warning("Pesquisa n\u00e3o encontrada em atos", call. = FALSE)
    return(resultado_vazio)
  }

  resultado
}

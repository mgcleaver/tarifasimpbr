#' Tabela com todos os ex-tarifarios presentes nos anexos IV a X
#'
#' @description
#' Consolida, em uma unica tabela, os registros de ex-tarifarios lidos por
#' [ler_anexo()] nos anexos IV, V, VI, VIII, IX e X. A identificacao de
#' ex-tarifario e feita a partir da presenca de valor na coluna `no_ex`.
#'
#' @param x Objeto retornado por [download_tarifas()], que no caso e o caminho
#'   temporario do arquivo de tarifas de importacao vigentes baixado.
#' @return
#' Um `tibble` com uma linha por ex-tarifario e, no minimo, as colunas:
#' \describe{
#'   \item{lista}{Nome abreviado da lista de excecao.}
#'   \item{ncm}{Codigo NCM padronizado com 8 digitos.}
#'   \item{descricao}{Descricao do produto associada ao ex.}
#'   \item{no_ex}{Numero do ex padronizado com 3 digitos.}
#'   \item{tarifa_ex}{Aliquota associada ao ex-tarifario.}
#'   \item{quota}{Volume de quota, quando houver.}
#'   \item{unidade_quota}{Unidade da quota, quando houver.}
#'   \item{ato_de_inclusao}{Ato normativo informado no anexo, quando houver.}
#'   \item{data_do_ato_de_inclusao}{Data do ato de inclusao, quando houver.}
#'   \item{inicio_de_vigencia}{Inicio da vigencia do registro.}
#'   \item{termino_de_vigencia}{Termino da vigencia do registro.}
#'   \item{obs}{Observacoes presentes no anexo, quando houver.}
#' }
#'
#' @details
#' A funcao usa [ler_anexo()] como fonte unica de leitura e limpeza dos dados.
#' Em seguida, filtra apenas as linhas com `no_ex` preenchido e padroniza as
#' colunas opcionais para permitir a consolidacao entre anexos com estruturas
#' ligeiramente diferentes.
#'
#' @examples
#' \dontrun{
#' x <- download_tarifas()
#' exs <- ex_tarifarios(x)
#' dplyr::glimpse(exs)
#' }
#'
#' @export
ex_tarifarios <- function(x) {
  anexos <- c("iv", "v", "vi", "viii", "ix", "x")

  adiciona_colunas_ausentes <- function(df) {
    colunas_texto <- c(
      "descricao",
      "lista",
      "aliquota",
      "quota",
      "unidade_quota",
      "ato_de_inclusao",
      "obs"
    )

    for (coluna in setdiff(colunas_texto, names(df))) {
      df[[coluna]] <- NA_character_
    }

    if (!"data_do_ato_de_inclusao" %in% names(df)) {
      df$data_do_ato_de_inclusao <- as.Date(NA)
    }

    df
  }

  purrr::map_dfr(
    anexos,
    function(anexo_atual) {
      anexo <- ler_anexo(x = x, n_anexo = anexo_atual) |>
        adiciona_colunas_ausentes()

      anexo |>
        dplyr::filter(!is.na(.data$no_ex) & trimws(.data$no_ex) != "") |>
        dplyr::transmute(
          lista = .data$lista,
          ncm = .data$ncm,
          descricao = .data$descricao,
          no_ex = .data$no_ex,
          tarifa_ex = .data$aliquota,
          quota = .data$quota,
          unidade_quota = .data$unidade_quota,
          ato_de_inclusao = .data$ato_de_inclusao,
          data_do_ato_de_inclusao = .data$data_do_ato_de_inclusao,
          inicio_de_vigencia = .data$inicio_de_vigencia,
          termino_de_vigencia = .data$termino_de_vigencia,
          obs = .data$obs
        )
    }
  ) |>
    dplyr::arrange(.data$lista, .data$ncm, .data$no_ex, .data$inicio_de_vigencia)
}

# Compatibilidade temporaria enquanto o NAMESPACE antigo ainda exporta este nome.
listar_ex_tarifarios <- ex_tarifarios

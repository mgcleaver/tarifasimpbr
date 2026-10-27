#' Consultar próximos vencimentos das listas de exceção
#'
#' Retorna os registros dos Anexos IV, V, VI, VIII, IX e X cujo
#' `termino_de_vigencia` está no intervalo entre a data da consulta e o fim
#' do mês indicado por `meses`.
#'
#' @param x Caminho de um arquivo de tarifas da Camex existente, como o
#'   retornado por [download_tarifas()].
#' @param meses Número inteiro não negativo de meses à frente. O padrão, `0`,
#'   consulta até o último dia do mês corrente; `1`, até o último dia do mês
#'   seguinte; `2`, até o último dia do mês posterior ao seguinte, e assim
#'   sucessivamente. São meses de calendário, não intervalos de 30 dias.
#' @param incluir_hoje Lógico. Se `TRUE`, inclui registros que vencem hoje.
#'   O padrão, `FALSE`, seleciona apenas términos posteriores a hoje.
#'
#' @details
#' A data de referência é obtida uma única vez com `Sys.Date()`. O último dia
#' do intervalo é sempre incluído. Não há filtro por `inicio_de_vigencia`:
#' registros que ainda vão começar a vigorar também podem ser retornados.
#' Términos ausentes e a data `9999-12-31`, usada para vigência aberta, são
#' excluídos.
#'
#' A função reutiliza a leitura e o cache de [ler_anexo()]. Cada anexo é
#' filtrado antes da consolidação, reduzindo o volume intermediário em memória.
#' Na primeira consulta, as seis abas ainda precisam ser lidas por completo.
#'
#' @return Um `tibble` com todas as colunas fornecidas por [ler_anexo()] e a
#'   coluna `anexo`, com o numeral romano em minúsculas. Colunas inexistentes
#'   em algum anexo recebem `NA`. Os registros são ordenados por término
#'   crescente, preservando a ordem de origem nos empates. Quotas, Ex e
#'   registros repetidos são preservados, sem agregação ou deduplicação.
#'   Sem correspondências, retorna uma tabela vazia com as mesmas colunas,
#'   sem aviso.
#'
#' @seealso [tarifas_futuras()], [tarifas_vigentes()]
#' @examples
#' \dontrun{
#' x <- download_tarifas()
#'
#' # Vencimentos futuros até o fim do mês corrente:
#' fim_vigencia(x)
#'
#' # Até o fim do próximo mês, incluindo vencimentos de hoje:
#' fim_vigencia(x, meses = 1, incluir_hoje = TRUE)
#' }
#'
#' @export
fim_vigencia <- function(x, meses = 0, incluir_hoje = FALSE) {
  if (
    missing(x) || !is.character(x) || length(x) != 1L || is.na(x) ||
      !nzchar(trimws(x)) || !file.exists(x) || dir.exists(x)
  ) {
    stop("x deve ser o caminho de um arquivo existente.", call. = FALSE)
  }

  if (
    !is.numeric(meses) || is.complex(meses) || length(meses) != 1L ||
      !is.finite(meses) || meses < 0 || meses != floor(meses)
  ) {
    stop("meses deve ser um numero inteiro, finito e nao negativo.", call. = FALSE)
  }

  if (
    !is.logical(incluir_hoje) || length(incluir_hoje) != 1L ||
      is.na(incluir_hoje)
  ) {
    stop("incluir_hoje deve ser TRUE ou FALSE.", call. = FALSE)
  }

  hoje <- Sys.Date()
  inicio <- if (incluir_hoje) hoje else hoje + 1
  limite <- suppressWarnings(
    lubridate::floor_date(hoje, unit = "month") +
      lubridate::period(month = meses + 1) - lubridate::days(1)
  )

  if (is.na(limite) || !is.finite(as.numeric(limite)) || limite < hoje) {
    stop("meses produz um limite de data invalido.", call. = FALSE)
  }

  anexos <- c("iv", "v", "vi", "viii", "ix", "x")
  names(anexos) <- anexos

  purrr::map(
    anexos,
    function(anexo) {
      ler_anexo(x = x, n_anexo = anexo) |>
        dplyr::filter(
          .data$termino_de_vigencia != as.Date("9999-12-31"),
          dplyr::between(.data$termino_de_vigencia, .env$inicio, .env$limite)
        )
    }
  ) |>
    dplyr::bind_rows(.id = "anexo") |>
    dplyr::arrange(.data$termino_de_vigencia)
}

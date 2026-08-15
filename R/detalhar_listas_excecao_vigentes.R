#' Consolida os codigos NCM em listas de excecao tarifaria e adiciona colunas de
#' contagem de quota e de destaque (Ex) bem como uma coluna para indicar a presença
#' de NCM integral.
#'
#' @description
#' Itera sobre as seguintes listas de excecao tarifaria: desabastecimento
#' (\strong{IV}), Letec (\strong{V}), Lebitbk (\strong{VI}), Concessoes OMC
#' (\strong{VIII}), DCC (\strong{IX}) e Automotivos ACE-14 (\strong{X}) e produz
#' um resumo contendo todos os codigos NCM vigentes nessas listas com contagem
#' de quota e de destaques (Ex), indicador de NCM integral, lista(s) de excecao
#' e tarifa aplicada quando uma NCM for incluída integralmente em uma lista sem
#' que seja uma quota.
#'
#' @details
#' A funcao aplica, para cada anexo especificado, a combinacao de
#' `adiciona_indicador_ex_quota` e `tarifas_vigentes` para:
#'
#' * ler e formatar o anexo correspondente;
#' * adicionar indicadores de quota, destaque (Ex) e NCM integral; e
#' * filtrar apenas os registros vigentes na data de execucao (`Sys.Date()`).
#'
#' Os anexos tambem sao processados para obter as NCMs incluidas integralmente
#' em listas de excecao com suas respectivas tarifas aplicadas, datas de
#' vigencia e ato de inclusao.
#'
#' Em seguida, os resultados de cada anexo sao agregados por NCM e consolidados
#' em um unico `tibble`/`data.frame`. Por fim, sao adicionados dados de tarifas
#' aplicadas a partir das listas de excecao, produzindo:
#'
#' * o numero de ocorrencias com quota por NCM na lista de excecao especificada;
#' * o numero de destaques (Ex) por NCM na lista de excecao especificada;
#' * o indicador binario (0/1) informando se ha medida que abrange a NCM
#'   integralmente (sem destaque);
#' * a coluna `lista`, que mantem a(s) lista(s) de excecao em que a NCM aparece;
#' * a coluna `tarifa_aplicada` com dados apenas para as ncms incluidas
#'   integralmente em listas de excecao. Pode ocorrer de uma NCM estar presente
#'   em mais de uma lista de excecao, mas, nesse caso, em regra, a NCM aparece
#'   na lista de desabastecimento e em outra lista ou na lista de concessoes
#'   da OMC e em outra lista. Como normalmente os codigos presentes na lista de
#'   desabastecimento e de concessoes da OMC nao sao incluidos nessas
#'   listas integralmente, nesses casos de uma NCM fazer parte de mais de uma lista,
#'   a tarifa aplicada sera, provavelmente, referente a outra lista remanescente;
#' * as colunas `inicio_de_vigencia`, `termino_de_vigencia`,
#'   `ato_de_inclusao` e `data_do_ato_de_inclusao`, preenchidas apenas quando
#'   houver alteracao tarifaria vigente para a integralidade da NCM, sem quota
#'   e sem destaque (Ex). Nos demais casos, inclusive quando houver somente
#'   ex-tarifarios, essas colunas permanecem `NA`.
#'
#' Se os dados de origem contiverem mais de um registro integral vigente para
#' a mesma NCM, a funcao interrompe o processamento, pois nao e possivel
#' representar tarifas e metadados conflitantes em uma unica linha.
#'
#' @param x deve ser o resultado da funcao `download_tarifas()`.
#'
#' @return um `tibble` (ou `data.frame`) com uma linha por NCM e as colunas:
#'
#' * `ncm`: codigo NCM;
#' * `contagem_quota`: numero de ocorrencias com quota associadas a NCM nas
#'   listas de excecao vigentes;
#' * `contagem_ex`: numero de ocorrencias com destaque (Ex) associadas a NCM
#'   nas listas de excecao vigentes;
#' * `ncm_integral`: indicador binario (0/1) informando se existe medida em lista
#'   de exceção que abrange a NCM integralmente (sem destaque);
#' * `lista`: string com a(s) lista(s) de excecao em que a NCM aparece,
#'   concatenadas com virgula quando houver mais de uma.
#' * `tarifa_aplicada`: apresenta a tarifa aplicada para as ncms incluidas
#'   integralmente em lista de excecao;
#' * `inicio_de_vigencia`, `termino_de_vigencia`, `ato_de_inclusao` e
#'   `data_do_ato_de_inclusao`: metadados do registro que altera a tarifa para
#'   a integralidade da NCM. Permanecem `NA` quando nao houver esse registro.
#'
#' @examples
#' \dontrun{
#' # Exemplo de uso:
#' x <- download_tarifas()
#' resumo_excecoes <- detalhar_listas_excecao_vigentes(x)
#' dplyr::glimpse(resumo_excecoes)
#' }
#'
#' @export
detalhar_listas_excecao_vigentes <- function(x) {
  anexos <- c("iv", "v", "vi", "viii", "ix", "x")

  listas_detalhadas_vigentes <- purrr::map(
    anexos,
    ~ adiciona_indicador_ex_quota(x = x, n_anexo = .x) |>
      tarifas_vigentes()
  )

  ncms_integrais <- purrr::map(
    listas_detalhadas_vigentes,
    ~ seleciona_tarifas(.x)
  ) |>
    dplyr::bind_rows(
      tibble::tibble(
        inicio_de_vigencia = as.Date(character()),
        termino_de_vigencia = as.Date(character()),
        ato_de_inclusao = character(),
        data_do_ato_de_inclusao = as.Date(character())
      )
    )

  ncms_integrais_duplicadas <- ncms_integrais |>
    dplyr::count(.data$ncm, name = "numero_registros") |>
    dplyr::filter(.data$numero_registros > 1) |>
    dplyr::pull(.data$ncm)

  if (length(ncms_integrais_duplicadas) > 0) {
    stop(
      paste0(
        "Os dados de origem contem mais de um registro integral vigente ",
        "para a mesma NCM: ",
        paste(ncms_integrais_duplicadas, collapse = ", "),
        "."
      ),
      call. = FALSE
    )
  }

  ncms_integrais <- ncms_integrais |>
    dplyr::transmute(
      .data$ncm,
      tarifa_aplicada = as.numeric(.data$aliquota),
      inicio_de_vigencia = .data$inicio_de_vigencia,
      termino_de_vigencia = .data$termino_de_vigencia,
      ato_de_inclusao = .data$ato_de_inclusao,
      data_do_ato_de_inclusao = .data$data_do_ato_de_inclusao
    )

  resumo_tarifas_excecao <- purrr::map(
    listas_detalhadas_vigentes,
    ~ resume_tarifas_de_excecao(.x)
  ) |>
    dplyr::bind_rows() |>
    dplyr::group_by(.data$ncm) |>
    dplyr::summarise(
      contagem_quota = sum(.data$contagem_quota),
      contagem_ex = sum(.data$contagem_ex),
      ncm_integral = as.integer(any(.data$ncm_integral)),
      lista = {
        n_lista <- unique(.data$lista)
        paste0(n_lista, collapse = ", ")
      }
    ) |>
    dplyr::left_join(
      ncms_integrais,
      by = "ncm"
    )

  return(resumo_tarifas_excecao)
}

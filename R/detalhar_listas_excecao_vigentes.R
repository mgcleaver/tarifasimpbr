#' Traz os códigos NCM incluídos em listas de exceção tarifárias com indicadores
#' de contagem de quota e de destaques tarifário, bem como indicadores de presença
#' de quota, de destaques tarifários e de presença integral da ncm na lista de exceção.
#'
#' @description
#' Itera sobre as seguintes listas de exceções tarifárias: desabastecimento
#' (\strong{IV}), Letec (\strong{V}), Lebitbk (\strong{VI}), Concessões OMC
#' (\strong{VIII}), DCC (\strong{IX}) e Automotivos ACE-14 (\strong{X}) e produz
#' um resumo contendo todos os códigos NCM vigentes nessas listas com contagem
#' de quota e de destaques (Ex), bem como indicadores de presença de quota, de
#' destaque e de inclusão da integralidade da NCM em lista de exceção.
#'
#' @details
#' A função aplica, para cada anexo especificado, a combinação de
#' `adiciona_indicador_ex_quota` e `tarifas_vigentes` para:
#'
#' * ler e formatar o anexo correspondente;
#' * adicionar indicadores de quota, destaque (Ex) e NCM integral; e
#' * filtrar apenas os registros vigentes na data de execução (`Sys.Date()`).
#'
#' Os anexos também são processados para obter as ncms incluídas integralmente
#' em listas de exceção com suas respectivas tarifas aplicadas.
#'
#' Em seguida, os resultados de cada anexo são agregados por NCM e consolidados
#' em um único `tibble`/`data.frame`. Por fim, são adicionados dados de tarifas
#' aplicadas para ncms integrais, produzindo:
#'
#' * o número de ocorrências com quota por NCM;
#' * o número de destaques (Ex) por NCM;
#' * indicadores binários (0/1) informando se a NCM possui ao menos uma quota,
#'   ao menos um destaque (Ex) ou se há medida que abrange a NCM integralmente; e
#' * a coluna `lista`, que mantém a(s) lista(s) de exceção em que a NCM aparece;
#' * a coluna `tarifa_aplicada` com dados apenas para as ncms incluídas
#'   integralmente em listas de exceção. Pode ocorrer de uma NCM estar presente
#'   em mais de uma lista de exceção, mas, nesse caso, em regra, a NCM aparece
#'   na lista de desabastecimento e em outra lista ou na lista de concessões
#'   da OMC e em outra lista. Como normalmente os códigos presentes na lista de
#'   desabastecimento e na lista de concessões da OMC não são incluídos nessas
#'   listas integralmente, nesses casos de uma NCM fazer parte de mais de uma lista
#'   a tarifa aplicada será, provavelmente, referente à lista remanescente.
#'
#' @param x deve ser o resultado da função `download_tarifas()`.
#'
#' @return um `tibble` (ou `data.frame`) com uma linha por NCM e as colunas:
#'
#' * `ncm`: código NCM;
#' * `contagem_quota`: número de ocorrências com quota (`quota == 1`)
#'   associadas à NCM nas listas de exceção vigentes;
#' * `contagem_ex`: número de destaques (Ex) (`destaque_ex == 1`)
#'   associados à NCM nas listas de exceção vigentes;
#' * `quota`: indicador binário (0/1) informando se a NCM possui ao menos uma
#'   ocorrência com quota;
#' * `destaque_ex`: indicador binário (0/1) informando se a NCM possui ao menos
#'   um destaque (Ex) nas listas de exceção;
#' * `ncm_integral`: indicador binário (0/1) informando se há medida que
#'   abrange a NCM integralmente (sem destaque); e
#' * `lista`: string com a(s) lista(s) de exceção em que a NCM aparece,
#'   concatenadas com vírgula quando houver mais de uma.
#' * `tarifa_aplicada`: apresenta a tarifa aplicada para as ncms incluídas
#'   integralmente em lista de exceção.
#'
#' @examples
#' # Exemplo de uso:
#' x <- download_tarifas()
#' resumo_excecoes <- detalhar_listas_excecao_vigentes(x)
#' dplyr::glimpse(resumo_excecoes)
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
    dplyr::bind_rows() |>
    dplyr::transmute(
      .data$ncm,
      tarifa_aplicada = as.numeric(.data$aliquota)
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

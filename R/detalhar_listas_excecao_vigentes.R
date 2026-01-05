#' Traz os códigos NCM incluídos em listas de exceção tarifárias com indicadores
#' de contagem, de cota, de destaque e de presença integral da ncm.
#'
#' @description
#' Itera sobre as seguintes listas de exceções tarifárias: desabastecimento
#' (\strong{IV}), Letec (\strong{V}), Lebitbk (\strong{VI}), Concessões OMC
#' (\strong{VIII}), DCC (\strong{IX}) e Automotivos ACE-14 (\strong{X}) e produz
#' um resumo contendo todos os códigos NCM vigentes nessas listas com contagem
#' de cota e de destaques (Ex), bem como indicadores de presença de cota, de
#' destaque e de inclusão da integralidade da NCM em lista de exceção.
#'
#' @details
#' A função aplica, para cada anexo especificado, a combinação de
#' `adiciona_indicador_ex_cota` e `tarifas_vigentes` para:
#'
#' * ler e formatar o anexo correspondente;
#' * adicionar indicadores de cota, destaque (Ex) e NCM integral; e
#' * filtrar apenas os registros vigentes na data de execução (`Sys.Date()`).
#'
#' Os anexos também são processados para obter as ncms incluídas integralmente
#' em listas de exceção com suas respectivas tarifas aplicadas.
#'
#' Em seguida, os resultados de cada anexo são agregados por NCM e consolidados
#' em um único `tibble`/`data.frame`. Por fim, são adicionados dados de tarifas
#' aplicadas para ncms integrais, produzindo:
#'
#' * o número de ocorrências com cota por NCM;
#' * o número de destaques (Ex) por NCM;
#' * indicadores binários (0/1) informando se a NCM possui ao menos uma cota,
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
#' @return
#' Um `tibble` (ou `data.frame`) com uma linha por NCM e as colunas:
#'
#' * `ncm`: código NCM;
#' * `contagem_cota`: número de ocorrências com cota (`cota == 1`)
#'   associadas à NCM nas listas de exceção vigentes;
#' * `contagem_ex`: número de destaques (Ex) (`destaque_ex == 1`)
#'   associados à NCM nas listas de exceção vigentes;
#' * `cota`: indicador binário (0/1) informando se a NCM possui ao menos uma
#'   ocorrência com cota;
#' * `destaque_ex`: indicador binário (0/1) informando se a NCM possui ao menos
#'   um destaque (Ex) nas listas de exceção;
#' * `ncm_integral`: indicador binário (0/1) informando se há medida que
#'   abrange a NCM integralmente (sem cota nem destaque); e
#' * `lista`: string com a(s) lista(s) de exceção em que a NCM aparece,
#'   concatenadas com vírgula quando houver mais de uma.
#' * `tarifa_aplicada`: apresenta a tarifa aplicada para as ncms incluídas
#'   integralmente em lista de exceção.
#'
#' @examples
#' \dontrun{
#' # Exemplo hipotético de uso:
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
    ~ adiciona_indicador_ex_cota(x = x, n_anexo = .x) |>
      tarifas_vigentes()
  )

  ncms_integrais <- purrr::map(
    listas_detalhadas_vigentes,
    ~ seleciona_tarifas(.x)
  ) |>
    dplyr::bind_rows() |>
    dplyr::transmute(ncm, tarifa_aplicada = as.numeric(aliquota_percent))

  resumo_tarifas_excecao <- purrr::map(
    listas_detalhadas_vigentes,
    ~ resume_tarifas_de_excecao(.x)
  ) |>
    dplyr::bind_rows() |>
    dplyr::group_by(ncm) |>
    dplyr::summarise(
      contagem_cota = sum(contagem_cota),
      contagem_ex = sum(contagem_ex),
      cota = as.integer(any(cota)),
      destaque_ex = as.integer(any(destaque_ex)),
      ncm_integral = as.integer(any(ncm_integral)),
      lista = {
        n_lista <- unique(lista)
        paste0(n_lista, collapse = ", ")
      }) |>
    dplyr::left_join(
      ncms_integrais,
      by = "ncm"
    )

  return(resumo_tarifas_excecao)
}

#' Adiciona indicador de destaque (ex) e cota
#'
#' Usada dentro da função principal `detalhar_listas_excecao_vigentes` para
#' adicionar um indicador de destaque (ex) de cota e de presença integral de uma
#' NCM nos anexo iv, v, vi, xiii, ix, x.
#'
#' @param x deve ser o resultado de `tarifas_download()`. No caso o argumento
#'    é passado para esta função interna por meio do argumento x proveniente de
#'    `detalhar_listas_excecao_vigentes`.
#'
#' @param n_anexo é o número romano de um anexo relacionado com uma lista de
#'    exceção. As opções possíveis são iv, v, vi, viii, ix, x.
#'
#' @return
#' O mesmo objeto de entrada, porém com três colunas adicionais: cota, destaque_ex
#' e ncm_integral.
#'
#' @keywords internal
#' @noRd
adiciona_indicador_ex_cota <- function(
    x,
    n_anexo = c("iv", "v", "vi", "viii", "ix", "x")) {

  n_anexo <- match.arg(
    n_anexo
  )

  suprime_texto <- function(y) {
    suppressMessages(
      suppressWarnings(
        y
      )
    )
  }

  out <- suprime_texto(
    ler_anexo_formatado(x = x, n_anexo = n_anexo) |>
      dplyr::mutate(
        quota = as.numeric(quota),
        no_ex = as.numeric(no_ex)
      )  |>
      dplyr::mutate(
        cota = dplyr::if_else(is.na(quota), 0, 1),
        destaque_ex = dplyr::if_else(is.na(no_ex), 0, 1),
        ncm_integral = dplyr::if_else(
          is.na(quota) & is.na(no_ex),
          1,
          0))
  )

  return(out)

}

#' Resume informações tarifárias a partir dos indicadores de destaque e cota
#'
#' Usada dentro da função principal `detalhar_listas_excecao_vigentes`.
#'
#' @keywords internal
#' @noRd
resume_tarifas_de_excecao <- function(x) {
  out <- x |>
    dplyr::group_by(ncm, lista) |>
    dplyr::summarise(
      contagem_cota = sum(cota == 1),
      contagem_ex = sum(destaque_ex == 1),
      cota = any(cota == 1),
      destaque_ex = any(destaque_ex == 1),
      ncm_integral = any(ncm_integral == 1),
      .groups = 'drop')

  return(out)
}

#' Seleciona colunas específicas das listas de exceções tarifárias
#'
#' Usada dentro da função principal `detalhar_listas_excecao_vigentes`.
#'
#' @keywords internal
#' @noRd
seleciona_tarifas <- function(
    x) {

  padrao <- paste(
    "^ncm$",
    "aliquota_percent",
    "inicio_de_vigencia",
    "termino_de_vigencia",
    "ato_de_inclusao",
    "data_do_ato_de_inclusao",
    "lista",
    sep = "|"
  )

  out <- x |>
    dplyr::filter(ncm_integral == 1) |>
    dplyr::select(dplyr::matches(padrao))

  return(out)
}

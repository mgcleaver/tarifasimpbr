#' Processa os anexos tarifários e a partir deles gera uma tabela contendo
#' as tarifas aplicadas
#'
#' @description
#' Combina as informações dos anexos I, II, IV, V, VI, VIII, IX, X para calcular
#'  a \emph{tarifa aplicada} por código NCM. Opcionalmente,
#' pode retornar um resultado detalhado por NCM com contagem de quota e de
#' destaque (Ex), além de indicar se a NCM se encontra integralmente em uma lista
#' de exceção.
#'
#' @details
#' A função:
#'
#' 1. Lê o anexo I por meio de [ler_anexo()] com `n_anexo = "i"`,
#'    removendo a coluna `resolucoes`;
#' 2. Lê o anexo II por meio de [ler_anexo()] com `n_anexo = "ii"`,
#'    obtendo a Tarifa Externa Brasileira (`aliquota_aplicada`)
#'    como coluna numérica `teb`;
#' 3. Processa os demais anexos de exceção por meio de
#'    [detalhar_listas_excecao_vigentes()], obtendo, entre outros, a coluna
#'    `tarifa_aplicada` para NCM incluídas integralmente em listas de exceção;
#' 4. Calcula a coluna `tarifa_aplicada` e prioriza, nessa ordem, a tarifa das
#'    listas de exceção, a TEB e, na ausência dessas, a alíquota TEC (`tec`);
#' 5. Ajusta a coluna `teb` para assumir a `tec` quando a TEB estiver
#'    ausente.
#'
#' Quando `detalhar = FALSE` (padrão), a função retorna apenas as colunas
#' necessárias para identificar o NCM, a descrição, a lista de exceção
#' (quando aplicável), a TEB e a tarifa aplicada.
#'
#' Quando `detalhar = TRUE`, a função amplia o resultado com colunas adicionais
#' derivadas de [detalhar_listas_excecao_vigentes()], incluindo:
#'
#' * `contagem_quota`: número de ocorrências com quota;
#' * `contagem_ex`: número de destaques (Ex);
#' * `ncm_integral`: indicador (0/1) de medida e lista de exceção
#'    aplicável à NCM integralmente.
#'
#' @param x Objeto, é o resultado obtido a partir da função `download_tarifas()`
#' @param detalhar Lógico, indica se o resultado deve incluir as colunas
#'   detalhadas das listas de exceção. O padrão é `FALSE`. Se `TRUE`,
#'   retorna indicadores de contagem, de quota, de destaque (Ex) e de NCM integral.
#'
#' @return Um `tibble` (ou `data.frame`) com, no mínimo, as colunas:
#'
#' * `ncm`: código NCM;
#' * `descricao_tec`: descrição da NCM (descrição no nível de oito dígitos)
#' * `descricao_tec_concatenada`: descrição da NCM a partir da posição;
#' * `bkbit`: indica se a NCM é grafada como bem de capital/bem de informática
#' * `lista`: lista de exceção associada a uma NCM (quando aplicável);
#' * `tec`: alíquota da Tarifa Externa Comum;
#' * `teb`: alíquota TEB aplicável;
#' * `tarifa_aplicada`: A tarifa aplicada é o que consta na TEB, mas se estiver
#'    integralmente em uma lista de exceção, a tarifa aplicada será a que consta
#'    na lista de exceção.
#'
#' Quando `detalhar = TRUE`, o retorno inclui também:
#'
#' * `contagem_quota`, `contagem_ex`, `ncm_integral`,
#'   conforme descrito em [detalhar_listas_excecao_vigentes()].
#'
#' @seealso
#' [ler_anexo()], [detalhar_listas_excecao_vigentes()],
#' [tarifas_vigentes()].
#'
#' @examples
#' # Resultado enxuto, apenas com tarifa aplicada e TEB:
#' x <- download_tarifas()
#' tarifas <- tarifas_aplicadas(x)
#' dplyr::glimpse(tarifas)
#'
#' # Resultado detalhado, com informações, por NCM, de contagem de quota, de Ex
#' e indicador de NCM integral:
#' tarifas_detalhadas <- tarifas_aplicadas(x, detalhar = TRUE)
#' dplyr::glimpse(tarifas_detalhadas)
#'
#' @export
tarifas_aplicadas <- function(x, detalhar = FALSE) {

  anexoi <- ler_anexo(x, "i") |>
    dplyr::select(
      -resolucoes
    )

  anexoii <- ler_anexo(x, "ii") |>
    dplyr::transmute(.data$ncm, teb = as.numeric(.data$aliquota_aplicada))

  message("Processando demais anexos...")
  listas_excecao <- detalhar_listas_excecao_vigentes(x)

  base <- anexoi |>
    dplyr::left_join(anexoii, by = "ncm") |>
    dplyr::left_join(
      listas_excecao,
      by = "ncm"
    ) |>
    dplyr::mutate(
      tarifa_aplicada = dplyr::coalesce(.data$tarifa_aplicada, .data$teb, .data$tec),
      teb = dplyr::coalesce(.data$teb, .data$tec))

  message("Tarifas aplicadas geradas com sucesso")

  if (!detalhar) {
    return(
      base |>
        dplyr::select(
          ncm:descricao_tec_concatenada,
          bkbit,
          lista,
          tec,
          teb,
          tarifa_aplicada
        )
    )
  }

  out <- base |>
    dplyr::mutate(
      dplyr::across(
        dplyr::where(is.numeric),
        ~ tidyr::replace_na(.x, 0)
      )
    ) |>
    dplyr::select(
      ncm:descricao_tec_concatenada,
      bkbit,
      lista,
      tec,
      teb,
      tarifa_aplicada,
      contagem_quota,
      contagem_ex,
      ncm_integral
    )

  return(out)
}

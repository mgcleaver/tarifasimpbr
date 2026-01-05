#' Processa os anexos tarifários e a partir deles gera uma tabela contendo
#' as tarifas aplicadas
#'
#' @description
#' Combina as informações dos anexos I, II, IV, V, VI, VIII, IX, X para calcular
#'  a \emph{tarifa aplicada} por código NCM. Opcionalmente,
#' pode retornar um resultado detalhado com indicadores de quota, destaque (Ex)
#' e presença da integralidade da NCM em uma lista de exceção.
#'
#' @details
#' A função:
#'
#' 1. Lê o anexo I por meio de [ler_anexo_formatado()] com `n_anexo = "i"`,
#'    removendo a coluna `resolucoes`;
#' 2. Lê o anexo II por meio de [ler_anexo_formatado()] com `n_anexo = "ii"`,
#'    obtendo a Tarifa Externa Brasileira (`aliquota_aplicada_percent`)
#'    como coluna numérica `teb`;
#' 3. Processa os demais anexos de exceção por meio de
#'    [detalhar_listas_excecao_vigentes()], obtendo, entre outros, a coluna
#'    `tarifa_aplicada` para NCM incluídas integralmente em listas de exceção;
#' 4. Calcula a coluna `tarifa_aplicada` e prioriza, nessa ordem, a tarifa das
#'    listas de exceção, a TEB e, na ausência dessas, a alíquota TEC (`tec_percent`);
#' 5. Ajusta a coluna `teb` para assumir a `tec_percent` quando a TEB estiver
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
#' * `quota`: indicador (0/1) de existência de quota para a NCM;
#' * `destaque_ex`: indicador (0/1) de existência de destaque (Ex);
#' * `ncm_integral`: indicador (0/1) de medida aplicável à NCM integralmente.
#'
#' @param x Objeto, é o resultado obtido a partir da função `download_tarifas()`
#' @param detalhar Lógico, indica se o resultado deve incluir as colunas
#'   detalhadas das listas de exceção. O padrão é `FALSE`. Se `TRUE`,
#'   retorna indicadores de contagem, de quota, de destaque (Ex) e de NCM integral.
#'
#' @return Um `tibble` (ou `data.frame`) com, no mínimo, as colunas:
#'
#' * `ncm`: código NCM;
#' * `descricao_tec_concatenada`: descrição associada à NCM;
#' * `bkbit`: indicador de bem de capital/bem de informática, quando disponível;
#' * `lista`: lista de exceção associada (quando aplicável);
#*  * `teb`: alíquota TEB aplicável (ou TEC, quando a TEB estiver ausente);
#' * `tarifa_aplicada`: alíquota efetivamente aplicada após considerar TEC,
#'   TEB e listas de exceção.
#'
#' Quando `detalhar = TRUE`, o retorno inclui também:
#'
#' * `contagem_quota`, `contagem_ex`, `quota`, `destaque_ex`, `ncm_integral`,
#'   conforme descrito em [detalhar_listas_excecao_vigentes()].
#'
#' @seealso
#' [ler_anexo_formatado()], [detalhar_listas_excecao_vigentes()],
#' [tarifas_vigentes()].
#'
#' @examples
#' \dontrun{
#' # Resultado enxuto, apenas com tarifa aplicada e TEB:
#' x <- download_tarifas()
#' tarifas <- tarifas_aplicadas(x)
#' dplyr::glimpse(tarifas)
#'
#' # Resultado detalhado, com informações de quota, Ex e NCM integral:
#' tarifas_detalhadas <- tarifas_aplicadas(x, detalhar = TRUE)
#' dplyr::glimpse(tarifas_detalhadas)
#' }
#'
#' @export
tarifas_aplicadas <- function(x, detalhar = FALSE) {

  anexoi <- ler_anexo_formatado(x, "i") |>
    dplyr::select(
      -resolucoes
    )

  anexoii <- ler_anexo_formatado(x, "ii") |>
    dplyr::transmute(ncm, teb = as.numeric(aliquota_aplicada_percent))

  message("Processando demais anexos...")
  listas_excecao <- detalhar_listas_excecao_vigentes(x)

  base <- anexoi |>
    dplyr::left_join(anexoii, by = "ncm") |>
    dplyr::left_join(
      listas_excecao,
      by = "ncm"
    ) |>
    dplyr::mutate(
      tarifa_aplicada = dplyr::coalesce(tarifa_aplicada, teb, tec_percent),
      teb = dplyr::coalesce(teb, tec_percent))

  if (!detalhar) {
    return(
      base |>
        dplyr::select(
          ncm:descricao_tec_concatenada,
          bkbit,
          lista,
          teb,
          tarifa_aplicada
        )
    )
  }

  out <- base |>
    dplyr::mutate(
      dplyr::across(
        where(is.numeric),
        ~ tidyr::replace_na(.x, 0)
      )
    ) |>
    dplyr::select(
      ncm:descricao_tec_concatenada,
      bkbit,
      lista,
      teb,
      tarifa_aplicada,
      contagem_quota,
      contagem_ex,
      quota,
      destaque_ex,
      ncm_integral
    )

    return(out)
  }

#' Ler anexos do arquivo excel obtido por meio da funcao `download_tarifas`.
#'
#' Le o anexo desejado do arquivo de tarifas obtido por meio da funcao
#' `download_tarifas`. Para cada anexo, a funcao `ler_anexo` limpa
#' e organiza os dados de cada aba do arquivo (anexo).
#'
#' Para o Anexo I, a funcao `ler_anexo` apenas delega o processamento para
#' a funcao `ler_anexo1`. Para os demais anexos suportados, a funcao:
#' \itemize{
#'   \item identifica a aba correta com base no nome do anexo;
#'   \item pula linhas de cabecalho conforme o tipo de anexo;
#'   \item padroniza nomes de colunas com `janitor::clean_names` e a partir de outras regras;
#'   \item garante a existencia da coluna de termino_de_vigencia e renomeia
#'     inicio_da_vigencia para inicio_de_vigencia quando for o caso;
#'   \item formata datas com a funcao auxiliar `formata_datas`;
#'   \item padroniza codigos \code{ncm} e \code{no_ex}
#' }
#'
#' @param x Objeto retornado por `download_tarifas`, que no caso e o caminho
#' temporario do arquivo de tarifas de importacao vigentes baixado.
#' @param n_anexo String indicando qual anexo deve ser lido. Deve ser
#'   um dos:
#'   \code{"i"}, \code{"ii"}, \code{"iv"}, \code{"v"},
#'   \code{"vi"}, \code{"viii"}, \code{"ix"}, \code{"x"}.
#'
#' @return
#' Para \code{n_anexo = "i"}, retorna o tibble produzido por `ler_anexo1`.
#'
#' Para os demais anexos, retorna um tibble com, no minimo, as colunas:
#' \describe{
#'   \item{ncm}{Codigo NCM, como texto.}
#'   \item{no_ex}{Numero do \emph{ex} padronizado com 3 digitos}
#'   \item{inicio_de_vigencia}{Data de inicio de vigencia em formato
#'     padronizado, conforme `formata_datas`.}
#'   \item{termino_de_vigencia}{Data de termino de vigencia em formato
#'     padronizado; quando ausente no arquivo, e preenchida com
#'     \code{"9999-12-31"}.}
#'   \item{data_do_ato_de_inclusao}{(Quando existente) data do ato de inclusao,
#'     tambem formatada por `formata_datas`.}
#'   \item{lista}{(Coluna criada para indicar a presença de uma NCM nos anexos
#'   IV (Desabastecimento), V (Letec), VI (Lebitbk), VIII (Concessoes OMC), IX
#'   (DCC) e X (Automotivo))}
#' }
#' Outras colunas presentes no anexo original sao mantidas, podendo variar
#' conforme o tipo de anexo.
#'
#' @details
#' A selecao da aba e feita com base nos nomes das planilhas do arquivo Excel,
#' convertidos para minusculas, procurando um padrao que contenha o numero
#' romano do anexo (por exemplo, \code{" iv "} para o Anexo IV).
#'
#' Para o Anexo VI, caso exista uma coluna \code{x10}, ela e removida
#' automaticamente antes do processamento.
#'
#' Quando nao ha coluna de termino de vigencia no anexo, a funcao cria
#' a coluna \code{termino_de_vigencia} e preenche dados faltantes com a data
#' "9999-12-31".
#'
#' Se existir a coluna \code{inicio_da_vigencia}, ela e renomeada para
#' \code{inicio_de_vigencia}. Caso contrario, a funcao espera que
#' \code{inicio_de_vigencia} ja exista no anexo.
#'
#' @section Anexos suportados:
#' \itemize{
#'   \item \code{"i"}: TEC vigente
#'   \item \code{"ii"}: le a aba do Anexo II
#'   \item \code{"iv"}: le o Anexo IV - Desabastecimento.
#'   \item \code{"v"}: le o Anexo V - Letec.
#'   \item \code{"vi"}: le o Anexo VI - Lebitbk.
#'   \item \code{"viii"}: le o Anexo VIII - Concessoes OMC.
#'   \item \code{"ix"}: le o Anexo IX - DCC.
#'   \item \code{"x"}: le o Anexo X - Automotivo.
#' }
#'
#' @seealso
#'   `download_tarifas` para obtencao do arquivo de tarifas,
#'   `ler_anexo1` para o processamento especifico do Anexo I
#'   e `formata_datas` para limpeza e padronizacao de datas.
#'
#' @examples
#' x <- download_tarifas()
#'
#' # Ler Anexo I
#' anexo_i <- ler_anexo(x, n_anexo = "i")
#'
#' # Ler Anexo V (Letec) ja com datas e NCM padronizados
#' anexo_v <- ler_anexo(x, n_anexo = "v")
#'
#' @export
ler_anexo <- function(
    x,
    n_anexo = c("i", "ii", "iv", "v", "vi", "viii", "ix", "x")
) {

  n_anexo <- match.arg(
    n_anexo
  )

  n_anexo_lower <- paste0(
    " ",
    n_anexo,
    " "
  )

  # regra para definir se coluna "lista" sera incluida:
  # se a consulta for para anexo i ou ii, is_lista = 0
  is_lista <- 0

  # caso a consulta seja para os demais anexos, is_lista = 1
  if(n_anexo_lower %in% c(" iv ", " v ", " vi ", " viii ", " ix ", " x ")) {
    tem_lista <- 1
  }

  nome_abas <- readxl::excel_sheets(x) |>
    stringr::str_to_lower()

  selecao_aba <- nome_abas |>
    stringr::str_subset(n_anexo_lower)

  numero_aba <- which(nome_abas == selecao_aba)

  nome <- paste0("Anexo_", n_anexo)
  condicao6 <- FALSE

  nome_lista <- listar_anexos() |>
    dplyr::filter(.data$numero_anexo == n_anexo) |>
    dplyr::pull(.data$nome_abreviado_anexo)

  # configuracao da leitura do anexo a partir da aba
  if(stringr::str_detect(n_anexo_lower, " i ")) {
    return(ler_anexo1(x))
  } else if(stringr::str_detect(n_anexo_lower, " ii ")) {
    message("Processando Anexo II...")
    pula_linhas <- obter_linha_cabecalho(x, numero_aba)
    return(
      readxl::read_excel(
        x,
        sheet = numero_aba,
        skip = pula_linhas,
        guess_max = 1e6,
        col_types = c("text")) |>
        janitor::clean_names() |>
        dplyr::rename(
          tec = .data$tec_percent,
          aliquota_aplicada = .data$aliquota_aplicada_percent
        ) |>
        dplyr::mutate(ncm = stringr::str_replace_all(.data$ncm, "\\.", ""))
    )
  } else if (stringr::str_detect(n_anexo_lower, " iv ")) {
    message("Processando Anexo IV - Desabastecimento...")
    pula_linhas <- obter_linha_cabecalho(x, numero_aba) # 4
  } else if (stringr::str_detect(n_anexo_lower, " v ")) {
    message("Processando Anexo V - Letec...")
    pula_linhas <- obter_linha_cabecalho(x, numero_aba) # 4
  } else if (stringr::str_detect(n_anexo_lower, " vi ")) {
    message("Processando Anexo VI - Lebitbk...")
    pula_linhas <- obter_linha_cabecalho(x, numero_aba) # 3
    condicao6 <- TRUE
  } else if (stringr::str_detect(n_anexo_lower, " viii ")) {
    message("Processando Anexo VIII - Concessoes OMC...")
    pula_linhas <- obter_linha_cabecalho(x, numero_aba) # 3
  } else if (stringr::str_detect(n_anexo_lower, " ix ")) {
    message("Processando Anexo IX - DCC...")
    pula_linhas <- obter_linha_cabecalho(x, numero_aba) # 3
  } else if (stringr::str_detect(n_anexo_lower, " x ")) {
    message("Processando Anexo X - Automotivo...")
    pula_linhas <- obter_linha_cabecalho(x, numero_aba) # 4
  }

  anexo <- readxl::read_excel(
    x,
    sheet = numero_aba,
    skip = pula_linhas,
    guess_max = 1e6,
    col_types = c("text")) |>
    janitor::clean_names()

  if(condicao6 && "x10" %in% names(anexo)) {
    anexo <- anexo |>
      dplyr::select(-.data$x10)
  }

  if ("aliquota_percent" %in% names(anexo)) {
    anexo <- anexo |>
      dplyr::rename(aliquota = .data$aliquota_percent)
  }

  nomes_colunas <- names(anexo)

  termino_vigencia <- stringr::str_subset(nomes_colunas, "termino_de_vigencia")

  processa_data_inclusao <- FALSE

  if("data_do_ato_de_inclusao" %in% nomes_colunas) {
    processa_data_inclusao <- TRUE
  }

  inicio <- stringr::str_subset(nomes_colunas, "inicio_da_vigencia")

  if(!purrr::is_empty(inicio)) {
    anexo <- anexo |>
      dplyr::rename(inicio_de_vigencia = .data$inicio_da_vigencia)
  }

  if(purrr::is_empty(termino_vigencia)) {

    if (n_anexo_lower == " viii ") {
      anexo <- anexo |>
        dplyr::mutate(termino_de_vigencia = as.Date("9999-12-31"))
    } else {
      anexo <- anexo |>
        dplyr::mutate(termino_de_vigencia = NA)
    }

    anexo <- anexo |>
      dplyr::relocate(.data$termino_de_vigencia, .after = .data$inicio_de_vigencia)
  }



  obs <- stringr::str_subset(nomes_colunas, "obs")

  if(!purrr::is_empty(obs)) {
    anexo <- anexo |>
      dplyr::rename(obs = dplyr::all_of(obs)) |>
      dplyr::mutate(
        obs = obs |>
          stringr::str_trim() |>
          dplyr::na_if("") |>
          dplyr::na_if("-")
      )
  }

  unidade_quota <- stringr::str_subset(nomes_colunas, "unidade_da_quota")

  if(!purrr::is_empty(unidade_quota)) {
    anexo <- anexo |>
      dplyr::rename(unidade_quota = .data$unidade_da_quota)
  }

  result <- anexo |>
    dplyr::mutate(
      inicio_de_vigencia = formata_datas(.data$inicio_de_vigencia),
      termino_de_vigencia = formata_datas(.data$termino_de_vigencia, preenche_data = "9999-12-31"),
      no_ex = dplyr::case_when(
        stringr::str_detect(
          .data$no_ex, "\\d"
        ) ~ stringr::str_pad(
          .data$no_ex,
          width = 3,
          side = 'left',
          pad = "0"),
        TRUE ~ .data$no_ex
      )) |>
    dplyr::mutate(ncm = stringr::str_replace_all(.data$ncm, "\\.", ""))

  if("no_ex" %in% nomes_colunas) {
    result <- result |>
      dplyr::mutate(
        no_ex = .data$no_ex |>
          dplyr::na_if("-")
      )
  }

  if("quota" %in% nomes_colunas) {
    result <- result |>
      dplyr::mutate(
        quota = .data$quota |> dplyr::na_if("-"),
        unidade_quota = .data$unidade_quota |>
          dplyr::na_if( "-")
      )
  }



  if(tem_lista == 1) {
    result <- result |>
      dplyr::mutate(lista = nome_lista)
  }

  if(processa_data_inclusao) {
    return(result |>
             dplyr::mutate(
               data_do_ato_de_inclusao = formata_datas(
                 .data$data_do_ato_de_inclusao)
               ))

  }

  return(result)

}

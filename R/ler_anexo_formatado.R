#' Ler anexos do arquivo excel obtido por meio da função `download_tarifas`.
#'
#' Lê o anexo desejado do arquivo de tarifas obtido por meio da função
#' `download_tarifas`. Para cada anexo, a função `ler_anexo_formatado` limpa
#' e organiza os dados de cada aba do arquivo (anexo).
#'
#' Para o Anexo I, a função apenas delega o processamento para
#' `ler_anexo1`. Para os demais anexos suportados, a função:
#' \itemize{
#'   \item identifica a aba correta com base no nome do anexo;
#'   \item pula linhas de cabeçalho conforme o tipo de anexo;
#'   \item padroniza nomes de colunas com `janitor::clean_names`;
#'   \item garante a existência de colunas de vigência;
#'   \item formata datas com a função auxiliar `formata_datas`;
#'   \item padroniza códigos \code{ncm} e \code{no_ex}
#' }
#'
#' @param x Objeto retornado por `download_tarifas`, que no caso é o caminho
#' temporário em que o arquivo com as tarifas de importação vigentes foi baixado.
#' @param n_anexo String indicando qual anexo deve ser lido. Deve ser
#'   um dos:
#'   \code{"i"}, \code{"ii"}, \code{"iv"}, \code{"v"},
#'   \code{"vi"}, \code{"viii"}, \code{"ix"}, \code{"x"}.
#'
#' @return
#' Para \code{n_anexo = "i"}, retorna o tibble produzido por `ler_anexo1`.
#'
#' Para os demais anexos, retorna um tibble com, no mínimo, as colunas:
#' \describe{
#'   \item{ncm}{Código NCM, como texto.}
#'   \item{no_ex}{Número do \emph{ex} padronizado com 3 dígitos}
#'   \item{inicio_de_vigencia}{Data de início de vigência em formato
#'     padronizado, conforme `formata_datas`.}
#'   \item{termino_de_vigencia}{Data de término de vigência em formato
#'     padronizado; quando ausente no arquivo, é preenchida com
#'     \code{"9999-12-31"}.}
#'   \item{data_do_ato_de_inclusao}{(Quando existente) data do ato de inclusão,
#'     também formatada por `formata_datas`.}
#' }
#' Outras colunas presentes no anexo original são mantidas, podendo variar
#' conforme o tipo de anexo.
#'
#' @details
#' A seleção da aba é feita com base nos nomes das planilhas do arquivo Excel,
#' convertidos para minúsculas, procurando um padrão que contenha o número
#' romano do anexo (por exemplo, \code{" iv "} para o Anexo IV).
#'
#' Para o Anexo VI, caso exista uma coluna \code{x10}, ela é removida
#' automaticamente antes do processamento.
#'
#' Quando não há coluna de término de vigência no anexo, a função cria
#' a coluna \code{termino_de_vigencia} com valor padrão \code{"-"} e,
#' em seguida, aplica [formata_datas()] com o argumento
#' \code{preenche_data = "9999-12-31"}.
#'
#' @section Anexos suportados:
#' \itemize{
#'   \item \code{"i"}: TEC vigente
#'   \item \code{"ii"}: lê a aba do Anexo II
#'   \item \code{"iv"}: lê o Anexo IV – Desabastecimento.
#'   \item \code{"v"}: lê o Anexo V – LETEC.
#'   \item \code{"vi"}: lê o Anexo VI – LEBITBK.
#'   \item \code{"viii"}: lê o Anexo VIII – Concessões OMC.
#'   \item \code{"ix"}: lê o Anexo IX – DCC.
#'   \item \code{"x"}: lê o Anexo X – Automotivo.
#' }
#'
#' @seealso
#'   `download_tarifas` para obtenção do arquivo de tarifas,
#'   `ler_anexo1` para o processamento específico do Anexo I
#'   e `formata_datas` para limpeza e padronização de datas.
#'
#' @examples
#' \dontrun{
#' arquivo <- download_tarifas()
#'
#' # Ler Anexo I
#' anexo_i <- ler_anexo_formatado(arquivo, n_anexo = "i")
#'
#' # Ler Anexo V (LETEC) já com datas e NCM padronizados
#' anexo_v <- ler_anexo_formatado(arquivo, n_anexo = "v")
#' }
#'
#' @export
ler_anexo_formatado <- function(
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

  nome_abas <- readxl::excel_sheets(x) |>
    stringr::str_to_lower()

  selecao_aba <- nome_abas |>
    stringr::str_subset(n_anexo_lower)

  numero_aba <- which(nome_abas == selecao_aba)

  nome <- paste0("Anexo_", n_anexo)
  condicao6 <- FALSE

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
        dplyr::mutate(ncm = stringr::str_replace_all(ncm, "\\.", ""))
    )
  } else if (stringr::str_detect(n_anexo_lower, " iv ")) {
    message("Processando Anexo IV - Desabastecimento...")
    pula_linhas <- obter_linha_cabecalho(x, numero_aba) # 4
  } else if (stringr::str_detect(n_anexo_lower, " v ")) {
    message("Processando Anexo V - LETEC...")
    pula_linhas <- obter_linha_cabecalho(x, numero_aba) # 4
  } else if (stringr::str_detect(n_anexo_lower, " vi ")) {
    message("Processando Anexo VI - LEBITBK...")
    pula_linhas <- obter_linha_cabecalho(x, numero_aba) # 3
    condicao6 <- TRUE
  } else if (stringr::str_detect(n_anexo_lower, " viii ")) {
    message("Processando Anexo VIII - Concessões OMC...")
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

  nomes_colunas <- names(anexo)

  if(condicao6 && "x10" %in% nomes_colunas) {
    anexo <- anexo |>
      dplyr::select(-x10)
  }


  termino_vigencia <- stringr::str_subset(nomes_colunas, "termino_de_vigencia")

  processa_data_inclusao <- FALSE

  if("data_do_ato_de_inclusao" %in% nomes_colunas) {
    processa_data_inclusao <- TRUE
  }

  if(purrr::is_empty(termino_vigencia)) {
    anexo <- anexo |>
      dplyr::mutate(termino_de_vigencia = "-")
  }

  inicio <- stringr::str_subset(nomes_colunas, "inicio_da_vigencia")

  if(!purrr::is_empty(inicio)) {
    anexo <- anexo |>
      dplyr::rename(inicio_de_vigencia = inicio_da_vigencia)
  }

  result <- anexo |>
    dplyr::mutate(
      inicio_de_vigencia = formata_datas(inicio_de_vigencia),
      termino_de_vigencia = formata_datas(termino_de_vigencia, preenche_data = "9999-12-31"),
      no_ex = dplyr::case_when(
        stringr::str_detect(
          no_ex, "\\d"
        ) ~ stringr::str_pad(
          no_ex,
          width = 3,
          side = 'left',
          pad = "0"),
        TRUE ~ no_ex
      )) |>
    dplyr::mutate(ncm = stringr::str_replace_all(ncm, "\\.", ""))

  if(processa_data_inclusao) {
    return(result |>
             dplyr::mutate(data_do_ato_de_inclusao = formata_datas(data_do_ato_de_inclusao)))

  } else {
    return(result)
  }

}

#' Adivinha número de linhas a serem puladas para leitura correta do arquivo
#' @keywords internal
#' @noRd
obter_linha_cabecalho <- function(path, aba = NULL) {

  # Lê tudo sem nomes de coluna
  tmp <- suppressMessages(
    readxl::read_excel(
      path,
      sheet = aba,
      col_names = FALSE
    ))

  # Procura a linha onde aparece "NCM"
  linha_cabecalho <- tmp |>
    dplyr::mutate(row = dplyr::row_number()) |>
    tidyr::pivot_longer(-row) |>
    dplyr::filter(toupper(as.character(value)) == "NCM") |>
    dplyr::pull(row) |>
    unique()

  if (length(linha_cabecalho) == 0) {
    stop("Erro de leitura, não há nenhuma coluna 'NCM' na planilha.")
  }

  return(linha_cabecalho - 1)

}

#' Formatar datas provenientes dos anexos de tarifas
#'
#' Função auxiliar para padronizar datas que podem vir em formatos
#' heterogêneos nos anexos de tarifas: datas escritas como texto,
#' datas no formato numérico do Excel (serial number) ou valores
#' especiais representados por hífen (`"-"`).
#'
#' Esta função é usada internamente por `ler_anexo_formatado` e
#' outras funções de limpeza dos anexos.
#'
#' @param x Vetor de caracteres contendo datas em diferentes formatos:
#'   \itemize{
#'     \item números inteiros representando datas no padrão Excel
#'       (serial date);
#'     \item datas textuais (ex.: `"01/02/2024"`, `"2024-02-01"`);
#'     \item hífen (`"-"`), indicando ausência de data.
#'   }
#' @param preenche_data Valor opcional usado para substituir entradas iguais
#'   a `"-"`. Se `NULL` (padrão), o hífen é convertido para `NA`.
#'   Caso contrário, o hífen é substituído pelo valor informado antes da
#'   tentativa de conversão (por exemplo: `"9999-12-31"`).
#'
#' @return
#' Um vetor de classe `Date` com as datas convertidas. Entradas inválidas
#' ou impossíveis de interpretar retornam `NA`.
#'
#' @details
#' A função segue três passos principais:
#' \itemize{
#'   \item remoção de espaços em branco nas extremidades;
#'   \item substituição opcional do hífen (`"-"`) pelo valor definido
#'     em `preenche_data`;
#'   \item identificação e conversão de datas em formato Excel (números
#'     inteiros) e de datas textuais, usando
#'     `lubridate::parse_date_time` com múltiplas ordens possíveis
#'     (\code{"dmy"}, \code{"dmY"}, \code{"ymd"}, \code{"Ymd"}).
#' }
#'
#' Notas:
#' \itemize{
#'   \item A origem do Excel usada é `"1899-12-30"`, compatível com o
#'     comportamento padrão do Excel no Windows.
#'   \item Conflitos de parsing são suprimidos com \code{suppressWarnings()}.
#' }
#'
#' @examples
#' \dontrun{
#' formata_datas(c("01/02/2024", "2024-02-01"))
#' formata_datas(c("45210", "45211"))  # datas em formato Excel
#' formata_datas(c("-", "01/01/2024"), preenche_data = "9999-12-31")
#' }
#'
#' @keywords internal
formata_datas <- function(x, preenche_data = NULL) {
  # Remove espaços extras
  x <- trimws(x)

  # Substitui "-" pelo definido no argumento
  if (is.null(preenche_data)) {
    x <- gsub("^-$", "", x)
  } else {
    x <- gsub("^-$", preenche_data, x)
  }

  # Verifica se é número (mesmo se veio como texto)
  is_num <- grepl("^[0-9]+$", x)

  # Inicializa vetor de saída
  out <- rep(as.Date(NA), length(x))

  # Converte os números inteiros (Excel serial date)
  if (any(is_num)) {
    out[is_num] <- as.Date(as.numeric(x[is_num]), origin = "1899-12-30")
  }

  # Converte os que são textos de data
  if (any(!is_num)) {
    out[!is_num] <- suppressWarnings(
      lubridate::parse_date_time(
        x[!is_num],
        orders = c("dmy", "dmY", "ymd", "Ymd")
      ) |>  as.Date()
    )
  }

  return(out)
}

#' Função para consultar nome e número dos anexos das tarifas de importação
#'
#' Função pode ser utilizada para consulta rápida dos nomes e números dos anexos
#' de tarifas ou para verificar qual argumento usar na função `ler_anexo_formatado`.
#'
#' @return Retorna um tibble com número do anexo e nome correspondente do anexo
#'
#' @examples
#' listar_anexos()
#'
#' @export
listar_anexos <- function() {
  tibble::tibble(
    numero_anexo = c("i", "ii", "iv", "v", "vi", "viii", "ix", "x"),
    nome_abreviado_anexo = c(
      "Tarifa Externa Comum",
      "Anexo II",
      "Desabastecimento",
      "LETEC",
      "LEBITBK",
      "Concessões OMC",
      "DCC",
      "Automotivos ACE-14"
    )
  )
}

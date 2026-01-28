#' Formatar datas provenientes dos anexos de tarifas
#'
#' Funcao auxiliar para padronizar datas que podem vir em formatos
#' heterogeneos nos anexos de tarifas: datas escritas como texto,
#' datas no formato numerico do Excel (serial number) ou valores
#' especiais representados por hifen (`"-"`).
#'
#' Esta funcao e usada internamente por `ler_anexo` e
#' outras funcoes de limpeza dos anexos.
#'
#' @param x Vetor de caracteres contendo datas em diferentes formatos:
#'   \itemize{
#'     \item numeros inteiros representando datas no padrao Excel
#'       (serial date);
#'     \item datas textuais (ex.: `"01/02/2024"`, `"2024-02-01"`);
#'     \item hifen (`"-"`), indicando ausencia de data.
#'   }
#' @param preenche_data Valor opcional usado para substituir entradas iguais
#'   a `"-"`. Se `NULL` (padrao), o hifen e convertido para `NA`.
#'   Caso contrario, o hifen e substituido pelo valor informado antes da
#'   tentativa de conversao (por exemplo: `"9999-12-31"`).
#'
#' @return
#' Um vetor de classe `Date` com as datas convertidas. Entradas invalidas
#' ou impossiveis de interpretar retornam `NA`.
#'
#' @details
#' A funcao segue tres passos principais:
#' \itemize{
#'   \item remocao de espacos em branco nas extremidades;
#'   \item substituicao opcional do hifen (`"-"`) pelo valor definido
#'     em `preenche_data`;
#'   \item identificacao e conversao de datas em formato Excel (numeros
#'     inteiros) e de datas textuais, usando
#'     `lubridate::parse_date_time` com multiplas ordens possiveis
#'     (\code{"dmy"}, \code{"dmY"}, \code{"ymd"}, \code{"Ymd"}).
#' }
#'
#' Notas:
#' \itemize{
#'   \item A origem do Excel usada e `"1899-12-30"`, compativel com o
#'     comportamento padrao do Excel no Windows.
#'   \item Conflitos de parsing sao suprimidos com \code{suppressWarnings()}.
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
  # Remove espacos extras
  x <- trimws(x)

  # Substitui "-" pelo definido no argumento
  if (is.null(preenche_data)) {
    x <- gsub("^-$", "", x)
  } else {
    x <- gsub("^-$", preenche_data, x)
  }

  # Verifica se e numero (mesmo se veio como texto)
  is_num <- grepl("^[0-9]+$", x)

  # Inicializa vetor de saida
  out <- rep(as.Date(NA), length(x))

  # Converte os numeros inteiros (Excel serial date)
  if (any(is_num)) {
    out[is_num] <- as.Date(as.numeric(x[is_num]), origin = "1899-12-30")
  }

  # Converte os que sao textos de data
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

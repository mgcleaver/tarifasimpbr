# Le somente os metadados necessarios para apresentar a coluna de datas.
# O texto bruto continua separado e e usado na validacao, nunca o texto exibido.
datas_atos_excel <- function(x, numero_aba, primeira_linha, coluna, bruto) {
  resultado <- list(texto = bruto, calculo = bruto)
  if (!length(bruto) || !file.exists(x)) return(resultado)
  if (readxl::excel_format(x) != "xlsx") {
    warning("Apresentacao visual das datas disponivel somente para XLSX; texto bruto preservado.", call. = FALSE)
    return(resultado)
  }

  ler_xml <- function(nome) {
    conexao <- unz(x, nome, open = "rb")
    on.exit(close(conexao))
    xml2::read_xml(conexao, options = "NONET")
  }
  localizar <- function(no, nome) xml2::xml_find_all(no, paste0(".//*[local-name()='", nome, "']"))
  wb <- ler_xml("xl/workbook.xml")
  abas <- localizar(wb, "sheet")
  nome_aba <- readxl::excel_sheets(x)[[numero_aba]]
  aba <- abas[xml2::xml_attr(abas, "name") == nome_aba]
  atributos <- xml2::xml_attrs(aba[[1]])
  id <- unname(atributos[grepl("(^|:)id$", names(atributos))])
  rels <- localizar(ler_xml("xl/_rels/workbook.xml.rels"), "Relationship")
  destino <- xml2::xml_attr(rels[xml2::xml_attr(rels, "Id") == id], "Target")
  if (startsWith(destino, "/")) destino <- substring(destino, 2) else destino <- paste0("xl/", destino)
  folha <- ler_xml(destino)
  membros <- utils::unzip(x, list = TRUE)$Name
  estilos <- if ("xl/styles.xml" %in% membros) ler_xml("xl/styles.xml") else
    xml2::read_xml("<styleSheet><cellXfs><xf numFmtId='0'/></cellXfs></styleSheet>")
  xfs <- xml2::xml_find_all(estilos, ".//*[local-name()='cellXfs']/*[local-name()='xf']")
  formatos <- localizar(estilos, "numFmt")
  personalizados <- stats::setNames(xml2::xml_attr(formatos, "formatCode"), xml2::xml_attr(formatos, "numFmtId"))
  sistema1904 <- xml2::xml_attr(localizar(wb, "workbookPr"), "date1904")
  sistema1904 <- length(sistema1904) == 1L && !is.na(sistema1904) && sistema1904 %in% c("1", "true")

  letra <- ""
  while (coluna > 0L) {
    letra <- paste0(LETTERS[(coluna - 1L) %% 26L + 1L], letra)
    coluna <- (coluna - 1L) %/% 26L
  }
  enderecos <- paste0(letra, primeira_linha + seq_along(bruto) - 1L)
  celulas <- localizar(folha, "c")
  indices <- match(enderecos, xml2::xml_attr(celulas, "r"))
  nao_renderizadas <- integer()
  for (i in which(!is.na(indices))) {
    celula <- celulas[[indices[[i]]]]
    tipo <- xml2::xml_attr(celula, "t")
    if (!is.na(tipo) && tipo != "n") next # Texto, inclusive misto, permanece literal.
    if (is.na(bruto[[i]]) || !nzchar(bruto[[i]])) next
    numero <- suppressWarnings(as.numeric(bruto[[i]]))
    if (!is.finite(numero)) next
    estilo <- suppressWarnings(as.integer(xml2::xml_attr(celula, "s")))
    if (is.na(estilo)) estilo <- 0L
    formato_id <- if (estilo >= 0L && estilo < length(xfs))
      as.integer(xml2::xml_attr(xfs[[estilo + 1L]], "numFmtId")) else NA_integer_
    formato <- unname(personalizados[as.character(formato_id)])
    if (is.na(formato)) formato <- switch(as.character(formato_id),
      "0" = "General", "1" = "0", "14" = "dd/mm/yyyy", NA_character_)

    # A base 1904 pertence ao arquivo, nao a uma preferencia de formatacao.
    # Fracoes continuam invalidas para o parser de datas de atos acordado.
    if (sistema1904 && numero >= 0 && numero == floor(numero)) {
      resultado$calculo[[i]] <- format(as.Date(numero, origin = "1904-01-01"), "%Y-%m-%d")
    }
    if (!is.na(formato) && formato %in% c("General", "0", "@")) next
    visual <- exibir_data_excel(numero, formato, sistema1904)
    if (is.na(visual)) {
      nao_renderizadas <- c(nao_renderizadas, i)
    } else {
      resultado$texto[[i]] <- visual
      # Corrige a diferenca de base anterior a marco de 1900, sem aceitar
      # o dia ficticio 29/02/1900 como uma data real.
      if (!sistema1904 && numero >= 1 && numero <= 60 && numero == floor(numero)) {
        resultado$calculo[[i]] <- if (numero == 60) "29/02/1900" else
          format(as.Date(numero, origin = "1899-12-31"), "%Y-%m-%d")
      }
    }
  }
  if (length(nao_renderizadas)) warning(
    paste0("Formato visual de datas nao suportado nas linhas de dados: ",
           paste(nao_renderizadas, collapse = ", "), ". Texto bruto preservado."), call. = FALSE)
  resultado
}

# Formatos numericos de data sem hora, nomes de meses ou condicoes. O formato
# interno 14 depende da localidade do Excel e e apresentado em pt-BR.
exibir_data_excel <- function(numero, formato, sistema1904 = FALSE) {
  if (is.na(formato) || numero < 0 || !is.finite(numero)) return(NA_character_)
  formato <- tolower(formato)
  formato <- gsub("\\[\\$-[^]]+\\]", "", formato)
  formato <- gsub('"([^"dmy]*)"', "\\1", formato)
  formato <- gsub("\\\\([/.-])", "\\1", formato)
  if (!grepl("^(d{1,2}|m{1,2}|y{2}|y{4})([/.-])(d{1,2}|m{1,2}|y{2}|y{4})\\2(d{1,2}|m{1,2}|y{2}|y{4})$", formato)) return(NA_character_)
  partes <- strsplit(formato, "[/.-]")[[1]]
  if (!setequal(substr(partes, 1, 1), c("d", "m", "y"))) return(NA_character_)
  dia_excel <- floor(numero)
  if (!sistema1904 && dia_excel == 0) return(NA_character_)
  data <- as.Date(dia_excel, origin = if (sistema1904) "1904-01-01" else if (dia_excel < 60) "1899-12-31" else "1899-12-30")
  if (data > as.Date("9999-12-31")) return(NA_character_)
  valores <- c(d = as.integer(format(data, "%d")), m = as.integer(format(data, "%m")), y = as.integer(format(data, "%Y")))
  if (!sistema1904 && dia_excel == 60) valores <- c(d = 29L, m = 2L, y = 1900L)
  textos <- vapply(partes, function(parte) {
    valor <- valores[[substr(parte, 1, 1)]]
    if (parte == "yy") valor <- valor %% 100L
    sprintf(paste0("%0", nchar(parte), "d"), valor)
  }, character(1))
  paste(textos, collapse = sub("^[dmy]+([/.-]).*$", "\\1", formato))
}

# Converte a celula inteira: uma parte invalida impede qualquer resumo parcial.
# O texto original e preservado pelo leitor, antes de chamar esta funcao.
formata_datas_atos <- function(x, contexto = NULL) {
  original <- as.character(x)
  resultado <- rep(as.Date(NA_character_), length(original))
  invalidas <- integer()
  origem_excel <- as.Date("1899-12-30")
  limite_excel <- as.numeric(as.Date("9999-12-31") - origem_excel)

  converte_parte <- function(parte) {
    if (grepl("^[0-9]+$", parte)) {
      serial <- as.numeric(parte)
      if (!is.finite(serial) || serial > limite_excel) return(NA_real_)
      return(as.numeric(origem_excel) + serial)
    }

    if (grepl("^[0-9]{1,2}/[0-9]{1,2}/[0-9]{4}$", parte)) {
      componentes <- as.integer(strsplit(parte, "/", fixed = TRUE)[[1]])
      data_iso <- sprintf("%04d-%02d-%02d", componentes[3], componentes[2], componentes[1])
    } else if (grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", parte)) {
      data_iso <- parte
    } else {
      return(NA_real_)
    }

    data <- as.Date(data_iso, format = "%Y-%m-%d")
    if (is.na(data) || substr(data_iso, 1, 4) == "0000" ||
        format(data, "%Y-%m-%d") != data_iso) return(NA_real_)
    as.numeric(data)
  }

  for (i in seq_along(original)) {
    celula <- trimws(original[[i]])
    if (is.na(celula) || celula %in% c("", "-")) next

    partes <- stringr::str_split(
      celula, stringr::regex("\\s*\\be\\b\\s*|[;,|\\r\\n]+", ignore_case = TRUE)
    )[[1]] |>
      trimws()
    datas <- vapply(partes, converte_parte, numeric(1))

    if (anyNA(datas)) {
      invalidas <- c(invalidas, i)
    } else {
      resultado[[i]] <- as.Date(max(datas), origin = "1970-01-01")
    }
  }

  if (length(invalidas)) {
    warning(
      paste0(
        "Datas de atos invalidas",
        if (!is.null(contexto)) paste0(" em ", contexto),
        " nas linhas de dados: ", paste(invalidas, collapse = ", "),
        ". Texto original preservado; data_mais_recente_dos_atos = NA."
      ),
      call. = FALSE
    )
  }

  resultado
}

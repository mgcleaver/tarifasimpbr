# ambiente interno para guardar resultados processados durante a sessao do R
.cache_tarifas <- new.env(parent = emptyenv())

cria_chave_cache_anexo_i <- function(x) {
  tryCatch(
    {
      caminho <- normalizePath(x, winslash = "/", mustWork = TRUE)
      info_arquivo <- file.info(caminho)

      if (
        is.na(info_arquivo$size) ||
          is.na(info_arquivo$mtime)
      ) {
        return(NA_character_)
      }

      paste0(
        caminho,
        "|",
        info_arquivo$size,
        "|",
        as.numeric(info_arquivo$mtime)
      )
    },
    error = function(e) NA_character_
  )
}

ler_anexo1_com_cache <- function(x) {
  ler_anexo_com_cache(x, "i")
}

ler_anexo_com_cache <- function(x, n_anexo) {
  chave_cache <- cria_chave_cache_anexo_i(x)

  if (is.na(chave_cache)) {
    return(ler_anexo_sem_cache(x, n_anexo))
  }

  # Preserva a chave existente do Anexo I e separa os demais anexos do arquivo.
  if (n_anexo != "i") {
    chave_cache <- paste0(chave_cache, "|", n_anexo)
  }

  tem_cache <- tryCatch(
    exists(chave_cache, envir = .cache_tarifas, inherits = FALSE),
    error = function(e) FALSE
  )

  if (tem_cache) {
    resultado_cache <- tryCatch(
      get(chave_cache, envir = .cache_tarifas, inherits = FALSE),
      error = function(e) NULL
    )

    if (!is.null(resultado_cache)) {
      message("Obtendo Anexo ", toupper(n_anexo), " processado do cache...")
      return(resultado_cache)
    }
  }

  resultado <- ler_anexo_sem_cache(x, n_anexo)

  tryCatch(
    assign(chave_cache, resultado, envir = .cache_tarifas),
    error = function(e) NULL
  )

  resultado
}

#' Limpar cache interno de tarifas
#'
#' Remove da memoria os resultados processados e armazenados durante a sessao
#' atual do R. O cache evita reprocessamentos de todos os anexos suportados em
#' chamadas a [ler_anexo()]. Depois da limpeza, o cache e preenchido novamente
#' conforme os anexos forem lidos. Cada anexo e identificado pelo caminho
#' normalizado, tamanho e data de modificacao do arquivo. O cache guarda os
#' dados sem filtro de vigencia; esse filtro continua sendo aplicado na consulta.
#'
#' @return Invisivelmente `TRUE` quando a limpeza e concluida, ou `FALSE` se
#'   houver alguma falha inesperada ao limpar o cache.
#'
#' @export
limpar_cache_tarifas <- function() {
  tryCatch(
    {
      rm(
        list = ls(envir = .cache_tarifas, all.names = TRUE),
        envir = .cache_tarifas
      )
      invisible(TRUE)
    },
    error = function(e) invisible(FALSE)
  )
}

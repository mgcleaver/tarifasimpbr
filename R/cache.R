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
  chave_cache <- cria_chave_cache_anexo_i(x)

  if (is.na(chave_cache)) {
    return(ler_anexo1(x))
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
      message("Obtendo Anexo I processado do cache...")
      return(resultado_cache)
    }
  }

  resultado <- ler_anexo1(x)

  tryCatch(
    assign(chave_cache, resultado, envir = .cache_tarifas),
    error = function(e) NULL
  )

  resultado
}

#' Limpar cache interno de tarifas
#'
#' Remove da memoria os resultados processados e armazenados durante a sessao
#' atual do R. Atualmente, o cache e usado apenas para evitar reprocessamentos
#' repetidos do Anexo I em chamadas a [ler_anexo()].
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

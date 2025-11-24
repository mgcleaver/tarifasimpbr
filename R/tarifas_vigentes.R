tarifas_vigentes <- function(x) {
  hoje <- Sys.Date()

  x |>
    dplyr::mutate(ref = hoje) |>
    dplyr::filter(
      dplyr::between(
        ref,
        inicio_de_vigencia,
        termino_de_vigencia
      )
    ) |>
    dplyr::select(-ref)
}

tarifas_futuras <- function(x) {
  hoje <- Sys.Date()

  x |>
    dplyr::mutate(ref = hoje) |>
    dplyr::filter(
      ref < inicio_de_vigencia
    ) |>
    dplyr::select(-ref)
}

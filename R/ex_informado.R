# Mesma regra de presenca usada nos indicadores tarifarios: NA, vazio e
# hifen sao ausencias. Nao reclassifica valores preenchidos nem valida a
# numeracao legal do destaque; zeros a esquerda permanecem significativos.
ex_informado <- function(no_ex) {
  valor <- stringr::str_trim(as.character(no_ex))
  !is.na(valor) & valor != "" & valor != "-"
}

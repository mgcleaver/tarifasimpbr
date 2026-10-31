# Execute na raiz do projeto com Rscript --vanilla tools/check.R --atualizar.
# A biblioteca fica isolada dos pacotes pessoais e separada por versão do R.
argumentos <- commandArgs(trailingOnly = TRUE)
if (any(!argumentos %in% "--atualizar")) {
  stop("Use Rscript --vanilla tools/check.R [--atualizar].")
}
if (!file.exists("DESCRIPTION")) {
  stop("Execute este script na raiz do projeto tarifasimpbr.")
}

projeto <- normalizePath(".", winslash = "/")
descricao <- read.dcf("DESCRIPTION")
versao_r_minima <- sub(".*R \\(>= ([0-9.]+)\\).*", "\\1", descricao[1, "Depends"])
if (getRversion() < versao_r_minima) {
  stop(
    "A validação requer R >= ", versao_r_minima,
    ". Selecione o executável correto do Rscript."
  )
}
pasta_check <- file.path(projeto, ".r-check", paste0("R-", getRversion()))
biblioteca <- file.path(pasta_check, "library")
dir.create(biblioteca, recursive = TRUE, showWarnings = FALSE)
Sys.setenv(
  R_LIBS = biblioteca,
  R_LIBS_USER = biblioteca,
  R_LIBS_SITE = "NULL",
  NOT_CRAN = "true"
)
.libPaths(c(biblioteca, .Library), include.site = FALSE)
options(repos = c(CRAN = "https://cloud.r-project.org"), timeout = 300)

campos <- intersect(c("Imports", "LinkingTo", "Suggests"), colnames(descricao))
dependencias <- strsplit(paste(descricao[1, campos], collapse = ","), ",")[[1]]
dependencias <- trimws(sub("\\s*\\(.*\\)", "", dependencias))
instalados <- installed.packages()
faltantes <- setdiff(dependencias, rownames(instalados))

if ("--atualizar" %in% argumentos || length(faltantes) > 0L) {
  disponiveis <- available.packages()
  if (nrow(disponiveis) == 0L) {
    stop("Não foi possível consultar o CRAN para preparar as dependências.")
  }
  if (length(faltantes) > 0L) {
    install.packages(
      faltantes,
      lib = biblioteca,
      dependencies = c("Depends", "Imports", "LinkingTo")
    )
  }
  if ("--atualizar" %in% argumentos) {
    update.packages(
      lib.loc = biblioteca,
      ask = FALSE,
      checkBuilt = TRUE
    )
  }
}

instalados <- installed.packages()
if (!all(dependencias %in% rownames(instalados))) {
  stop("Há dependências ausentes; consulte as mensagens de instalação acima.")
}
write.csv(
  instalados[, c("Package", "Version", "Built", "LibPath")],
  file.path(pasta_check, "dependencias.csv"),
  row.names = FALSE
)
writeLines(capture.output(sessionInfo()), file.path(pasta_check, "sessionInfo.txt"))

# R CMD build/check herdam a mesma biblioteca isolada.
# Copie somente os fontes: evita copiar a biblioteca de teste e caminhos longos
# dos metadados do Git antes de R CMD build aplicar o .Rbuildignore.
pasta_fonte <- tempfile("fonte-", tmpdir = pasta_check)
dir.create(pasta_fonte)
arquivos <- list.files(projeto, all.files = TRUE, full.names = TRUE, no.. = TRUE)
arquivos <- arquivos[
  !basename(arquivos) %in% c(".git", ".r-check", ".Rproj.user")
]
if (!all(file.copy(arquivos, pasta_fonte, recursive = TRUE))) {
  stop("Não foi possível preparar os fontes para R CMD build.")
}
setwd(pasta_check)
executavel_r <- file.path(R.home("bin"), "R")
status_build <- system2(
  executavel_r,
  c("CMD", "build", "--no-manual", shQuote(pasta_fonte))
)
unlink(pasta_fonte, recursive = TRUE)
if (status_build != 0L) {
  stop("R CMD build falhou.")
}
arquivo_pacote <- paste0(descricao[1, "Package"], "_", descricao[1, "Version"], ".tar.gz")
status_check <- system2(executavel_r, c("CMD", "check", "--no-manual", arquivo_pacote))
log_check <- file.path(paste0(descricao[1, "Package"], ".Rcheck"), "00check.log")
if (status_check != 0L || !file.exists(log_check)) {
  stop("R CMD check falhou; consulte os resultados em ", pasta_check)
}
if (any(grepl("^Status:.*(ERROR|WARNING)", readLines(log_check, warn = FALSE)))) {
  stop("R CMD check encontrou erros ou avisos; consulte ", log_check)
}
message("Validação concluída. Resultados e versões das dependências em ", pasta_check)

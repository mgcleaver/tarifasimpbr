# tarifasimpbr

![Lifecycle: Experimental](https://img.shields.io/badge/lifecycle-experimental-orange)
[![R-CMD-check](https://github.com/mgcleaver/tarifasimpbr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/mgcleaver/tarifasimpbr/actions/workflows/R-CMD-check.yaml)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE.md)

## Visão geral

tarifasimpbr é um pacote com funções para consultar e trabalhar com as
tarifas de importação do Brasil.

A fonte das informações tarifárias deste pacote pode ser encontrada em:
<https://www.gov.br/mdic/pt-br/assuntos/camex/se-camex/strat/tarifas/vigentes>

## Instalação

```         
# install.packages("pak")
pak::pak("mgcleaver/tarifasimpbr")

# ou, usando remotes
# install.packages("remotes")
remotes::install_github("mgcleaver/tarifasimpbr")
```

Observação: em alguns sistemas, a instalação via `pak::pak()` pode exigir
ferramentas de compilação para instalar dependências a partir do código-fonte.
No Windows, **verifique se o Rtools compatível com a sua versão do R está
instalado**. No macOS, pode ser necessário instalar as Xcode Command Line Tools.
No Linux, pode ser necessário instalar compiladores e bibliotecas de sistema
usadas por dependências do R.

## Uso

```         
library(tarifasimpbr)

# consultar a data do arquivo oficial vigente
data_arquivo_tarifas()

# baixar o arquivo oficial de tarifas para um diretório temporário
x <- download_tarifas()

# ou salvar em um arquivo específico; o diretório deve existir
# arquivos existentes são sobrescritos
dir.create("dados", showWarnings = FALSE)
x_local <- download_tarifas(file.path("dados", "tarifas_raw.xlsx"))
```

Com a função `listar_anexos()` é possível obter uma tabela com as referências para cada
um dos anexos. A coluna `numero_anexo` que resulta de `listar_anexos()` deve ser utilizada
para consultar um anexo específico na função `ler_anexo()`.

```
# consultar os anexos tarifários disponíveis para leitura
listar_anexos()

# ler o anexo i formatado
tec <- ler_anexo(x, "i")

# ler letec formatada
letec <- ler_anexo(x, "v")

# ler dcc formatada
dcc <- ler_anexo(x, "ix")

# ver listas de dcc com vigência futura (se houver)
dcc |>
    tarifas_futuras()

# obter tabela completa das tarifas aplicadas vigentes
tarifas <- tarifas_aplicadas(x)

# obter lista com todos os ex-tarifários presentes nos anexos entre iv e x.
lista_ex <- ex_tarifarios(x)
```

## Outras informações

Os nomes das colunas aliquota, tec, teb e tarifa_aplicada estão em
porcentagem.

A tarifa_aplicada é obtida a partir da teb e das listas de exceção. As
listas de exceção têm prioridade sobre a teb somente quando a tarifa da
NCM foi alterada integralmente pela lista. No caso de uma lista que altera
somente parte do código (por meio de destaque), constará
a teb no resultado da tarifa_aplicada.

Por sua vez, a teb é formada entre a combinação do anexo ii com o anexo i. O anexo ii
tem prioridade sobre o anexo i.

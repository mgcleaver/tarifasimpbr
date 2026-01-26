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
```

## Uso

```         
library(tarifasimpbr)

# baixar arquivo temporário de tarifas de importação
x <- download_tarifas()

# consultar os anexos tarifários disponíveis para leitura
listar_anexos()

# ler o anexo tarifário formatado
tec <- ler_anexo(x, "i")
letec <- ler_anexo(x, "v")

# obter tabela completa das tarifas aplicadas vigentes
tarifas <- tarifas_aplicadas(x)
```

## Outras informações

Os nomes das colunas aliquota, tec, teb e tarifa_aplicada estão em
porcentagem.

A tarifa_aplicada é obtida a partir da teb e das listas de exceção. A
lista de exceção tem prioridade sobre a teb somente quando a tarifa da
NCM foi alterada integralmente pela lista. No caso de uma lista de
exceção alterar somente parte do código (por meio de destaque), constará
a teb no resultado da tarifa_aplicada.

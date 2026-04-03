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
```

Com a função `listar_anexos()` é possível obter uma tabela com as referências para cada
um dos anexos. A coluna numero_anexo que resulta de `listas_anexos()` deve ser utilizada 
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
lista_ex <- ex_tarifarios()
```

## Outras informações

Os nomes das colunas aliquota, tec, teb e tarifa_aplicada estão em
porcentagem.

A tarifa_aplicada é obtida a partir da teb e das listas de exceção. A
lista de exceção tem prioridade sobre a teb somente quando a tarifa da
NCM foi alterada integralmente pela lista. No caso de uma lista de
exceção alterar somente parte do código (por meio de destaque), constará
a teb no resultado da tarifa_aplicada.

Por sua vez, a teb é formada entre a combinação do anexo ii com o anexo i. O anexo ii
tem prioridade sobre o anexo i.

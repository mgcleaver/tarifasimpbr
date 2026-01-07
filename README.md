# tarifasimpbr

## Visão geral

tarifasimpbr é um pacote com funções para consultar e trabalhar com as
tarifas de importação do Brasil.

A fonte das informações tarifárias deste pacote pode ser encontrada em: 
https://www.gov.br/mdic/pt-br/assuntos/camex/se-camex/strat/tarifas/vigentes

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

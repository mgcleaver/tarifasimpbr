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

O pacote requer **R >= 4.2.0**.

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

O pacote suporta I, II, III, IV, V, VI, VIII, IX, X e XI. O XI (Letec+)
é incluído nas consultas consolidadas somente quando existe no arquivo;
planilhas antigas sem essa aba continuam funcionando. A leitura explícita
com `ler_anexo(x, "xi")` exige que a aba esteja presente.

O VII (antiga Lista Covid) não é suportado e não consta das planilhas
oficiais de novembro de 2026 verificadas. O [dicionário de colunas](man/colunas_tarifas.Rd)
explica seu encerramento e a limitação para arquivos históricos com essa aba.

```
# consultar os anexos tarifários disponíveis para leitura
listar_anexos()

# ler o anexo i formatado
tec <- ler_anexo(x, "i")

# ler letec formatada
letec <- ler_anexo(x, "v")

# ler Letec+ em uma planilha que contenha o Anexo XI
letec_mais <- ler_anexo(x, "xi")

# ler dcc formatada
dcc <- ler_anexo(x, "ix")

# ver listas de dcc com vigência futura (se houver)
dcc |>
    tarifas_futuras()

# obter tabela completa das tarifas aplicadas vigentes
tarifas <- tarifas_aplicadas(x)

# obter todos os ex-tarifários das listas suportadas, incluindo XI quando presente
lista_ex <- ex_tarifarios(x)

# consultar vencimentos futuros até o fim do mês corrente
vencimentos <- fim_vigencia(x)

# consultar até o fim do próximo mês, incluindo vencimentos de hoje
vencimentos <- fim_vigencia(x, meses = 1, incluir_hoje = TRUE)
```

## Dicionário de colunas e migração para 0.9.0

Consulte o [dicionário das colunas](man/colunas_tarifas.Rd), também disponível
no R com `?colunas_tarifas`. Ele explica as descrições, alíquotas, datas,
quotas e a sentinela de ausência de prazo individual.

A versão 0.9.0 altera os nomes das colunas de datas de atos de forma
incompatível com seleções e filtros que utilizem os nomes anteriores:

| Nome até 0.8.0 | Nome a partir de 0.9.0 |
| --- | --- |
| `data_do_ato_de_inclusao` nas listas e tabelas derivadas | `data_mais_recente_dos_atos` |
| `data_do_ato` nas buscas | `data_mais_recente_dos_atos` |

Os nomes antigos deixam de ser retornados. A nova coluna é `Date` e contém
a maior data cronológica da célula, mesmo que a ordem textual seja diferente.
`datas_dos_atos_texto` é uma coluna adicional que preserva o conteúdo original
antes da conversão. Se qualquer parte da célula for inválida, a data resumida
é `NA` e a leitura emite aviso. O resumo da célula não identifica, por si só,
a data de cada ato citado. Veja as demais mudanças no [NEWS.md](NEWS.md).

## Outras informações

Os nomes das colunas aliquota, tec, teb e tarifa_aplicada estão em
porcentagem.

A tarifa_aplicada é obtida a partir da teb e das listas de exceção. As
listas de exceção têm prioridade sobre a teb somente quando a tarifa da
NCM foi alterada integralmente pela lista e não está sujeita a quota. No caso de uma lista que altera
somente parte do código (por meio de destaque), constará
a teb no resultado da tarifa_aplicada.

A Letec+ segue essas mesmas regras. Términos individuais ausentes continuam
representados por `9999-12-31`, conforme o leitor existente. Esse valor indica
ausência de término informado na planilha, não uma garantia de vigência futura;
não substitui o limite normativo de utilização da lista.

Por sua vez, a teb é formada entre a combinação do anexo ii com o anexo i. O anexo ii
tem prioridade sobre o anexo i.

## Desenvolvimento

As instruções para validar o pacote localmente e a descrição da CI estão em
[CONTRIBUTING.md](.github/CONTRIBUTING.md).

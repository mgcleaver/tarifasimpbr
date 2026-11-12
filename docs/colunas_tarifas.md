# Dicionário das colunas tarifárias

Este dicionário descreve as principais colunas retornadas pelo **tarifasimpbr
0.9.0**. A ajuda instalada no R continua disponível com `?colunas_tarifas`.
Nem todas as funções retornam todas as colunas. `NA` representa informação
ausente ou não aplicável àquela saída, conforme as regras abaixo.

[Voltar ao README](../README.md)

## Escopo das funções

| Função | Conteúdo retornado |
| --- | --- |
| `listar_anexos()` | Catálogo dos anexos suportados, com `numero_anexo` e `nome_abreviado_anexo`, ambos como texto. Não verifica quais abas existem em um arquivo. |
| `ler_anexo(x, n_anexo)` | Dados do anexo solicitado, com nomes e valores padronizados. Pode preservar colunas adicionais da planilha. A leitura não filtra vigências. |
| `buscar_ncm(x, ncm)` | Registros da NCM nos anexos suportados; no III, também códigos raiz que a abrangem. Retorna a descrição da NCM e a descrição específica da medida em colunas separadas. Não filtra vigências. |
| `buscar_ato(x, ato)` | Registros que correspondem ao ato nos Anexos I, II e listas de exceção suportadas. Usa as mesmas colunas de `buscar_ncm()`. Não filtra vigências. |
| `ex_tarifarios(x)` | Registros com destaque Ex nas listas de exceção suportadas, incluindo medidas futuras ou vencidas que estejam na fonte. `listar_ex_tarifarios()` é um nome alternativo para a mesma função. |
| `detalhar_listas_excecao_vigentes(x)` | Consolidação por NCM das medidas vigentes nas listas, com contagens e dados do registro integral sem quota, quando existente. |
| `tarifas_aplicadas(x)` | NCMs do Anexo I, descrições, marcação BK/BIT, lista, TEC, TEB e tarifa geral aplicada. |
| `tarifas_aplicadas(x, detalhar = TRUE)` | Acrescenta atos, datas, vigências, contagens e indicador de NCM integral à tabela de tarifas. |
| `fim_vigencia(x, meses = 0, incluir_hoje = FALSE)` | Registros cujo término individual está entre amanhã e o fim do mês corrente, por padrão. Os argumentos alteram esse intervalo. Preserva colunas dos anexos e acrescenta `anexo`. Exclui a sentinela `9999-12-31`. |
| `tarifas_vigentes(x)` e `tarifas_futuras(x)` | Filtram uma lista já lida, preservando suas colunas, pela data de execução da consulta. |

São suportados **I, II, III, IV, V, VI, VIII, IX, X e XI**. As listas de exceção
tratadas em conjunto são IV (Desabastecimento), V (Letec), VI (Lebitbk), VIII
(Concessões OMC), IX (DCC), X (Automotivos ACE-14) e XI (Letec+).

O XI é opcional nas consultas consolidadas: arquivos antigos sem essa aba
continuam aceitos. `ler_anexo(x, "xi")` exige uma única aba correspondente.
Essa compatibilidade não torna opcionais os demais anexos exigidos por cada função.

O **VII não é suportado**: `ler_anexo(x, "vii")` gera erro e as consultas
consolidadas não incluem essa aba, mesmo em arquivos históricos. Ele correspondia
à antiga Lista Covid. A [compilação oficial da Resolução Gecex 272/2021](https://www.gov.br/mdic/pt-br/assuntos/camex/resolucoes/gecex/resolucoes-compiladas/resolucao-gecex-no-272-de-19-de-novembro-de-2021-compilada)
registra fim de vigência em 31/03/2024 e revogação expressa pela Resolução
Gecex 592/2024. As planilhas de 04/11/2026 e 11/11/2026 verificadas não contêm VII.

## Identificação e descrições

| Coluna | Tipo no R | Significado e origem |
| --- | --- | --- |
| `ncm` | `character` | Código sem pontuação, normalmente com oito dígitos, preservando zeros iniciais. O Anexo III pode conter códigos raiz mais curtos; esses códigos também podem aparecer em `buscar_ncm()`. |
| `no_ex` | `character` | Número do destaque Ex, padronizado com três dígitos, como `"002"`. Ausência indica que o registro não especifica destaque. |
| `anexo` | `character` | Número romano em minúsculas nas buscas e em `fim_vigencia()`, como `"xi"`. |
| `nome_anexo` | `character` | Nome do anexo nas buscas, conforme o catálogo de `listar_anexos()`. |
| `lista` | `character` | Nome da lista de exceção. Na consolidação e nas tarifas, pode reunir nomes separados por vírgula quando há medidas vigentes em mais de uma lista. Sua presença, por si só, não significa que a tarifa geral foi substituída. |
| `descricao_tec` | `character` | Descrição da NCM de oito dígitos no Anexo I. |
| `descricao_tec_concatenada` | `character` | Descrição formada com os níveis hierárquicos do Anexo I, da posição de quatro dígitos até o subitem de oito. |
| `descricao` | `character` | Nas buscas, descrição da NCM obtida do Anexo I, quando disponível. Nas linhas do Anexo III retornadas por `buscar_ncm()`, recebe `NA`. Em `ler_anexo()` e `ex_tarifarios()`, é a descrição do registro na fonte. |
| `descricao_medida` | `character` | Nas buscas, descrição específica do Anexo II ou da lista de exceção, inclusive restrições do produto. Não substitui `descricao` nem `descricao_tec_concatenada`. Recebe `NA` quando a fonte não fornece essa descrição; também é `NA` para os Anexos I e III. |
| `bkbit` | `character` | Marcação como bem de capital (BK) ou de informática e telecomunicações (BIT), quando informada no Anexo I. |

Exemplo real na planilha de **11/11/2026**, ao executar
`buscar_ncm(x, "29225099")` e selecionar as linhas do Anexo XI:

| `ncm` | `no_ex` | `descricao` | `descricao_medida` |
| --- | --- | --- | --- |
| `29225099` | `NA` | Outros | Outros |
| `29225099` | `002` | Outros | Outros, exceto treonina |

## Alíquotas, quotas e contagens

Os valores de alíquotas estão em **porcentagem**: `10.8` representa 10,8%,
e não uma fração de 0 a 1. O tipo depende da função que produz a coluna.

| Coluna | Tipo no R | Significado e limites |
| --- | --- | --- |
| `tec` | `numeric` no Anexo I e nas tarifas; `character` na leitura do Anexo II | Alíquota da Tarifa Externa Comum. |
| `teb` | `numeric` | Tarifa Externa Brasileira: alíquota aplicada do Anexo II, recorrendo à TEC quando não houver valor nesse anexo. |
| `aliquota` | `character` na leitura das listas e em `fim_vigencia()`; `numeric` nas buscas | Alíquota do registro. Nas buscas, reúne a TEC para o Anexo I, a alíquota aplicada para o II e a alíquota da lista; no III, recebe `NA`. |
| `aliquota_aplicada` | `character` | Alíquota aplicada informada no Anexo II, preservada como texto pelo leitor. |
| `tarifa_aplicada` | `numeric` | Na consolidação das listas, alíquota do registro vigente para NCM integral sem quota, ou `NA` quando ele não existe. Em `tarifas_aplicadas()`, usa essa alíquota, depois a TEB e, na ausência, a TEC. |
| `tarifa_ex` | `character` | Alíquota do destaque em `ex_tarifarios()`, proveniente da coluna `aliquota` da lista. |
| `quota`, `unidade_quota` | `character` | Quantidade e unidade informadas na fonte. O pacote não calcula saldo disponível de quota. |
| `contagem_quota` | `integer` | Número de registros vigentes com quota nas listas de exceção, por NCM; não é volume nem saldo. |
| `contagem_ex` | `integer` | Número de registros vigentes com destaque Ex nas listas, por NCM. Um registro com Ex e quota pode contribuir para ambas as contagens. |
| `ncm_integral` | `integer` | Indicador 0/1 de presença de registro vigente sem Ex nas listas. Um registro integral com quota não substitui a tarifa geral. |

A regra de tarifa geral exige **registro vigente sem Ex e sem quota**, inclusive
na Letec+. A tabela `tarifas_aplicadas()` não calcula automaticamente a tarifa
específica de cada Ex ou quota. Se houver mais de um registro integral vigente
sem quota para a mesma NCM, a consolidação informa erro; não escolhe uma lista
arbitrariamente.

Em `tarifas_aplicadas(detalhar = TRUE)`, as contagens e `ncm_integral` recebem
zero quando a NCM não possui registros vigentes nas listas de exceção.

## Atos e suas datas

| Coluna | Tipo no R | Significado e origem |
| --- | --- | --- |
| `resolucoes` | `character` | Referências normativas do Anexo I. |
| `atos_de_inclusao` | `character` | Referências normativas do Anexo II. |
| `ato_de_inclusao` | `character` | Referências normativas das listas de exceção; também aparece nas saídas derivadas. |
| `ato` | `character` | Nome usado nas buscas para reunir as referências normativas dos diferentes anexos. Uma célula pode mencionar vários atos. |
| `datas_dos_atos_texto` | `character` | Conteúdo original da coluna de datas dos atos de inclusão, antes da conversão, inclusive espaços e separadores. Uma célula numérica do Excel é preservada como texto do número serial, como `"46332"`, e não como a formatação visual `"06/11/2026"`. |
| `data_mais_recente_dos_atos` | `Date` | Maior data cronológica entre todas as datas válidas da célula. Não é necessariamente a data do ato pesquisado, de publicação, de início ou de término da medida. |

O par `datas_dos_atos_texto` / `data_mais_recente_dos_atos` está presente nas
listas **IV, V, VI, VIII, IX, X e XI**. Quando a fonte não contém a coluna de
datas dos atos, ambas recebem `NA`. Não são inferidas datas a partir da vigência
nem do ano escrito na referência normativa. Nas buscas, as linhas dos Anexos I,
II e III recebem `NA` nesse par de colunas.

Na consolidação das listas e em `tarifas_aplicadas(detalhar = TRUE)`, **ato,
datas dos atos e vigências vêm do mesmo registro integral sem quota que
determina a tarifa geral**. Quando há somente Ex ou quota, esses metadados
recebem `NA`. A saída padrão `tarifas_aplicadas(x)` não contém essas colunas.

### Conversão de datas dos atos

São aceitos os formatos brasileiro (`d/m/aaaa` ou `dd/mm/aaaa`), ISO
(`aaaa-mm-dd`) e números inteiros seriais do Excel, com origem em `1899-12-30`.
Datas múltiplas podem ser separadas por `e` (sem distinguir maiúsculas), vírgula,
ponto e vírgula, barra vertical ou quebra de linha.

A escolha é cronológica, independentemente da ordem textual. O pacote não
associa automaticamente cada ato à data que ocupa a mesma posição em outra
célula.

| Texto da célula | `data_mais_recente_dos_atos` | Comportamento |
| --- | --- | --- |
| `25/02/2025 e 06/11/2026` | `2026-11-06` | Maior data. |
| `06/11/2026 e 25/02/2025` | `2026-11-06` | Mesmo resultado com ordem invertida. |
| `2025-02-25; 2026-11-06` | `2026-11-06` | Formato ISO e separador aceitos. |
| `46332` | `2026-11-06` | Serial inteiro do Excel. |
| `06/11/2026 e 31/02/2026` | `NA` | Aviso: uma das datas é inválida. |
| Célula ausente, vazia ou somente `-` | `NA` | Ausência, sem aviso. |

Se **qualquer parte for inválida**, toda a data resumida é `NA`, com aviso que
identifica o anexo e as linhas de dados. O texto original é preservado; não se
descarta a parte inválida para calcular um máximo parcial. O aviso ocorre no
processamento. Leituras reutilizadas do cache não o repetem;
`limpar_cache_tarifas()` força nova leitura.

Na planilha de 11/11/2026, os registros `29225099` integral e Ex `002` do XI
trazem `25/02/2025 e 06/11/2026`. Ambos resultam em `2026-11-06`, inclusive
quando encontrados por `buscar_ato(x, 705)`: a data resume a célula que também
cita a Resolução 978, e não apenas o ato pesquisado.

## Vigências e ausência de prazo individual

| Coluna | Tipo no R | Significado |
| --- | --- | --- |
| `inicio_de_vigencia` | `Date` | Início individual da medida informado na fonte. |
| `termino_de_vigencia` | `Date` | Término individual informado na fonte, ou a sentinela descrita abaixo quando ele está ausente. |

`tarifas_vigentes()` inclui os dois limites do intervalo e usa a data da
consulta. `tarifas_futuras()` seleciona inícios posteriores a essa data.

**`9999-12-31` (31/12/9999) é uma sentinela de ausência de prazo individual
conhecido na fonte.** O leitor a usa quando o término está ausente, vazio ou
como hífen. Não é uma data real de expiração nem garantia de vigência indefinida.

O prazo individual não deve ser confundido com o limite normativo de utilização
de uma lista, como 31/12/2028 para a Letec+. O pacote não preenche a coluna de
término individual com esse limite. `fim_vigencia()` exclui a sentinela dos
vencimentos. Datas ausentes de atos ou de início de vigência não recebem a
sentinela.

Na planilha de 11/11/2026, esse tratamento se aplica, por exemplo, às linhas
do XI para `29055990` e para `29225099` integral e Ex `002`, sem término
individual informado.

## Outras colunas da fonte

`regra` identifica a tabela de origem no Anexo III; `obs` contém observações
quando fornecidas pela função. Ambas são texto. Nem toda função preserva todas
as observações da fonte.

`ler_anexo()` e `fim_vigencia()` podem preservar colunas adicionais que variam
entre anexos e arquivos. Exemplos observados, como texto, na planilha de
11/11/2026:

| Coluna | Origem e conteúdo |
| --- | --- |
| `setor_do_produto_divisao_isic` | Classificação setorial informada nas listas de exceção. |
| `enquadramento_anexo_res_gmc_49_19` | Campo de enquadramento informado no Anexo IV. |
| `bit_bk` | Marcação informada no Anexo II; não é a coluna `bkbit` obtida do Anexo I. |
| `anexo_iii_da_decisao_cmc_no_08_22_percent` | Campo percentual da planilha do Anexo II, mantido como texto. |
| `fundamentacao_da_aliquota_aplicada` | Fundamentação informada no Anexo II. |

Esses exemplos não constituem uma lista fixa de colunas para todos os arquivos.
Para inspecionar a saída de uma leitura, use `names(ler_anexo(x, "xi"))` ou
`str(ler_anexo(x, "xi"))` em um arquivo que contenha XI.

## Migração para 0.9.0

| Nome até 0.8.0 | Nome a partir de 0.9.0 |
| --- | --- |
| `data_do_ato_de_inclusao` nas listas e tabelas derivadas | `data_mais_recente_dos_atos` |
| `data_do_ato` nas buscas | `data_mais_recente_dos_atos` |

Os nomes antigos deixam de ser retornados. Atualize seleções, filtros e demais
consumidores dessas colunas. `datas_dos_atos_texto` é uma coluna adicional.
Os nomes na planilha de entrada continuam aceitos; não é necessário renomeá-los.
Consulte também as [notas da versão](../NEWS.md).

## Fontes e manutenção

Os tipos e exemplos desta página foram conferidos nas saídas do pacote com a
[planilha oficial de 11/11/2026](https://www.gov.br/mdic/pt-br/assuntos/camex/estrategia-comercial/arquivos-listas/11-11-2026-anexos-i-a-x-resolucao-gecex-272-21.xlsx).
A compatibilidade sem XI foi verificada com a
[planilha de 04/11/2026](https://www.gov.br/mdic/pt-br/assuntos/camex/estrategia-comercial/arquivos-listas/04-11-2026-anexos-i-a-x-resolucao-gecex-272-21.xlsx).
As saídas dependem do arquivo utilizado e, nas funções que filtram vigência,
da data de execução.

Ao alterar o significado das colunas, mantenha esta página e a fonte da ajuda
[`R/colunas_tarifas.R`](../R/colunas_tarifas.R) consistentes. A ajuda
`?colunas_tarifas` permanece parte do pacote instalado.

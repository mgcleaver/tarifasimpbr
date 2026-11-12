#' Dicionario das colunas tarifarias
#'
#' Significado, tipos e origem das principais colunas retornadas pelo pacote.
#' Colunas adicionais da planilha podem ser preservadas por [ler_anexo()] e
#' [fim_vigencia()]. Nem todas as funcoes retornam todas as colunas abaixo.
#'
#' @section Identificacao e descricoes:
#' \describe{
#'   \item{ncm}{Texto sem pontuacao, normalmente com oito digitos. O Anexo III
#'     pode conter codigos raiz mais curtos. Zeros iniciais sao preservados.}
#'   \item{no_ex}{Texto com o numero do destaque Ex, padronizado com tres
#'     digitos. Ausencia indica que o registro nao especifica destaque.}
#'   \item{anexo, nome_anexo}{Numero romano em minusculas e nome do anexo no
#'     catalogo e nas buscas. `fim_vigencia()` acrescenta somente `anexo`.}
#'   \item{lista}{Nome da lista de excecao; na consolidacao, nomes separados
#'     por virgula quando houver medidas em mais de uma lista.}
#'   \item{descricao_ncm}{Descricao da propria fonte na leitura dos Anexos
#'     I e II. Nas tarifas e buscas, vem do I, sem substituicao pelo texto do
#'     II ou das listas. Nas buscas do Anexo III, recebe `NA`.}
#'   \item{descricao_tec_concatenada}{Descricao formada com os niveis
#'     hierarquicos do Anexo I, a partir da posicao.}
#'   \item{descricao}{Texto da propria lista em `ler_anexo()` para IV, V,
#'     VI, VIII, IX, X e XI, e em `fim_vigencia()`. Preservado para Ex,
#'     codigo integral e quota.}
#'   \item{descricao_ex}{Nas buscas e em `ex_tarifarios()`, copia somente
#'     a descricao da lista quando ha Ex indicado em `no_ex`. Sem Ex ou sem
#'     descricao na lista, recebe `NA`. Nao usa nem concatena a descricao
#'     do Anexo I. Nas buscas, I, II e III recebem `NA`.}
#'   \item{bkbit}{Marcacao da NCM como bem de capital (BK) ou de informatica e
#'     telecomunicacoes (BIT), quando informada nos Anexos I e II. Nas tarifas
#'     e buscas vem do I; os valores de cada fonte permanecem preservados.}
#' }
#'
#' A indicacao de Ex segue a presenca de `no_ex`: `NA`, vazio apos remover
#' espacos e hifen sao ausencias. Zeros a esquerda sao preservados. Quota
#' preenchida, sozinha, nao preenche `descricao_ex`. Nas planilhas de 04/11 e
#' 11/11/2026 nao foram observados `0` ou `000`; esses valores continuam
#' tratados como preenchidos pela regra existente, sem validar a numeracao
#' legal nem reinterpreta-los silenciosamente como ausencia.
#'
#' @section Aliquotas e quotas:
#' \describe{
#'   \item{tec, teb}{Aliquotas percentuais da TEC e da Tarifa Externa
#'     Brasileira. Na tabela de tarifas aplicadas sao numericas.}
#'   \item{aliquota}{Percentual da lista ou do Anexo II, como texto no leitor,
#'     Ex e vencimentos. Nas buscas e numerica e pode representar a TEC do I.}
#'   \item{tarifa_aplicada}{Percentual numerico consolidado: lista de excecao
#'     vigente para NCM integral sem quota, depois TEB e, na ausencia, TEC.
#'     Nao representa automaticamente a tarifa especifica de cada Ex ou quota.}
#'   \item{quota, unidade_quota}{Quantidade e unidade informadas na fonte,
#'     como texto. O pacote nao calcula o saldo disponivel da quota.}
#'   \item{contagem_quota, contagem_ex}{Numero de registros vigentes com
#'     quota ou destaque, respectivamente; nao sao volumes de quota.}
#'   \item{ncm_integral}{Indicador 0/1 de presenca de registro sem Ex.
#'     Um registro integral com quota nao substitui a tarifa geral.}
#' }
#'
#' @section Atos e suas datas:
#' \describe{
#'   \item{ato_de_inclusao}{Atos de inclusao do Anexo II e listas de excecao.}
#'   \item{resolucoes}{Referencias normativas do Anexo I, nao necessariamente
#'     atos de inclusao.}
#'   \item{ato}{Campo das buscas que reune essas referencias normativas.
#'     Uma celula pode mencionar varios atos.}
#'   \item{datas_dos_atos_texto}{Texto das datas dos atos de inclusao: textos
#'     sao mantidos literalmente, inclusive espacos e separadores; numeros com
#'     formato de data XLSX suportado sao apresentados conforme esse formato.
#'     Por exemplo, `46332` com data curta resulta em `"06/11/2026"`.}
#'   \item{data_mais_recente_dos_atos}{Coluna de classe `Date` com o maior
#'     valor cronologico entre todas as datas da celula. Nao e necessariamente
#'     a data do ato pesquisado, de publicacao, de inicio ou fim da medida.}
#' }
#'
#' Esse par de colunas esta presente em todas as listas suportadas: IV, V,
#' VI, VIII, IX, X e XI. Se a planilha nao tiver coluna de datas de atos,
#' ambas recebem `NA`. O pacote nao infere datas a partir da vigencia nem do
#' ano escrito na referencia normativa. Nas buscas, registros dos Anexos I,
#' II e III nao possuem esse metadado e recebem `NA`.
#'
#' O Anexo VII correspondia a Lista Covid. A
#' [compilacao oficial da Resolucao Gecex 272/2021](https://www.gov.br/mdic/pt-br/assuntos/camex/resolucoes/gecex/resolucoes-compiladas/resolucao-gecex-no-272-de-19-de-novembro-de-2021-compilada)
#' registra fim de vigencia em 31/03/2024 e revogacao expressa pela Resolucao
#' Gecex 592/2024. A aba VII nao consta das planilhas oficiais de 04/11/2026
#' e 11/11/2026 verificadas. O pacote nao suporta VII: a leitura explicita
#' rejeita esse argumento e as consultas consolidadas nao o incluem, mesmo
#' se houver uma aba historica. A compatibilidade com arquivos sem XI nao
#' significa cobertura da antiga Lista Covid. Nao ha coluna VII a migrar
#' nas planilhas verificadas; eventual suporte historico exigiria adaptacao
#' e validacao especificas.
#'
#' A apresentacao consulta tipo e estilo de cada celula XLSX. O formato interno
#' 14 (data curta, dependente da localidade do Excel) usa pt-BR, `dd/mm/aaaa`.
#' Formatos personalizados numericos de dia, mes e ano com `/`, `-` ou `.`
#' sao suportados, inclusive ISO e componentes sem zero inicial. Texto misto
#' permanece literal; numeros com formato Geral ou inteiro permanecem como texto
#' numerico. A validacao usa o valor bruto separado internamente: uma fracao
#' de serial continua invalida mesmo quando exibida sem hora.
#'
#' Formatos com nomes de meses, horas, condicoes ou secoes multiplas, e arquivos
#' XLS, mantem o texto bruto com aviso. Nao se reproduzem largura de coluna,
#' `#####`, formatacao condicional ou preferencias regionais individuais.
#' Formulas usam o resultado salvo, sem recalculo. Nao e criada uma coluna
#' publica adicional de valor bruto e o arquivo de origem nao e modificado.
#'
#' Sao aceitas datas brasileiras (`d/m/aaaa` ou `dd/mm/aaaa`), ISO
#' (`aaaa-mm-dd`) e numeros inteiros seriais do Excel, com origem
#' `1899-12-30` quando escritos como texto. Celulas numericas respeitam tambem
#' o sistema 1904 da pasta; datas formatadas anteriores a marco de 1900
#' consideram a peculiaridade do Excel, rejeitando o dia ficticio 29/02/1900.
#' Datas multiplas podem usar `e` (sem distinguir maiusculas),
#' ponto e virgula, virgula, barra vertical ou quebra de linha como separador.
#' A ordem textual nao altera o resultado. Nao se associam atos e datas
#' automaticamente pela posicao em suas respectivas celulas.
#'
#' Uma celula ausente, vazia ou contendo somente hifen resulta em `NA`, sem
#' aviso. Se qualquer parte nao for uma data valida, toda a data resumida
#' resulta em `NA`, com aviso que identifica o anexo e as linhas de dados.
#' O original permanece disponivel. Nao se descarta a parte invalida para
#' calcular um maximo parcial. O aviso ocorre no processamento; leituras
#' reutilizadas do cache nao o repetem. [limpar_cache_tarifas()] forca nova leitura.
#'
#' Exemplo: `"06/11/2026 e 25/02/2025"` resulta em `2026-11-06`, mesmo
#' quando a busca for pelo ato de 2025. `"06/11/2026 e 31/02/2026"` resulta
#' em `NA` com aviso, pois fevereiro de 2026 nao tem 31 dias.
#'
#' Na consolidacao e em `tarifas_aplicadas(detalhar = TRUE)`, ato, datas dos
#' atos e vigencias pertencem ao registro integral sem quota que determina a
#' tarifa geral. Quando houver somente Ex ou quota, esses metadados sao `NA`.
#'
#' @section Vigencias e ausencia de prazo individual:
#' `inicio_de_vigencia` e `termino_de_vigencia` sao colunas de classe `Date`.
#' [tarifas_vigentes()] inclui ambos os limites e usa a data da consulta.
#' [tarifas_futuras()] seleciona inicios posteriores a essa data.
#'
#' `9999-12-31` (31/12/9999) e uma **sentinela de ausencia de prazo individual
#' conhecido na fonte**, usada quando o termino esta ausente, vazio ou como
#' hifen. Nao e uma data real de expiracao, nem garantia de vigencia indefinida.
#' Nao deve ser confundida com o limite normativo de utilizacao de uma lista,
#' como 31/12/2028 para a Letec+. O pacote nao insere esse limite na coluna de
#' termino individual. [fim_vigencia()] exclui a sentinela dos vencimentos.
#'
#' A sentinela nao e usada para datas ausentes de atos ou de inicio de vigencia.
#'
#' @section Outras colunas:
#' `regra` identifica a tabela de origem no Anexo III. `obs` preserva
#' observacoes, quando fornecidas pela funcao. Colunas como
#' `setor_do_produto_divisao_isic` provem da planilha e podem variar entre anexos.
#'
#' @section Migracao de nomes e arquivos antigos:
#' `data_mais_recente_dos_atos` substitui `data_do_ato_de_inclusao` nas listas
#' e tabelas derivadas, e `data_do_ato` nas buscas. Os nomes antigos deixam de
#' ser retornados; selecoes e filtros externos precisam usar o novo nome.
#' `datas_dos_atos_texto` e uma coluna adicional. Os nomes na planilha de entrada
#' nao mudam. Arquivos antigos sem XI continuam aceitos pelas consultas
#' consolidadas; a leitura explicita de XI continua exigindo a aba.
#'
#' Outras renomeacoes: `descricao_tec` no I e nas tarifas, e `descricao` nas
#' buscas, passam a `descricao_ncm`. O II tambem passa de `descricao` para
#' `descricao_ncm`. Em `ex_tarifarios()`, `descricao` passa a `descricao_ex`.
#' Nas listas e vencimentos, `descricao` conserva seu nome. `aliquota_aplicada` no II e
#' `tarifa_ex` nos Ex passam a `aliquota`. No II, `atos_de_inclusao` passa a
#' `ato_de_inclusao` e `bit_bk` a `bkbit`. No catalogo, `numero_anexo` e
#' `nome_abreviado_anexo` passam a `anexo` e `nome_anexo`. Os nomes antigos
#' deixam de ser retornados. Tipos e valores dessas colunas sao preservados.
#'
#' `descricao_ex` nas buscas e uma coluna adicional em relacao a 0.8.0.
#' Para quem testou versoes intermediarias deste branch, `descricao_medida`
#' passa a `descricao_ncm` no II, a `descricao_ex` nas buscas/Ex e volta a
#' `descricao` nas listas/vencimentos. Nas buscas sem Ex, agora recebe `NA`.
#'
#' @section Ordem das colunas:
#' A ordem relativa das colunas preexistentes na 0.8.0 e preservada,
#' considerando as renomeacoes acima. `datas_dos_atos_texto` fica
#' imediatamente depois de `data_mais_recente_dos_atos`; `descricao_ex`
#' fica imediatamente antes de `descricao_ncm` nas buscas. Novas colunas podem deslocar posicoes numericas.
#'
#' Na leitura individual com [ler_anexo()], `lista` permanece na ultima
#' posicao. Se a fonte nao possui datas de atos, o novo par e acrescentado
#' antes de `lista`; se possui, ocupa a regiao da coluna original de datas.
#' Os filtros de vigencia preservam a ordem da entrada.
#'
#' As outras funcoes mantem suas ordens proprias: `lista` nao precisa ser
#' a ultima coluna. [fim_vigencia()] preserva a ordem da uniao dos anexos,
#' que pode diferir da leitura individual. Prefira selecionar colunas por
#' nome, especialmente ao migrar entre versoes ou trocar a planilha.
#'
#' @name colunas_tarifas
#' @seealso [ler_anexo()], [buscar_ncm()], [buscar_ato()], [ex_tarifarios()],
#'   [detalhar_listas_excecao_vigentes()], [tarifas_aplicadas()], [fim_vigencia()]
NULL

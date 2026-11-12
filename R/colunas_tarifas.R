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
#'   \item{anexo, nome_anexo}{Numero romano em minusculas e nome do anexo nas
#'     buscas. `fim_vigencia()` acrescenta somente `anexo`.}
#'   \item{lista}{Nome da lista de excecao; na consolidacao, nomes separados
#'     por virgula quando houver medidas em mais de uma lista.}
#'   \item{descricao_tec}{Descricao da NCM de oito digitos do Anexo I.}
#'   \item{descricao_tec_concatenada}{Descricao formada com os niveis
#'     hierarquicos do Anexo I, a partir da posicao.}
#'   \item{descricao}{Nas buscas, descricao da NCM no Anexo I; em
#'     `ler_anexo()` e `ex_tarifarios()`, descricao do registro na fonte.}
#'   \item{descricao_medida}{Nas buscas, texto especifico do Anexo II ou da
#'     lista de excecao, incluindo restricoes como "exceto treonina". Nao
#'     substitui a descricao da NCM. Ausente quando a fonte nao a informa.}
#'   \item{bkbit}{Marcacao da NCM como bem de capital (BK) ou de informatica e
#'     telecomunicacoes (BIT), quando informada no Anexo I.}
#' }
#'
#' @section Aliquotas e quotas:
#' \describe{
#'   \item{tec, teb}{Aliquotas percentuais da TEC e da Tarifa Externa
#'     Brasileira. Na tabela de tarifas aplicadas sao numericas.}
#'   \item{aliquota, aliquota_aplicada}{Percentuais da lista ou do Anexo II.
#'     O leitor mantem esses valores como texto; `aliquota` nas buscas e numerica.}
#'   \item{tarifa_aplicada}{Percentual numerico consolidado: lista de excecao
#'     vigente para NCM integral sem quota, depois TEB e, na ausencia, TEC.
#'     Nao representa automaticamente a tarifa especifica de cada Ex ou quota.}
#'   \item{tarifa_ex}{Aliquota do destaque, como texto, em `ex_tarifarios()`.}
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
#'   \item{ato_de_inclusao, atos_de_inclusao, resolucoes, ato}{Textos com
#'     referencias normativas. Os nomes variam entre listas, Anexo II, Anexo I
#'     e buscas, respectivamente. Uma celula pode mencionar varios atos.}
#'   \item{datas_dos_atos_texto}{Texto original da coluna de datas dos atos de
#'     inclusao, preservado antes da conversao, inclusive espacos e separadores.
#'     Uma celula numerica do Excel e preservada como texto do numero serial:
#'     por exemplo, `"46332"`, e nao a formatacao visual `"06/11/2026"`.}
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
#' Sao aceitas datas brasileiras (`d/m/aaaa` ou `dd/mm/aaaa`), ISO
#' (`aaaa-mm-dd`) e numeros inteiros seriais do Excel, com origem
#' `1899-12-30`. Datas multiplas podem usar `e` (sem distinguir maiusculas),
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
#' @name colunas_tarifas
#' @seealso [ler_anexo()], [buscar_ncm()], [buscar_ato()], [ex_tarifarios()],
#'   [detalhar_listas_excecao_vigentes()], [tarifas_aplicadas()], [fim_vigencia()]
NULL

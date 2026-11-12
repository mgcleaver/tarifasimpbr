# tarifasimpbr 0.9.0

## Novidades

- Inclui o Anexo XI (Letec+) em `listar_anexos()` e `ler_anexo()`.
- Integra o XI às consultas por NCM e ato, à listagem de Ex, aos vencimentos
  e às tarifas aplicadas, usando as mesmas regras de NCM integral, Ex e quota
  das demais listas de exceção.
- Acrescenta `descricao_medida` imediatamente antes de `descricao_ncm` nas buscas por NCM e ato, preservando
  a descrição específica da medida no Anexo II ou na lista de exceção,
  inclusive em registros sem Ex, sem substituir as descrições da NCM.
  Quando a fonte não informa essa descrição, a coluna recebe `NA`.
  Isso mantém, por exemplo, "Outros, exceto treonina" para 2922.50.99 Ex 002.
- Documenta as colunas em `?colunas_tarifas`, incluindo a migração dos nomes
  e seus tipos, com link para a documentação no README.

## Mudanças incompatíveis: nomes de colunas

- Unifica a descrição da NCM do Anexo I em `descricao_ncm`: substitui
  `descricao_tec` na leitura do I e nas tarifas, e `descricao` nas buscas.
  Mantém `descricao_tec_concatenada`, que contém informação hierárquica distinta.
- Usa `descricao_medida` para a descrição do Anexo II, das listas, Ex e
  vencimentos, substituindo `descricao` nessas saídas. Nas buscas, coloca-a
  imediatamente antes de `descricao_ncm`, inclusive quando vazias.
- Unifica `aliquota_aplicada` do II e `tarifa_ex` dos Ex em `aliquota`.
  Mantém `tec`, `teb` e `tarifa_aplicada`, com significados próprios.
- No II, padroniza `atos_de_inclusao` para `ato_de_inclusao` e `bit_bk`
  para `bkbit`, preservando valores e tipos. `resolucoes` no I e `ato` nas
  buscas mantêm os nomes, pois têm escopo normativo mais amplo.
- Em `listar_anexos()`, usa `anexo` e `nome_anexo` em lugar de `numero_anexo`
  e `nome_abreviado_anexo`, como nas buscas. Os nomes anteriores deixam de
  ser retornados. Veja o dicionário de colunas para migrar seleções e filtros.

## Mudança incompatível: datas dos atos

- Renomeia `data_do_ato_de_inclusao` nas listas e tabelas derivadas, e
  `data_do_ato` em `buscar_ncm()` e `buscar_ato()`, para
  `data_mais_recente_dos_atos` (classe `Date`). Os nomes antigos deixam de
  ser retornados: seleções, filtros e demais consumidores devem ser atualizados.
  O nome da coluna na planilha de entrada permanece aceito.
- Acrescenta `datas_dos_atos_texto` (texto), preservando o conteúdo original
  textual, inclusive espaços e separadores. Células numéricas XLSX com data
  curta passam a mostrar `06/11/2026`, por exemplo, em lugar de `46332`.
  Consulta os estilos, suporta formatos numéricos de data brasileiros e ISO,
  e considera a base 1904 quando declarada. O formato interno 14 usa pt-BR.
  Formatos visuais não suportados preservam o texto bruto com aviso.
  A validação continua usando o valor bruto separado do texto exibido.
- Aplica o mesmo par de colunas às listas IV, V, VI, VIII, IX, X e XI e às
  consultas, Ex, vencimentos e tarifas detalhadas. Quando a fonte não contém
  a coluna de datas de atos, ambas recebem `NA`. A tabela de
  `tarifas_aplicadas(detalhar = FALSE)` não recebe esse par de datas.
- Seleciona a maior data cronológica da célula, independentemente da ordem
  textual. São aceitos formatos brasileiro e ISO, inteiros seriais do Excel
  e múltiplas datas separadas por `e`, vírgula, ponto e vírgula, barra vertical
  ou quebra de linha. Esse resumo não associa automaticamente cada ato à
  sua data: em uma busca pela Resolução 705, a célula que também cita a 978
  continua resumida pela maior data das duas.
- Se qualquer parte for inválida, retorna `NA` com aviso, preservando o
  texto e evitando um máximo parcial. Ausências resultam em `NA` sem aviso.
  O aviso ocorre na conversão; a reutilização do cache não o repete.

## Correções e compatibilidade

- Mantém `lista` na última posição de `ler_anexo()`, inclusive quando as novas
  colunas de datas são criadas por ausência da coluna original na fonte.
  Preserva a ordem relativa das colunas anteriores nas saídas públicas,
  considerando as renomeações deliberadas e a posição de `descricao_medida` nas buscas. As funções derivadas
  mantêm suas ordens próprias, incluindo a união de colunas em `fim_vigencia()`.
- Preserva a leitura de planilhas antigas sem XI: as consultas consolidadas
  só incluem a aba quando presente. A leitura explícita do XI informa erro
  se a aba não existir ou estiver duplicada.
- A Letec+ segue a mesma regra de tarifa geral das demais listas: somente
  medida vigente para NCM integral, sem Ex e sem quota, substitui a TEB/TEC.
  Ex e quotas continuam identificados separadamente. Metadados das tarifas
  detalhadas pertencem ao mesmo registro integral que determina a alíquota.
- Mantém `9999-12-31` como sentinela de ausência de prazo individual conhecido,
  não como expiração real ou garantia de vigência indefinida. Não preenche
  os términos individuais ausentes com o limite normativo da Letec+.
- Rejeita explicitamente `ler_anexo(x, "vii")`, que antes retornava o VIII
  por aceitar abreviações no argumento. Documenta a ausência de suporte ao
  VII (antiga Lista Covid), encerrado em 2024 e ausente das planilhas verificadas.
  Compatibilidade com planilhas sem XI não significa suporte histórico ao VII.
- Mantém R >= 4.2.0 e acrescenta `xml2` às dependências diretas para ler
  os estilos XLSX (já era dependência indireta via `rvest`).

# tarifasimpbr 0.8.0

## Compatibilidade

- Passa a exigir R >= 4.2.0. O código do pacote não mudou, mas a versão atual de
  `readxl` no CRAN, uma dependência direta, exige esse mínimo. Quem usa R 4.1
  precisa atualizar o R para instalar esta versão.
- Atualiza o mínimo de `testthat` para 3.1.7, que disponibiliza
  `local_mocked_bindings()`, já usado pelos testes.

## Mudanças

- A CI passa a testar R 4.2.0, `oldrel-1` e `release`, deixa de testar `devel`
  e atualiza as dependências. As verificações também podem ser executadas
  manualmente e rodam toda semana, além dos pushes e pull requests.
- Adiciona `tools/check.R` para validar o pacote em uma biblioteca local isolada,
  respeitando o requisito de R do `DESCRIPTION`, com atualização opcional das
  dependências e registro das versões usadas.
- Move as instruções de desenvolvimento para `.github/CONTRIBUTING.md`.

# tarifasimpbr 0.7.0

## Novidades

- Adiciona `fim_vigencia(x, meses = 0, incluir_hoje = FALSE)` para consultar
  vencimentos das listas de exceção até o fim do mês corrente ou de meses
  seguintes, preservando os registros e as colunas de origem de cada anexo.

## Melhorias e correções

- Rejeita valores excessivos de `meses` em `fim_vigencia()` antes do cálculo
  de datas, evitando diferenças de validação entre sistemas operacionais.
- Estende o cache em memória de `ler_anexo()` a todos os anexos suportados,
  mantendo a identificação por caminho, tamanho e data de modificação do arquivo
  e a limpeza por `limpar_cache_tarifas()`. O filtro de vigência continua sendo
  aplicado depois da leitura dos dados em cache.
- Silencia as mensagens de reutilização dos anexos em cache, mantendo as
  mensagens de leitura e processamento quando os dados precisam ser lidos.
- Corrige a busca numérica de `buscar_ato()` para desconsiderar anos separados
  por barras com espaços e datas numéricas ou por extenso, preservando buscas
  por múltiplos atos, zeros à esquerda e números de atos de quatro dígitos.

## Compatibilidade e dados

- Na planilha oficial de 26/10/2026, verificada em 27/10/2026, os nove anexos
  suportados, `tarifas_aplicadas()` (com e sem detalhamento) e `ex_tarifarios()`
  produzem resultados idênticos aos da versão 0.6.0. A consolidação contém
  10.516 NCMs únicas, sem tarifas aplicadas ausentes.
- `fim_vigencia()` inclui o último dia do mês consultado, exclui vigências
  abertas e preserva quotas, Ex e registros com início de vigência futuro.
  Na mesma planilha, com referência em 27/10/2026, não há vencimentos até o
  fim de outubro e há 10 registros com vencimento até o fim de novembro.
- O cache passa a manter mais anexos em memória para evitar releituras nas
  consultas seguintes. Use `limpar_cache_tarifas()` para liberar esses dados
  ou forçar uma nova leitura do arquivo.

# tarifasimpbr 0.6.0

## Novidades

- `ler_anexo()` passa a ler arquivos cujas abas não usam separador após o numeral
  romano, como as planilhas de histórico de medidas tarifárias
  (abas nomeadas apenas `"Anexo IV"`, `"Anexo V"` etc.). Nas versões anteriores
  esses arquivos falhavam com o erro `` `sheet` must have length 1 `` e não podiam
  ser lidos. A identificação da aba agora aceita hífen com ou sem espaços,
  travessão, espaço simples ou nenhum separador após o numeral
  (`"Anexo IV - ..."`, `"Anexo IV-..."`, `"Anexo IV – ..."`, `"Anexo IV"`).

## Mudanças

- Evita linhas duplicadas do Anexo III em `buscar_ncm()` quando o mesmo código raiz aparece em mais de uma tabela.
- Torna a identificação de quotas e Ex robusta a valores textuais, sem alterar indevidamente a tarifa aplicada.
- Preserva tarifas ausentes no resultado detalhado de `tarifas_aplicadas()`, mantendo zero apenas nos indicadores e contagens.
- `tarifas_aplicadas(detalhar = TRUE)` passa a informar início e término de
  vigência, ato de inclusão e data do ato para alterações tarifárias aplicáveis
  à NCM integralmente. Os campos permanecem como `NA` quando houver apenas
  ex-tarifário ou quota.
- Interrompe a consolidação com erro informativo quando os anexos contêm mais de
  um registro integral vigente para a mesma NCM.
- Valida tamanho e legibilidade da planilha baixada antes de substituir o arquivo de destino: o download é feito em um arquivo temporário criado no mesmo diretório de `destfile` e, depois de validado, é movido para o destino em uma única operação, sem cópia intermediária. O diretório de destino precisa permitir escrita e comportar uma cópia adicional do arquivo durante o download.
- Trata término de vigência vazio ou ausente como vigência aberta.
- Melhora o diagnóstico para anexos sem aba correspondente ou com mais de uma aba correspondente.
- Corrige o teste de indisponibilidade do download para funcionar no `R CMD check` sem depender de `NOT_CRAN=true`.
- Amplia a cobertura de testes de `ler_anexo()`, `tarifas_aplicadas()`, `buscar_ncm()` e `download_tarifas()`.

## Notas de compatibilidade

Nos arquivos oficiais de tarifas da SE-Camex verificados, os resultados de
`tarifas_aplicadas()` são idênticos aos da versão 0.5.0, com `detalhar = FALSE`
e `detalhar = TRUE`. As mudanças abaixo alteram o comportamento apenas em
arquivos com as características descritas.

- A busca da aba do anexo é ancorada no início do nome: apenas abas que começam
  com `"Anexo <numeral>"` são reconhecidas. Nomes em que o numeral aparece no
  meio do texto (por exemplo, `"Notas gerais - anexo iv e v"`) deixam de ser
  considerados, o que evita a leitura silenciosa da aba errada. Arquivos com
  nomes de aba fora desse padrão passam a gerar erro explícito em vez de
  resultado incorreto.
- Quotas e números de Ex preenchidos com texto não numérico (por exemplo,
  `"60.000 toneladas"`) passam a ser reconhecidos como valores presentes. Antes
  eram convertidos para `NA` e tratados como ausência, o que podia atribuir à NCM
  a tarifa da lista de exceção em vez da TEB/TEC. Os anexos oficiais verificados
  usam apenas valores numéricos nessas colunas, de modo que a correção não altera
  a tarifa compilada a partir deles.
- `tarifas_aplicadas(detalhar = TRUE)` mantém `NA` em `tec`, `teb` e
  `tarifa_aplicada`, substituindo ausências por zero somente em
  `contagem_quota`, `contagem_ex` e `ncm_integral`.

# tarifasimpbr 0.5.0

## Mudanças

- Adiciona o argumento `destfile` a `download_tarifas()` para permitir o salvamento do arquivo oficial em um caminho definido pelo usuário.
- Valida o caminho de destino e o resultado do download antes de informar sucesso.
- Documenta o salvamento em arquivo específico e amplia os testes de falha do download.
- Evita falhas do `R CMD check` quando o site oficial está temporariamente indisponível.

# tarifasimpbr 0.4.0

## Mudanças

- Adiciona `data_arquivo_tarifas()` para consultar a data do arquivo oficial de tarifas vigente no site da Camex.
- Refatora `download_tarifas()` para reutilizar a obtenção do link oficial do arquivo de tarifas.
- Remove avisos de depreciação em `buscar_ato()` relacionados ao uso de `.data$...` em seleções `tidyselect`.
- Atualiza as variáveis globais internas usadas pelo pacote para manter as checagens mais limpas.
- Adiciona testes para a extração da data do link e para o uso do link oficial de tarifas.

# tarifasimpbr 0.3.0

## Mudanças

- Adiciona `buscar_ncm()` para consultar uma NCM nos anexos tarifários e retornar as ocorrências encontradas em formato padronizado.
- Adiciona `buscar_ato()` para consultar atos nos anexos tarifários, com suporte a busca por número do ato ou texto.
- Adiciona cache em memória por sessão para reutilizar o Anexo I processado em leituras repetidas.
- Adiciona `limpar_cache_tarifas()` para limpar manualmente o cache interno.
- Atualiza mensagens de processamento dos anexos para deixar mais claro o que está sendo lido.
- Amplia a cobertura de testes para busca por NCM, busca por ato e cache do Anexo I.
- Atualiza o README com observações sobre ferramentas de compilação necessárias em algumas instalações.

# tarifasimpbr 0.2.0

## Mudanças

- Alteração de caracteres não ASCII para ASCII na documentação
- Correção da lógica da coluna ncm_integral resultante da função detalhar_listas_excecao_vigentes.R
- Simplificação do resultado de detalhar_listas_excecao_vigentes.R: remoção de indicadores
  de presença de ex tarifário e de quota por conta de redundância.
- Implementação de testes.
- Reorganiza arquivos de funções.
- Função nova para listar ex-tarifários das listas de instrumentos tarifários (anexos iv a x)
- Suporte ao Anexo III em `ler_anexo()`, com leitura das duas tabelas da aba do setor aeronautico e retorno padronizado das colunas `ncm`, `regra` e `obs`.

## Compatibilidade

- R >= 4.x
- Dependências principais:
  - dplyr
  - tidyr
  - tibble
  - purrr
  - readxl
  - janitor
  - stringr
  - lubridate
  - rlang
  - rvest
- Codificação UTF-8.

# tarifasimpbr (desenvolvimento)

- Silencia as mensagens de reutilização dos anexos em cache, mantendo as
  mensagens de leitura e processamento quando os dados precisam ser lidos.
- Adiciona `fim_vigencia(x, meses = 0, incluir_hoje = FALSE)` para consultar
  vencimentos das listas de exceção até o fim do mês corrente ou de meses
  seguintes, preservando os registros e as colunas de origem de cada anexo.
- Estende o cache em memoria de `ler_anexo()` a todos os anexos suportados,
  mantendo a identificacao por caminho, tamanho e data de modificacao do arquivo
  e a limpeza por `limpar_cache_tarifas()`. O filtro de vigencia continua sendo
  aplicado depois da leitura dos dados em cache.
- Corrige a busca numerica de `buscar_ato()` para desconsiderar anos separados
  por barras com espacos e datas numericas ou por extenso, preservando buscas
  por multiplos atos, zeros a esquerda e numeros de atos de quatro digitos.

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

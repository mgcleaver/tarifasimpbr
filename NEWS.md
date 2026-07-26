# tarifasimpbr 0.5.1

## Mudanças

- Evita linhas duplicadas do Anexo III em `buscar_ncm()` quando o mesmo código raiz aparece em mais de uma tabela.
- Torna a identificação de quotas e Ex robusta a valores textuais, sem alterar indevidamente a tarifa aplicada.
- Preserva tarifas ausentes no resultado detalhado de `tarifas_aplicadas()`, mantendo zero apenas nos indicadores e contagens.
- Valida tamanho e legibilidade da planilha baixada antes de substituir o arquivo de destino.
- Trata término de vigência vazio ou ausente como vigência aberta e aceita nomes de abas com ou sem espaços ao redor do hífen.
- Melhora o diagnóstico para anexos sem aba correspondente ou com mais de uma aba correspondente.
- Corrige o teste de indisponibilidade do download para funcionar no `R CMD check` sem depender de `NOT_CRAN=true`.

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

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

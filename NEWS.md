# tarifasimpbr 0.1.1

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

# tarifasimpbr 0.3.0

## Mudanças

- Adiciona `buscar_ncm()` para consultar uma NCM nos anexos tarifários e retornar as ocorrências encontradas em formato padronizado.
- Adiciona `buscar_ato()` para consultar atos nos anexos tarifários, com suporte a busca por número do ato ou texto.
- Adiciona cache em memória por sessão para reutilizar o Anexo I processado em leituras repetidas.
- Adiciona `limpar_cache_tarifas()` para limpar manualmente o cache interno.
- Atualiza mensagens de processamento dos anexos para deixar mais claro o que está sendo lido.
- Amplia a cobertura de testes para busca por NCM, busca por ato e cache do Anexo I.
- Atualiza o README com observações sobre ferramentas de compilação necessárias em algumas instalações.

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

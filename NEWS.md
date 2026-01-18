# tarifasimpbr 0.1.0

## Status

Esta é uma **versão experimental inicial** do pacote.  
A obtenção dos dados e o comportamento das funções podem sofrer ajustes em versões futuras, embora o formato atual dos dados de origem (planilhas Excel oficiais) seja considerado estável no momento.

---

## Principais funcionalidades

- Leitura automatizada dos anexos oficiais tarifários disponíveis em https://www.gov.br/mdic/pt-br/assuntos/camex/estrategia-comercial/arquivos-listas/16-01-2026-anexos-i-a-x-resolucao-gecex-272-21.xlsx. Inclui leitura da Tarifa Externa Comum (TEC) processada e otimizada para exploração de dados e consultas. Além disso, é possível ler tanto o anexo ii como as demais listas de exceção.

- Funções principais:
  - `ler_anexo()`: leitura padronizada dos diferentes anexos.
  - `detalhar_listas_excecao_vigentes()`: traz os códigos NCM presentes nas listas de exceção. Porém, os códigos presentes que ainda não entraram em vigor são excluídos. Também traz indicadores de contagem de quota e de destaques tarifários bem como indicadores de presença de quota, de destaques tarifários e de presença integral da ncm na lista de exceção.
  - `tarifas_aplicadas()`: geração de dados tarifários que incluem a TEC, a TEB e as tarifas aplicadas por NCM, além de indicadores de presença nas listas de exceção vigentes, classificação BK/BIT, quando for o caso, e descrição da TEC ao nível de NCM (8 dígitos) ou concatenada a partir de posição (4 dígitos) até NCM.

---

## Estrutura e estabilidade dos dados

O pacote usa o layout atual dos arquivos Excel oficiais publicados pela SE-Camex.  
Mudanças estruturais nesses arquivos poderão exigir mudanças na forma de processar os dados.

---

## Próximos passos planejados

- Melhoria na documentação.
- Eventuais correções e melhorias nas funções de leitura e processamento dos dados.

---

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

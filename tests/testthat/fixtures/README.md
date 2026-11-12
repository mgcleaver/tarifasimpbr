# Planilha sintetica de cache

`anexos-cache.xlsx` e uma planilha pequena criada exclusivamente para testes,
sem copiar dados da publicacao oficial. Contem os nove anexos anteriores ao XI:
I, II, III, IV, V, VI, VIII, IX e X (sem VII), dois produtos ficticios
e duas vigencias consecutivas no Anexo V. Os nomes seguem `listar_anexos()`.

Os testes leem os bytes do XLSX sintetico, verificam a equivalencia entre cache frio e quente
e mantem as vigencias futuras disponiveis para filtragem posterior. Os textos
de atos sao exemplos sinteticos; nao representam uma validacao da planilha
oficial de 19/10/2026.

`anexos-xi.xlsx` amplia essa base sintetica com tres produtos e o Anexo XI.
Tem seis linhas na nova aba: duas vigencias integrais consecutivas, um Ex
para a mesma NCM, uma NCM com quota, uma somente com Ex e outra com Ex e
quota. O cabecalho do XI fica na quinta linha, como na planilha de 11/11/2026.
As aliquotas, descricoes e codigos sao ficticios. A base anterior permanece
sem XI para testar compatibilidade historica.


# Datas e estilos de células

`datas-visuais.xlsx` é sintética e contém as abas V e XI com 18 registros
idênticos. Combina números com o formato curto interno 14, formatos
personalizados brasileiros e ISO, separadores literais e marca de localidade;
texto misto (inclusive inválido e com espaços), número Geral, serial como
texto, ausências, formato com nome de mês não suportado, fração de serial e
o dia fictício 29/02/1900. Colunas de origem conservam os cabeçalhos oficiais.

`datas-visuais-1904.xlsx` usa o sistema de datas 1904 e contém quatro linhas
no XI: serial 44870 formatado e Geral, texto "46332" e serial zero. As duas
primeiras linhas devem representar 06/11/2026; o texto mantém a convenção do
parser de textos, independente da base numérica da pasta.

As duas fixtures foram geradas com openpyxl 3.1.5, sem dados pessoais ou cópia
de planilhas oficiais. Os testes usam somente os XLSX; Python não é necessário
para executar o pacote ou os testes.

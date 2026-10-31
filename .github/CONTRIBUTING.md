# Desenvolvimento e validação

Na raiz do projeto, execute com uma versão do R que atenda ao requisito mínimo
do `DESCRIPTION` (R >= 4.2.0):

```sh
Rscript --vanilla tools/check.R --atualizar
```

O script instala as dependências declaradas no `DESCRIPTION`, atualiza os pacotes
da biblioteca de teste e executa `R CMD build` e `R CMD check --no-manual`.
A biblioteca é criada em `.r-check/R-<versão>/library`, sem usar ou atualizar
os pacotes da sua biblioteca pessoal. Nenhum pacote adicional de desenvolvimento
é necessário. Os logs, `sessionInfo.txt` e `dependencias.csv` ficam em
`.r-check/R-<versão>/` e não entram no Git nem no pacote distribuído.

Nas próximas verificações, omita `--atualizar` para reutilizar as versões
instaladas. Para conferir qual R será usado pelo terminal, execute
`Rscript --version`. No Windows, se ainda aparecer uma versão antiga, use o
caminho completo do executável no PowerShell:

```powershell
& 'C:\Program Files\R\R-4.6.2\bin\Rscript.exe' --vanilla tools/check.R --atualizar
```

A CI do GitHub testa R 4.2.0 (a versão mínima) e `oldrel-1` (a versão estável
anterior) no Linux, e `release` (a versão estável mais recente) no Windows, Linux
e macOS, sempre com as dependências atuais. As verificações rodam em pushes para
`main`/`master`, pull requests, manualmente e semanalmente às segundas-feiras, às
12h UTC. Assim, novas versões de R e das dependências continuam sendo verificadas
sem precisar editar a matriz a cada lançamento do R. O GitHub, porém, desativa
agendamentos em repositórios públicos após 60 dias sem atividade; nesse caso,
reative o workflow na aba Actions.

O requisito no `DESCRIPTION` define a versão mínima; a evidência de
compatibilidade vem dos testes e dos resultados da CI. Se o teste de R 4.2.0
falhar porque uma dependência passou a exigir R mais recente, suba o requisito
no `DESCRIPTION` e a versão mínima na matriz do workflow.

Os testes de integração consultam o site oficial e são pulados quando ele está
indisponível. Consulte os testes pulados no log antes de considerar validada a
integração com os dados oficiais. O ambiente registra as versões usadas, mas não
as congela: `--atualizar` consulta o CRAN, e a CI acompanha as versões atuais das
dependências.

[README.md](https://github.com/user-attachments/files/31710844/README.md)
# SWAT–CE-QUAL-W2

Aplicativo desenvolvido em R Shiny para apoiar o acoplamento de dados do
SWAT com o CE-QUAL-W2, executar simulações, calcular métricas de desempenho
e realizar otimizações de parâmetros.

## Funcionalidades

- Leitura e preparação de resultados do SWAT.
- Seleção de um arquivo de controle `.npt` do CE-QUAL-W2.
- Simulações hidrodinâmicas e de qualidade da água.
- Execução padrão ou dividida em janelas de monitoramento.
- Cálculo de RMSE, MAE, Bias, NSE e Skill de Persistência.
- Otimização multiobjetivo com MOPSO-CD.
- Geração de gráficos e cópia organizada dos resultados.
- Monitoramento do processo do CE-QUAL-W2.
- Encerramento automático do modelo quando um erro é detectado.
- Penalização de execuções inválidas durante a otimização.
- Proteção para arquivos abertos ou bloqueados no Windows.

## Requisitos

O aplicativo foi preparado para execução no Windows.

- R 4.5 ou versão compatível.
- PowerShell disponível no sistema.
- Permissão para executar os programas do CE-QUAL-W2.
- Acesso à internet na primeira instalação dos pacotes do R.

Os executáveis esperados na pasta `bin` são:

- `W2Pre3.7.exe`
- `w2_3.7_64.exe`

Antes de redistribuí-los, verifique as condições de licença do CE-QUAL-W2.

## Instalação

1. Baixe ou clone este repositório.
2. Abra o R na pasta do projeto.
3. Instale as dependências executando:

```r
source("loadPackages.R")
```

Os pacotes serão instalados em `R-library`, dentro do próprio projeto.
O pacote `mopsocd` é instalado a partir de:

```text
packages/mopsocd_0.5.1.tar.gz
```

## Execução

Pelo R:

```r
source("run_app.R")
```

Pelo PowerShell, a partir da pasta do projeto:

```powershell
Rscript .\run_app.R
```

Por padrão, o aplicativo fica disponível em:

```text
http://127.0.0.1:4891/
```

## Preparação dos dados do CE-QUAL-W2

No aplicativo, selecione diretamente o arquivo de controle `.npt`. A pasta
que contém esse arquivo passa a ser considerada o diretório da simulação.

Essa pasta deve conter os arquivos referenciados pelo arquivo de controle,
como batimetria, séries de entrada e demais dados exigidos pelo CE-QUAL-W2.

Também deve existir uma planilha de dados observados com o padrão:

```text
dados_obs_<sigla>.xlsx
```

A planilha deve possuir uma aba com o mesmo nome:

```text
dados_obs_<sigla>
```

Exemplo para o reservatório Edson Queiroz:

```text
Arquivo: dados_obs_edq.xlsx
Aba:     dados_obs_edq
```

## Simulação

O fluxo de uma simulação é:

1. Leitura e validação dos arquivos de entrada.
2. Preparação dos arquivos utilizados pelo CE-QUAL-W2.
3. Execução opcional do pré-processador.
4. Execução monitorada do CE-QUAL-W2.
5. Leitura dos arquivos de resultado.
6. Cálculo das métricas.
7. Geração de gráficos e cópia dos resultados.

No modo por janelas, o processo é repetido para cada intervalo definido
pelos dados de monitoramento. Os resultados são gravados na subpasta
`Janela`.

## Otimização

A otimização utiliza o pacote MOPSO-CD com `opt = 0`, correspondente à
minimização.

Para permitir a maximização de NSE e Skill, os valores válidos dessas
métricas são multiplicados por `-1` antes de serem entregues ao algoritmo.

Execuções inválidas recebem a penalidade:

```text
1e+06
```

A penalidade é aplicada quando:

- O CE-QUAL-W2 gera `W2Errordump.opt`.
- O processo termina com erro.
- Os arquivos de resultado não são atualizados.
- Os resultados não podem ser lidos ou são inválidos.
- Uma métrica retorna `NA`, `Inf` ou `-Inf`.

Após a penalização, a otimização continua com o próximo conjunto de
parâmetros.

## Monitoramento do CE-QUAL-W2

O arquivo `executar_cequal_monitorado.ps1` acompanha cada execução.

O monitor:

- Detecta um novo `W2Errordump.opt`.
- Verifica se os arquivos de saída continuam sendo atualizados.
- Fecha automaticamente a janela após o término ou erro.
- Impõe um limite de inatividade de 60 segundos.
- Impõe um limite máximo de 6 horas para uma única execução.
- Impede o uso de resultados antigos como se fossem da execução atual.

Os detalhes da última execução são registrados em:

```text
execucao_cequal_monitorada.log
```

## Arquivos bloqueados

No Windows, arquivos abertos no Excel ou em outro programa podem ficar
bloqueados.

- Para gravação, o aplicativo aguarda o arquivo ser fechado e depois
  continua automaticamente.
- Para leitura, o aplicativo informa qual arquivo está bloqueado. Feche o
  arquivo e inicie novamente a operação.

## Estrutura principal

```text
.
├── app_swat_cequal.R
├── contexto_simulacao.R
├── loadPackages.R
├── R/
│   ├── contexto_simulacao.R
│   ├── FUNC_simalation_calibration_otimizacao_CEQUALW2.R
│   ├── io_seguro.R
│   ├── ReadFile_cio_SWAT.R
│   └── ReadFile_Rch_SWAT.R
├── scripts/
│   ├── executar_cequal_monitorado.ps1
│   ├── selecionar_arquivo_npt_windows.ps1
│   └── selecionar_pasta_windows.ps1
├── packages/
│   └── mopsocd_0.5.1.tar.gz
├── run_app.R
├── bin/
│   ├── W2Pre3.7.exe
│   └── w2_3.7_64.exe
└── exemplo/
    └── w2_conR.npt
```

## Arquivos que não devem ser publicados

Dados locais, bibliotecas instaladas e resultados não devem fazer parte do
repositório. Recomenda-se incluir no `.gitignore`:

```gitignore
Reservatorios/
R-library/
.Rhistory
.RData
.Rproj.user/
Rplots.pdf
*.log
resultados_tipo*/
Resultados_tipo*/
```

## Engenharia Reversa

Os componentes da Engenharia Reversa estao separados por responsabilidade:

```text
R/engenharia_reversa/
|-- core.R
|-- dados.R
|-- api.R
|-- cequal_exportacao.R
`-- shiny.R
```

A base utilizada em tempo de execucao permanece em
`dados_engenharia_reversa/`. Para reconstruir essa base a partir dos arquivos
mestres, use a ferramenta administrativa:

```powershell
Rscript .\scripts\engenharia_reversa\preparar_base_engenharia_reversa.R DIR_ORIGEM DIR_DESTINO
```

Esse script nao e carregado durante a execucao normal do aplicativo.

## Arquivos legados

Copias antigas que nao participam da execucao foram preservadas em:

```text
arquivo/
|-- duplicatas_raiz/
|   |-- FUNC_simalation_calibration_otimizacao_CEQUALW2.R
|   `-- ReadFile_cio_SWAT.R
`-- legado/
    |-- app_swat_cequal-NOTEBOOK-DC.R
    `-- -NOTEBOOK-DC.Rhistory
```

As versoes ativas dos dois scripts duplicados ficam em `R/`. A pasta
`Reservatorios/` nao faz parte desta reorganizacao.

## Problemas comuns

### O aplicativo não encontra o arquivo `.npt`

Selecione novamente o arquivo de controle pelo botão do aplicativo. Não é
necessário renomeá-lo para `w2_conR.npt`.

### Uma planilha não pode ser aberta

Feche o arquivo no Excel e verifique se ele está disponível localmente,
especialmente quando estiver armazenado no OneDrive.

### O CE-QUAL-W2 apresentou erro

Consulte:

```text
W2Errordump.opt
execucao_cequal_monitorada.log
```

Em uma otimização, a execução inválida será penalizada e o processamento
continuará.

### Avisos sobre pacotes compilados em outra versão do R

Esses avisos normalmente não impedem a execução. Para removê-los, reinstale
os pacotes utilizando a mesma versão do R instalada no computador.

## Privacidade

Não publique planilhas, arquivos de reservatórios, resultados ou outros
dados que contenham informações restritas. A pasta `Reservatorios` deve
permanecer somente no computador do usuário, salvo quando houver autorização
expressa para sua distribuição.

library(tidyverse)
library(lubridate)

## Nesta atividade você deve utilizar o resultado do exercício 01 da Atividade da aula 03 (remuneração em dólares convertida para reais)
## Utilize o código daquele exercício como ponto de partida para esta atividade. 
## Sempre utilize o caminho relativo, não o caminho absoluto, pois não funcionará na correção do exercício.

### IMPORTANTE ###
## Se você utilizar alguma função própria ou do material de aula, o código da(s) função(ões) deve estar neste arquivo da atividade.
salarios <- read_csv("aula-03/data/201802_dados_salarios_servidores.csv.gz")

salarios %>%
  mutate(REMUNERACAO_FINAL = REMUNERACAO_REAIS + round((REMUNERACAO_DOLARES * 3.2421), digits = 2)) %>%
  filter(REMUNERACAO_FINAL >= 900) -> salarios

salarios %>%
  View()

### 1 ####
## 
## Correlação de ano de ingresso por cargo
## - Determine o coeficiente de correlação entre o tempo em anos desde a DATA_INGRESSO_ORGAO e o tempo em anos desde a DATA_DIPLOMA_INGRESSO_SERVICOPUBLICO
##   para todos os cargos que possuem no mínimo 200 servidores.
## - Crie uma coluna que determina se a correlação é positiva ou negativa, e outra coluna que define a força da correlação de acordo com 
##   o material visto em aula sobre interpretação do coeficiente.
## - O resultado desta atividade deve ser um Data Frame com as variáveis de Cargo, Coeficiente de Correlação, Direção da Correlação e Força da Correlação
## 
### # ####
salarios %>%
  group_by(DESCRICAO_CARGO) %>%
  summarise(
    QTD = n(),
    CORRELACAO_COEFICIENTE = cor(x = year(DATA_INGRESSO_ORGAO), y = year(DATA_DIPLOMA_INGRESSO_SERVICOPUBLICO )),
    CORRELACAO_DIRECAO = if_else(sign(CORRELACAO_COEFICIENTE) == 1, 'POSITIVA', 'NEGATIVA'),
    CORRELACAO_FORCA = case_when(
      abs(CORRELACAO_COEFICIENTE) >= 0.0 & abs(CORRELACAO_COEFICIENTE) < 0.3 ~ 'DESPREZIVEL',
      abs(CORRELACAO_COEFICIENTE) >= 0.3 & abs(CORRELACAO_COEFICIENTE) < 0.5 ~ 'FRACA',
      abs(CORRELACAO_COEFICIENTE) >= 0.5 & abs(CORRELACAO_COEFICIENTE) < 0.7 ~ 'MODERADA',
      abs(CORRELACAO_COEFICIENTE) >= 0.7 & abs(CORRELACAO_COEFICIENTE) < 0.9 ~ 'FORTE',
      abs(CORRELACAO_COEFICIENTE) >= 0.9 ~ 'MUITO FORTE')
  ) %>%
  filter(QTD >= 200) %>%
  ungroup() %>%
  select(-QTD) -> salarios_correlacao

salarios_correlacao %>%
  View()

### 2 ###
##
## - A partir do dataset do exercício anterior, selecione os 10 cargos de correlação mais forte (seja positiva ou negativa) e os 
##   10 cargos de correlação mais fraca (de novo, independente de ser positiva ou negativa)
## - Para estes 20 cargos, determine a Moda do órgão de lotação (ORGSUP_LOTACAO) e de exercício (ORGSUP_EXERCICIO)
## - Reponda se existe diferença entre as modas e se existe relação entre a Força da Correlação e a diferença entre as modas 
##   (caso haja diferença)
##
### # ###
rbind(
  salarios_correlacao %>%
    arrange(abs(CORRELACAO_COEFICIENTE)) %>%
    head(n = 10),
  salarios_correlacao %>%
    arrange(abs(CORRELACAO_COEFICIENTE)) %>%
    tail(n = 10)
) %>%
pull(DESCRICAO_CARGO) -> cargos_20

merge(
  merge(
    salarios %>%
      filter(DESCRICAO_CARGO %in% cargos_20) %>%
      group_by(DESCRICAO_CARGO, ORGSUP_LOTACAO) %>%
      summarise(
        MODA_LOTACAO = n()
      ) %>%
      mutate(QTD_MAX = max(MODA_LOTACAO)) %>%
      ungroup() %>%
      filter(QTD_MAX == MODA_LOTACAO) %>%
      select(-QTD_MAX),
    salarios %>%
      filter(DESCRICAO_CARGO %in% cargos_20) %>%
      group_by(DESCRICAO_CARGO, ORGSUP_EXERCICIO) %>%
      summarise(
        MODA_EXERCICIO = n()
      ) %>%
      mutate(QTD_MAX = max(MODA_EXERCICIO)) %>%
      ungroup() %>%
      filter(QTD_MAX == MODA_EXERCICIO) %>%
      select(-QTD_MAX),
    by = "DESCRICAO_CARGO"
  ),
  salarios_correlacao,
  by = "DESCRICAO_CARGO"
) %>%
View()

# Para a maioria dos casos em que a força da correlação é forte, a moda dos órgãos se mantém a mesma, tanto para lotação quanto para exercício.
# Enquanto que para a maioria dos casos em que a força da correlação é desprezível, a moda dos órgãos tende a ser diferente de lotação para exercício.
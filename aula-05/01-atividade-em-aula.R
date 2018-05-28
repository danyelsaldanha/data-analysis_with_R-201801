# Carregue a biblioteca tidyverse. Lembre que outras bibliotecas serão carregadas junto ao tidyverse
library(tidyverse)
library(lubridate)


# Crie um dataframe com o conteúdo do arquivo ted_main.csv.gz. 
ted <- read_csv("aula-05/data/ted_main.csv.gz")



# Visualize o resumo dos dados do dataframe. Verifique os mínimos, máximos, médias e medianas das variáveis numéricas.
ted %>%
  View()

summary(ted)

# As variáveis duration, film_date e published_date estão no tipo de dados apropriado?
## duration está, film_date e published_date não.

# Converta as seguintes variáveis utilizando o pacote Lubridate:
#     * duration, para duração (em segundos). Experimente utilizar as funções as.duration e duration. Mantenha aquela que considerar mais apropriada.
#     * film_date, para data, com a função as_datetime.
#     * published_date, para data, com a função as_datetime..
ted %>%
  mutate(duration = as.duration(duration)) %>%
  mutate(film_date = as_datetime(film_date)) %>%
  mutate(published_date = as_datetime(published_date)) -> ted

# Converta as seguintes variáveis character para variáveis categóricas com a função factor.
#     * event
#     * speaker_occupation
ted %>%
  mutate(event = factor(event)) %>%
  mutate(speaker_occupation = factor(speaker_occupation)) -> ted

# Retire do dataframe a variável name
ted %>%
  select(-name) -> ted

# Visualize novamente o resumo dos dados do dataframe. Verifique os mínimos, máximos, médias e medianas das variáveis numéricas. Verifique as contagens das variáveis categóricas
summary(ted)

# Verifique quais registros possuem a menor quantidade de línguas. Corrija para que possuam no mínimo 1 idioma.
ted %>%
  mutate(languages = replace(languages, languages == 0, 1)) -> ted

# Verifique os 15 registros com menor data de filmagem. 
ted %>%
  arrange(film_date, event) %>%
  head(15) %>%
  View()

# Crie um dataframe com a contagem de apresentações por ano de filmagem e visualize todo o seu conteúdo
ted %>%
  mutate(film_year = year(film_date)) %>%
  group_by(film_year) %>%
  summarise(
    qty = n()
  ) %>%
  ungroup() -> ted_count

# Analise os 10 quantis da quantidade de apresentações por ano.
# Descarte, do data frame de apresentações do TED Talks, aqueles cujo ano de filmagem tiver quantidade de apresentações menor ou igual à quantidade do quarto quantil.
quantile(ted_count$qty, probs = seq(0, 1, 0.1))

quantile(ted_count$qty, probs = seq(0, 1, 0.1))[5] -> qty_quantil

ted %>%
  group_by(film_year = year(film_date)) %>%
  mutate(qty = n()) %>%
  filter(qty > qty_quantil) -> ted

# Verifique novamente o resumo dos dados do dataframe
summary(ted)

# Verifique os 10 registros com maior duração.
ted %>%
  arrange(desc(duration)) %>%
  head(n = 10) %>%
  View()

# Existem apresentações com duração maior que 3 desvios padrão acima da média? Liste elas
ted %>%
  summarise(
    desvio_padrao = as.duration(sd(duration))
  ) -> duracao_med_sd

ted %>%
  filter(duration > duracao_med_sd$desvio_padrao * 3) %>%
  View()

# Calcule os 4 quartis e o IQR da duração das apresentações. Liste as apresentações cuja duração supera 1.5 * o IQR + o terceiro quartil
quantile(ted$duration, probs = seq(0, 1, 0.25))

IQR(ted$duration)

ted %>%
  filter(duration > (1.5 * IQR(duration) + quantile(duration, probs = seq(0, 1, 0.25))[3])) %>%
  View()

# Visualize os 10 quantis da quantidade de visualizações
quantile(ted$views, probs = seq(0, 1, 0.1)) %>%
  View()

# Compare as seguintes estatísticas descritivas da quantidade de visualizações:
#   * Média e Mediana. Qual é maior?
#       Média
#   * Desvio Absoluto da Mediana e Desvio Padrão. Qual é maior?
#       Desvio padrão
#   * Desvio Absoluto da Mediana e IQR. Quantas vezes o IQR é maior que o Desvio Absoluto da Mediana?
#       Mais de duas vezes
#   * Com base na média e na mediana, e na razão entre o IQR e o Desvio Absoluto da Mediana, 
#     você conclui que as quantidades de visualização estão distribuidas de forma simétrica em torno da média?
#       Não
ted %>%
  summarise(
    media = mean(views),
    mediana = median(views),
    desvio_padrao = sd(views),
    desvio_absoluto_mediana = median(abs(views - median(views))),
    iqr = IQR(views)
  ) %>%
  View()

# Calcule a média, o desvio padrão, a mediana e o IQR da quantidade de línguas dos seguintes grupos:
#     * 10% de vídeos com maior número de visualizações
ted %>%
  filter(views >= quantile(ted$views, probs = seq(0, 1, 0.1))[10]) %>%
  summarise(
    grupo = 'Menos visualizações',
    media = mean(languages),
    desvio_padrao = sd(languages),
    mediana = median(languages),
    iqr = IQR(languages)
  ) %>%
  View()

#     * 10% de vídeos com menor número de visualizações
ted %>%
  filter(views <= quantile(ted$views, probs = seq(0, 1, 0.1))[2]) %>%
  summarise(
    grupo = 'Menos visualizações',
    media = mean(languages),
    desvio_padrao = sd(languages),
    mediana = median(languages),
    iqr = IQR(languages)
  ) %>%
  View()

# Determine a quantidade de apresentações por evento cujo nome inicie com TED. Utilize a função str_detect para este filtro
ted %>%
  group_by(event) %>%
  summarise(qtd = n()) %>%
  filter(str_detect(event, 'TED')) %>%
  ungroup() %>%
  View()

# Determine, por evento cujo nome inicie com TED e que a quantidade de visualizações dos vídeos foi maior que a mediana calculada anteriormente.
#   * a quantidade de apresentações resultante do filtro, por evento
#   * o ano do evento (utilizar o menor ano da data de publicação)
#   * a quantidade média de línguas das apresentações
#   * o desvio padrão da quantidade de línguas
#   * o coeficiente de variação da quantidade de línguas
### EXIBA SOMENTE OS EVENTOS COM MAIS DE 10 APRESENTAÇÕES
ted %>%
  filter(str_detect(event, 'TED') & views > median(views)) %>%
  group_by(event) %>%
  summarise(
    qtd_apresentacoes = n(),
    ano = min(year(published_date)),
    media_linguas = mean(languages),
    desvio_padrao = sd(languages),
    coeficiente_variacao = desvio_padrao / media_linguas
  ) %>%
  ungroup() %>%
  filter(qtd_apresentacoes > 10) %>%
  View()

# Calcule e classifique as seguintes correlações
#     * Quantidade de visualizações e Quantidade de línguas
#     * Quantidade de visualizações e Duração
#     * Quantidade de visualizações e Quantidade de Comentários
#     * Quantidade de Comentários e Quantidade de línguas
ted %>%
  summarise(
    views_linguas = cor(x = views, y = languages),
    views_linguas_classificacao = case_when(
      abs(views_linguas) >= 0.0 & abs(views_linguas) < 0.3 ~ 'DESPREZIVEL',
      abs(views_linguas) >= 0.3 & abs(views_linguas) < 0.5 ~ 'FRACA',
      abs(views_linguas) >= 0.5 & abs(views_linguas) < 0.7 ~ 'MODERADA',
      abs(views_linguas) >= 0.7 & abs(views_linguas) < 0.9 ~ 'FORTE',
      abs(views_linguas) >= 0.9 ~ 'MUITO FORTE'),
    views_duracao = cor(x = views, y = duration),
    views_duracao_classificacao = case_when(
      abs(views_duracao) >= 0.0 & abs(views_duracao) < 0.3 ~ 'DESPREZIVEL',
      abs(views_duracao) >= 0.3 & abs(views_duracao) < 0.5 ~ 'FRACA',
      abs(views_duracao) >= 0.5 & abs(views_duracao) < 0.7 ~ 'MODERADA',
      abs(views_duracao) >= 0.7 & abs(views_duracao) < 0.9 ~ 'FORTE',
      abs(views_duracao) >= 0.9 ~ 'MUITO FORTE'),
    views_comentarios = cor(x = views, y = comments),
    views_comentarios_classificacao = case_when(
      abs(views_comentarios) >= 0.0 & abs(views_comentarios) < 0.3 ~ 'DESPREZIVEL',
      abs(views_comentarios) >= 0.3 & abs(views_comentarios) < 0.5 ~ 'FRACA',
      abs(views_comentarios) >= 0.5 & abs(views_comentarios) < 0.7 ~ 'MODERADA',
      abs(views_comentarios) >= 0.7 & abs(views_comentarios) < 0.9 ~ 'FORTE',
      abs(views_comentarios) >= 0.9 ~ 'MUITO FORTE'),
    comentarios_linguas = cor(x = comments, y = languages),
    comentarios_linguas_classificacao = case_when(
      abs(comentarios_linguas) >= 0.0 & abs(comentarios_linguas) < 0.3 ~ 'DESPREZIVEL',
      abs(comentarios_linguas) >= 0.3 & abs(comentarios_linguas) < 0.5 ~ 'FRACA',
      abs(comentarios_linguas) >= 0.5 & abs(comentarios_linguas) < 0.7 ~ 'MODERADA',
      abs(comentarios_linguas) >= 0.7 & abs(comentarios_linguas) < 0.9 ~ 'FORTE',
      abs(comentarios_linguas) >= 0.9 ~ 'MUITO FORTE')
  ) %>%
  View()

# Descarte os vídeos cuja duração seja maior que 3 desvios padrões da média. Calcule novamente as 5 correlações solicitadas
ted %>%
  summarise(
    media = as.duration(mean(duration)),
    desvio_padrao = as.duration(sd(duration))
  ) -> duracao_med_sd

ted %>%
  filter(duration <= duracao_med_sd$desvio_padrao * 3) -> ted_menor_3_sd

ted_menor_3_sd %>%
  summarise(
    views_linguas = cor(x = views, y = languages),
    views_linguas_classificacao = case_when(
      abs(views_linguas) >= 0.0 & abs(views_linguas) < 0.3 ~ 'DESPREZIVEL',
      abs(views_linguas) >= 0.3 & abs(views_linguas) < 0.5 ~ 'FRACA',
      abs(views_linguas) >= 0.5 & abs(views_linguas) < 0.7 ~ 'MODERADA',
      abs(views_linguas) >= 0.7 & abs(views_linguas) < 0.9 ~ 'FORTE',
      abs(views_linguas) >= 0.9 ~ 'MUITO FORTE'),
    views_duracao = cor(x = views, y = duration),
    views_duracao_classificacao = case_when(
      abs(views_duracao) >= 0.0 & abs(views_duracao) < 0.3 ~ 'DESPREZIVEL',
      abs(views_duracao) >= 0.3 & abs(views_duracao) < 0.5 ~ 'FRACA',
      abs(views_duracao) >= 0.5 & abs(views_duracao) < 0.7 ~ 'MODERADA',
      abs(views_duracao) >= 0.7 & abs(views_duracao) < 0.9 ~ 'FORTE',
      abs(views_duracao) >= 0.9 ~ 'MUITO FORTE'),
    views_comentarios = cor(x = views, y = comments),
    views_comentarios_classificacao = case_when(
      abs(views_comentarios) >= 0.0 & abs(views_comentarios) < 0.3 ~ 'DESPREZIVEL',
      abs(views_comentarios) >= 0.3 & abs(views_comentarios) < 0.5 ~ 'FRACA',
      abs(views_comentarios) >= 0.5 & abs(views_comentarios) < 0.7 ~ 'MODERADA',
      abs(views_comentarios) >= 0.7 & abs(views_comentarios) < 0.9 ~ 'FORTE',
      abs(views_comentarios) >= 0.9 ~ 'MUITO FORTE'),
    comentarios_linguas = cor(x = comments, y = languages),
    comentarios_linguas_classificacao = case_when(
      abs(comentarios_linguas) >= 0.0 & abs(comentarios_linguas) < 0.3 ~ 'DESPREZIVEL',
      abs(comentarios_linguas) >= 0.3 & abs(comentarios_linguas) < 0.5 ~ 'FRACA',
      abs(comentarios_linguas) >= 0.5 & abs(comentarios_linguas) < 0.7 ~ 'MODERADA',
      abs(comentarios_linguas) >= 0.7 & abs(comentarios_linguas) < 0.9 ~ 'FORTE',
      abs(comentarios_linguas) >= 0.9 ~ 'MUITO FORTE')
  ) %>%
  View()

# Utilizando o data frame original, crie um dataframe com a mediana da duração dos vídeos por ano de filmagem. Calcule a correlação entre o ano e a mediana da duração
# e interprete o resultado
ted %>%
  group_by(ano = year(film_date)) %>%
  summarise(mediana = median(duration)) %>%
  ungroup() -> ted_mediana_ano

ted_mediana_ano %>%
  summarise(correlacao = cor(x = ano, y = mediana)) %>%
  View()
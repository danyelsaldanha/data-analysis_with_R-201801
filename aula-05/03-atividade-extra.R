# Carregue as bibliotecas utilizadas
if (!"Hmisc" %in% installed.packages()) install.packages("Hmisc")
if (!"ggcorrplot" %in% installed.packages()) install.packages("ggcorrplot")
library(tidyverse)
library(lubridate)
library(magrittr)
library(Hmisc)

# Crie um dataframe com o conteúdo do arquivo ted_main.csv.gz.
ted <- read_csv("aula-05/data/ted_main.csv.gz")

ted %>%
  mutate(duration = as.duration(duration)) %>%
  mutate(film_date = as_datetime(film_date)) %>%
  mutate(published_date = as_datetime(published_date)) -> ted

ted %>%
  View()


# Crie um histograma da quantidade de visualizações multifacetado por ano de publicação, restrito aos
# anos entre 2012 e 2017.

ted %>%
  mutate(ano = year(published_date)) %>%
  filter(ano >= 2012 & ano <= 2017) %>%
  ggplot(aes(x = views)) +
  scale_x_continuous(labels = scales::format_format(big.mark = ".", decimal.mark=",", scientific = FALSE)) +
  geom_histogram(breaks = seq(from = 50000, to = 50000000, by = 500000)) +
  facet_wrap (~ ano, ncol = 3) +
  labs(x = "Quantidade de visualizações"
      ,title = "Visualizações por Ano" ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

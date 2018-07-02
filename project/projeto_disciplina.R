# Componentes: Danyel Saldanha, Luciano Silveira e Ronaldo Bernardi

# Descrição dos dados: https://tech.instacart.com/3-million-instacart-orders-open-sourced-d40d29ead6f2
# Estamos trabalhando com somente uma amostra do total de pedidos. O dataset abaixo não possui 3 milhões de pedidos ;)
library( tidyverse )

departments <- read_csv("project/departments.csv")                   # Cadastro de Departamentos
aisles <- read_csv("project/aisles.csv")                             # Cadastro de "Corredores"
products <- read_csv("project/products.csv")                         # Cadastro de Produtos

insta_orders <- read_csv( "project/orders_instacart.csv" )           # Amostra de pedidos de usuários
insta_products <- read_csv( "project/order_products_instacart.csv" ) # Produtos que compõe os pedidos


#1 # Quantos dos produtos do cadastro nunca foram comprados?

product_id <- products$product_id

insta_products %>%
    group_by(product_id) %>%
    summarise(count = n()) %>%
    ungroup() %>%
    pull(product_id) -> orders_product_id

notordered_product_id <- setdiff(product_id,orders_product_id)

paste(length(notordered_product_id),'produtos nunca foram comprados.')


#2 # Crie um dataframe com os dados combinados de produtos, corredores e departamentos.

products %>%
    inner_join(aisles, by = 'aisle_id') %>%
    inner_join(departments, by = 'department_id') -> products_aisles_departments


#3 # Quais as 10 combinações corredor + departamento que possuem mais produtos cadastrados? Use o dataframe da atividade #2.

products_aisles_departments %>%
    group_by(aisle_id, aisle, department_id, department) %>%
    summarise(count = n()) %>%
    arrange(desc(count)) %>%
    head(count, n = 10) %>%
    ungroup() -> top10_products_aisles_departments


#4 # Qual o percentual de pedidos que possuem algum produto dos pares 'corredor + departamento' da atividade anterior?

insta_products %>%
    inner_join(products, by = 'product_id') %>%
    inner_join(top10_products_aisles_departments, by = c('aisle_id', 'department_id')) %>%
    group_by(order_id) %>%
    summarise(count = n()) %>%
    ungroup() %>%
    pull(order_id) -> orders_top10_products_aisles_departments

insta_products %>%
    group_by(order_id) %>%
    summarise(count = n()) %>%
    ungroup() %>%
    pull(order_id) -> orders_order_id

perc_orders_aisles_departments <- (length(orders_top10_products_aisles_departments) * 100) / length(orders_order_id)


#5 # Crie um novo dataframe de produtos em pedidos retirando aqueles produtos que não estão categorizados (usar resultado das atividades 3 e 4)

insta_products %>%
    inner_join(products, by = 'product_id') %>%
    inner_join(top10_products_aisles_departments, by = c('aisle_id', 'department_id'))%>%
    filter(aisle != 'missing' & department != 'missing') %>%
    select(order_id, product_id) -> categorized_insta_products


#6 # Crie um dataframe que combine todos os dataframes através das suas chaves de ligação. Para produtos de pedidos, use o dataframe da atividade 4

# Transforme as variáveis user_id, department e aisle em factor
# Transforme a variável order_hour_of_day em um factor ordenado (ordered)

# Este dataframe deverá ser utilizado em todas as atividades seguintes

insta_products %>%
    inner_join(products, by = 'product_id') %>%
    inner_join(top10_products_aisles_departments, by = c('aisle_id', 'department_id')) %>%
    inner_join(insta_orders, by = 'order_id') %>%
    select(order_id,
           user_id,
           order_number,
           order_dow,
           order_hour_of_day,
           days_since_prior_order,
           product_id,
           product_name,
           aisle_id,
           aisle,
           department_id,
           department) %>%
    mutate(user_id = factor(user_id),
           department = factor(department),
           aisle = factor(aisle),
           order_hour_of_day = ordered(order_hour_of_day)) -> top10_insta_orders

summary(top10_insta_orders)


#7 # Identifique os 5 horários com maior quantidade de usuários distintos que fizeram pedidos

top10_insta_orders %>%
    group_by(order_hour_of_day) %>%
    summarise(count_user_id = n_distinct(user_id)) %>%
    arrange(desc(count_user_id)) %>%
    head(n = 5) %>%
    ungroup() -> top5_hour_of_day


#8 # Quais os 15 produtos mais vendidos nestes 5 horários? Identifique os produtos e a quantidade total nestes horários (total geral, não por hora)

top10_insta_orders %>%
    inner_join(top5_hour_of_day, by = 'order_hour_of_day') %>%
    group_by(product_id, product_name) %>%
    summarise(qtd = n()) %>%
    arrange(desc(qtd)) %>%
    head(15) -> top5_products


#9 # Calcule a média de vendas por hora destes 15 produtos ao longo do dia,
# e faça um gráfico de linhas mostrando a venda média por hora destes produtos. 
# Utilize o nome do produto para legenda da cor da linha.
# Você consegue identificar algum produto com padrão de venda diferente dos demais?

top5_products %>%
    inner_join(top10_insta_orders, by = 'product_id') %>%
    inner_join(products, by = 'product_id') %>%
    group_by(product_id, product_name, order_hour_of_day, order_dow) %>%
    summarise(qtd_hora = n()) %>%
    ungroup() %>%
    group_by(product_id, product_name, order_hour_of_day) %>%
    summarise(media_qtd_hora = mean(qtd_hora)) %>%
    ungroup() -> orders_product_x_hour

library(ggplot2)

ggplot(
    data = orders_product_x_hour,
    aes(x = order_hour_of_day,
        y = media_qtd_hora,
        group = product_name)
    ) +
    geom_line(aes(color = product_name)) +
    geom_point(aes(color = product_name)) +
    scale_y_continuous(breaks = seq(from = 0, to = 30, by = 2)) +
    labs(x = 'Hora',
         y = 'Quantidade',
         title = 'Top 15 Produtos por Hora',
         colour = 'Produto')

#O produto Organic Whole String Cheese teve um pico de compras às 16h bem mais alto que qualquer outro produto.


#10 # Calcule as seguintes estatísticas descritivas sobre a quantidade de pedidos por dia, para cada hora do dia. O resultado final deve ser exibido para cada hora do dia:
# Média, Desvio Padrão, Mediana, Mínimo e Máximo
# Considerando os valores calculados, você acredita que a distribuição por hora é gaussiana?

top10_insta_orders %>%
    group_by(order_dow, order_hour_of_day) %>%
    summarise(qtd_hora = n_distinct(order_id)) %>%
    group_by(order_hour_of_day) %>%
    summarise(media = mean(qtd_hora),
              desvio_padrao = sd(qtd_hora),
              mediana = median(qtd_hora),
              minimo = min(qtd_hora),
              maximo = max(qtd_hora)) %>%
    ungroup()

#A distribuição é gaussiana, pois o volume de compras vai aumentando à medida que as horas passam,
#chega em um pico máximo (por volta das 14h) e depois começa a diminuir novamente.


#11 # Faça um gráfico da média de quantidade de produtos por hora, com 1 desvio padrão para cima e para baixo em forma de gráfico de banda

top10_insta_orders %>%
    group_by(order_hour_of_day, product_id) %>%
    summarise(qtd_hora = n()) %>%
    ungroup() %>%
    group_by(order_hour_of_day) %>%
    mutate(hora = as.numeric(order_hour_of_day),
           sd_low = mean(qtd_hora) - 1 * sd(qtd_hora),
           sd_high = mean(qtd_hora) + 1 * sd(qtd_hora)) %>%
    ungroup() %>%
    ggplot( aes( x = hora, y = qtd_hora, ymin = sd_low, ymax = sd_high )) +
    geom_ribbon(aes(ymin = sd_low, ymax = sd_high), alpha = 0.3, col = "grey") +
    geom_jitter(alpha = .2, height = 0, width = 0.3) +
    scale_x_continuous( breaks = seq(from = 0, to = 24, by = 1)) +
    scale_y_continuous( breaks = seq(from = 0, to = 200, by = 25)) +
    labs(x = 'Hora', y = 'Produtos') +
    theme_bw()


#12 # Visualize um boxplot da quantidade de pedidos por hora nos 7 dias da semana. O resultado deve ter order_dow como eixo x.

top10_insta_orders %>%
    group_by(order_dow, order_hour_of_day) %>%
    summarise(qtd_hora = n()) %>%
    ungroup() %>%
    ggplot( aes( x = order_dow, y = qtd_hora, group = order_dow )) +
    geom_boxplot() +
    scale_x_continuous( breaks = seq(from = 0, to = 6, by = 1) ) +
    labs(x = 'Dia da Semana',
         y = 'Pedidos'
    ) +
    theme_bw()


#13 # Identifique, por usuário, o tempo médio entre pedidos

top10_insta_orders %>%
    group_by(user_id) %>%
    summarise(tempo_medio = mean(days_since_prior_order)) %>%
    ungroup()


#14 # Faça um gráfico de barras com a quantidade de usuários em cada tempo médio calculado

top10_insta_orders %>%
    group_by(user_id) %>%
    summarise(tempo_medio = mean(days_since_prior_order)) %>%
    ungroup() %>%
    group_by(tempo_medio) %>%
    summarise(usuarios = n()) %>%
    ungroup() %>%
    ggplot( aes( x = tempo_medio, y = usuarios )) +
    geom_col(fill="dark orange", alpha=0.6) +
    scale_y_continuous( breaks = seq(from = 0, to = 25000, by = 2500) ) +
    scale_x_continuous( breaks = seq(from = 0, to = 30, by = 1) ) +
    labs(x = 'Tempo Médio Entre Pedidos',
         y = 'Total de Usuários') +
    theme_bw()


#15 # Faça um gráfico de barras com a quantidade de usuários em cada número de dias desde o pedido anterior. Há alguma similaridade entre os gráficos das atividades 14 e 15? 

top10_insta_orders %>%
    group_by(days_since_prior_order) %>%
    summarise(count = n_distinct(user_id)) %>%
    ungroup() %>%
    ggplot( aes( x = days_since_prior_order, y = count )) +
    geom_col(fill="dark red", alpha=0.6) +
    scale_y_continuous( breaks = seq(from = 0, to = 25000, by = 2500) ) +
    scale_x_continuous( breaks = seq(from = 0, to = 30, by = 1) ) +
    labs(x = 'Dias desde o último pedido',
         y = 'Total de Usuários') +
    theme_bw()

#A aparência dos dois gráficos é muito parecida. Os valores de tempo médio por usuário e quantidade de usuários por tempo médio são muito parecidos.


#16 # Repita o gráfico da atividade 14 mantendo somente os usuários com no mínimo 5 pedidos. O padrão se mantém?

top10_insta_orders %>%
    group_by(user_id) %>%
    summarise(count = n()) %>%
    filter(count >= 5) %>%
    ungroup() -> users_5_orders

top10_insta_orders %>%
    inner_join(users_5_orders, by = 'user_id') %>%
    group_by(user_id) %>%
    summarise(tempo_medio = mean(days_since_prior_order)) %>%
    ungroup() %>%
    group_by(tempo_medio) %>%
    summarise(usuarios = n()) %>%
    ungroup() %>%
    ggplot( aes( x = tempo_medio, y = usuarios )) +
    geom_col(fill="dark green", alpha=0.6) +
    scale_y_continuous( breaks = seq(from = 0, to = 25000, by = 500) ) +
    scale_x_continuous( breaks = seq(from = 0, to = 30, by = 1) ) +
    labs(x = 'Tempo Médio Entre Pedidos',
         y = 'Total de Usuários') +
    theme_bw()

#Apesar de o número de usuários ser relativamente menor, o padrão se mantém.


#17 # O vetor abaixo lista todos os IDs de bananas maduras em seu estado natural.
# Utilizando este vetor, identifique se existem pedidos com mais de um tipo de banana no mesmo pedido.

bananas <- c(24852, 13176, 39276, 37067, 29259)

#Não foram encontrados pedidos com estes produtos no Top 10
top10_insta_orders %>%
    filter(product_id %in% bananas)

#Utilizando o dataframe original
products %>%
    filter(product_id %in% bananas) %>%
    inner_join(insta_products, by = 'product_id') %>%
    inner_join(insta_orders, by = 'order_id') %>%
    group_by(order_id) %>%
    summarise(count = n_distinct(product_id)) %>%
    filter(count > 1) %>%
    ungroup() %>%
    pull(order_id) -> orders_banana


#18 # Se existirem, pedidos resultantes da atividade 17, conte quantas vezes cada tipo de banana aparece nestes pedidos com mais de um tipo de banana.
# Após exibir os tipos de banana, crie um novo vetor de id de bananas contendo somente os 3 produtos de maior contagem de ocorrências

products %>%
    filter(product_id %in% bananas) %>%
    inner_join(insta_products, by = 'product_id') %>%
    filter(order_id %in% orders_banana) %>%
    group_by(product_id, product_name) %>%
    summarise(count = n()) %>%
    arrange(desc(count)) %>%
    head(n = 3) %>%
    pull(product_id) -> top3_bananas


#19 # Com base no vetor criado na atividade 18, conte quantos pedidos de, em média, são feitos por hora em cada dia da semana.

insta_orders %>%
    inner_join(insta_products, by = 'order_id') %>%
    filter(product_id %in% top3_bananas) %>%
    group_by(order_dow, order_hour_of_day) %>%
    summarise(count = n_distinct(order_id)) %>%
    ungroup()


#20 # Faça um gráfico dos pedidos de banana da atividade 19. O gráfico deve ter o dia da semana no eixo X, a hora do dia no eixo Y, 
# e pontos na intersecção dos eixos, onde o tamanho do ponto é determinado pela quantidade média de pedidos de banana 
# nesta combinação de dia da semana com hora

insta_orders %>%
    inner_join(insta_products, by = 'order_id') %>%
    filter(product_id %in% top3_bananas) %>%
    group_by(order_dow, order_hour_of_day, order_id) %>%
    ungroup() %>%
    ggplot( aes( x = order_dow, y = order_hour_of_day)) +
    geom_count() +
    scale_x_continuous( breaks = seq(from = 0, to = 6, by = 1)) +
    labs(x = 'Dia da Semana',
         y = 'Hora do Dia',
         size = 'Produtos') +
    theme_bw()


#21 # Faça um histograma da quantidade média calculada na atividade 19, facetado por dia da semana

insta_orders %>%
    inner_join(insta_products, by = 'order_id') %>%
    filter(product_id %in% top3_bananas) %>%
    group_by(order_dow, order_hour_of_day) %>%
    summarise(count = n_distinct(order_id)) %>%
    ungroup() %>%
    ggplot(aes(x = count)) +
    geom_histogram(breaks = seq(from = 0, to = 850, by = 5)) +
    facet_wrap(~order_dow, ncol = 2)


#22 # Teste se há diferença nas vendas por hora entre os dias 3 e 4 usando o teste de wilcoxon e utilizando a simulação da aula de testes

insta_orders %>%
    filter(order_dow %in% c(3, 4)) %>%
    group_by(order_dow, order_hour_of_day) %>%
    summarise(count = n()) %>%
    ungroup() -> orders_days

wilcox.test(count ~ order_dow, data = orders_days, alternative = "two.sided", conf.int = TRUE)

kruskal.test(count ~ order_dow, data = orders_days)

pairwise.wilcox.test(orders_days$count, orders_days$order_dow, p.adjust.method = "BH")
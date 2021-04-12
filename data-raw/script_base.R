
library(tidyverse)
movies <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-09/movies.csv')

# Analise Tidytuesday - dataset Bechdel
# A ideia é verificar se algumas caracteriticas dos filmes tem impacto no resultado do teste de Bechdel.
# Decidimos focar em caractesticas mais global que já essas impactam o filme como um todo
# As caracteristicas foram divididas em numéricas e categoricas

# Numercicas  -
# Categoricas -
# Resposta = binary

####### Analise das variavies numericas
movies_num = movies%>%
                  select(title, binary, budget_2013, domgross_2013,intgross_2013) %>%
                   mutate(domgross_2013 = as.numeric(domgross_2013),
                         intgross_2013 = as.numeric(intgross_2013),
                         lucro = intgross_2013 -budget_2013) %>%
                  filter(across(
                    .cols = everything(),
                    .fns = ~!is.na(.x)
                  ))

library(broom)
lm_l = anova(lm(lucro ~ binary, data = movies_num))
lm_l = tidy(lm_l) %>%
          slice(1) %>%
          mutate('Variable' = 'lucro') %>%
          relocate(Variable) %>%
          select(-term)


lm_d = anova(lm(domgross_2013 ~ binary, data = movies_num))
lm_d = tidy(lm_d) %>%
          slice(1) %>%
          mutate('Variable' = 'domgross_2013') %>%
          relocate(Variable) %>%
          select(-term)

lm_i = anova(lm(intgross_2013 ~ binary, data = movies_num))
lm_i = tidy(lm_i) %>%
          slice(1) %>%
          mutate('Variable' = 'intgross_2013') %>%
          relocate(Variable) %>%
          select(-term)

lm_b = anova(lm(budget_2013 ~ binary, data = movies_num))
lm_b = tidy(lm_b) %>%
        slice(1) %>%
        mutate('Variable' = 'budget_2013') %>%
        relocate(Variable) %>%
        select(-term)

# Ignarnado alguns presupostos, uma analise de variancia mostra que
# existe diferença significativa na entre as medias de todas as
# variavies finacnceiras dos dois grupos
#Table 1
rbind(lm_d,lm_i,lm_b,lm_l) %>% knitr::kable()

# A tabela com a media de valores confirma isso, demostrando que
# filmes com mariores orçamentos e receitas tenderam a falhar mais no teste de Bechdel
#Table 2
movies_num %>%
  group_by(binary) %>%
  summarise(across(
    .cols = where(is.numeric),
    .fns = list(mean = mean), na.rm = T
  )) %>%
  knitr::kable()


#Plot 1
#summary(movies_num$lucro)
p1 = movies_num %>%
  mutate(lucro_cat = case_when(
    lucro < 6013346 ~ 'muito baixo',
    lucro >= 6013346 & lucro < 55147752 ~ 'baixo',
    lucro >= 55147752 & lucro < 165041241 ~ 'medio' ,
    lucro >= 165041241 ~ 'alto'
  )) %>%
  group_by(binary, lucro_cat) %>%
  summarise(n = n()) %>%
  mutate(perc_f = n/sum(n[binary=='FAIL']), perc_p = n/sum(n[binary=='PASS'])) %>%
  mutate(across(
    .cols = starts_with('perc'),
    .fns = ~na_if(.x, Inf)
  )) %>%
  rowwise() %>%
  mutate(perc = round(sum(perc_f,perc_p, na.rm = T),2)) %>%
  select(binary,lucro_cat, n, perc) %>%
  mutate(lucro_cat = factor(lucro_cat,
                            levels = c('alto', 'medio', 'baixo', 'muito baixo'), ordered = T)) %>%
  ggplot()+
    aes(x = binary, y = n, fill = lucro_cat)+
    geom_col()+
  geom_text(aes(label = perc), position = position_stack(vjust = 0.5),
            color = 'white', size = 4.5)+
    labs(title = 'Lucro',
         y = 'n', x = 'Bechdel')+
  theme_minimal()+
  theme(legend.title = element_blank())

# Plot2
#summary(movies_num$domgross_2013)

p2 = movies_num %>%
  mutate(domgross_2013_cat = case_when(
    domgross_2013 < 20550000 ~ 'muito baixo',
    domgross_2013 >= 20550000 & domgross_2013 < 55990000 ~ 'baixo',
    domgross_2013 >= 55990000 & domgross_2013 < 121700000 ~ 'medio' ,
    domgross_2013 >= 121700000 ~ 'alto'
  )) %>%
  group_by(binary, domgross_2013_cat) %>%
  summarise(n = n()) %>%
  mutate(perc_f = n/sum(n[binary=='FAIL']), perc_p = n/sum(n[binary=='PASS'])) %>%
  mutate(across(
    .cols = starts_with('perc'),
    .fns = ~na_if(.x, Inf)
  )) %>%
  rowwise() %>%
  mutate(perc = round(sum(perc_f,perc_p, na.rm = T),2)) %>%
  select(binary,domgross_2013_cat, n, perc) %>%
  mutate(domgross_2013_cat = factor(domgross_2013_cat,
                            levels = c('alto', 'medio', 'baixo', 'muito baixo'), ordered = T)) %>%
  ggplot()+
  aes(x = binary, y = n, fill = domgross_2013_cat)+
  geom_col()+
  geom_text(aes(label = perc), position = position_stack(vjust = 0.5),
            color = 'white', size = 4.5)+
  labs(title = 'Arrecadação doméstica',
       y = 'n', x = 'Bechdel')+
  theme_minimal()+
  theme(legend.title = element_blank())

#Plot3
#summary(movies_num$intgross_2013)

p3 = movies_num %>%
  mutate(intgross_2013_cat = case_when(
    intgross_2013 < 33740000 ~ 'muito baixo',
    intgross_2013 >= 33740000 & intgross_2013 < 96890000 ~ 'baixo',
    intgross_2013 >= 96890000 & intgross_2013 < 242000000 ~ 'medio' ,
    intgross_2013 >= 242000000 ~ 'alto'
  )) %>%
  group_by(binary, intgross_2013_cat) %>%
  summarise(n = n()) %>%
  mutate(perc_f = n/sum(n[binary=='FAIL']), perc_p = n/sum(n[binary=='PASS'])) %>%
  mutate(across(
    .cols = starts_with('perc'),
    .fns = ~na_if(.x, Inf)
  )) %>%
  rowwise() %>%
  mutate(perc = round(sum(perc_f,perc_p, na.rm = T),2)) %>%
  select(binary,intgross_2013_cat, n, perc) %>%
  mutate(intgross_2013_cat = factor(intgross_2013_cat,
                                    levels = c('alto', 'medio', 'baixo', 'muito baixo'), ordered = T)) %>%
  ggplot()+
  aes(x = binary, y = n, fill = intgross_2013_cat)+
  geom_col()+
  geom_text(aes(label = perc), position = position_stack(vjust = 0.5),
            color = 'white', size = 4.5)+
  labs(title = 'Arrecadação internacional',
       y = 'n', x = 'Bechdel')+
  theme_minimal()+
  theme(legend.title = element_blank())


#Plot4
#summary(movies_num$budget_2013)

p4 = movies_num %>%
  mutate(budget_2013_cat = case_when(
    budget_2013 < 16234217 ~ 'muito baixo',
    budget_2013 >= 16234217 & budget_2013 < 37157440 ~ 'baixo',
    budget_2013 >= 37157440 & budget_2013 < 79081181 ~ 'medio' ,
    budget_2013 >= 79081181 ~ 'alto'
  )) %>%
  group_by(binary, budget_2013_cat) %>%
  summarise(n = n()) %>%
  mutate(perc_f = n/sum(n[binary=='FAIL']), perc_p = n/sum(n[binary=='PASS'])) %>%
  mutate(across(
    .cols = starts_with('perc'),
    .fns = ~na_if(.x, Inf)
  )) %>%
  rowwise() %>%
  mutate(perc = round(sum(perc_f,perc_p, na.rm = T),2)) %>%
  select(binary,budget_2013_cat, n, perc) %>%
  mutate(budget_2013_cat = factor(budget_2013_cat,
                                    levels = c('alto', 'medio', 'baixo', 'muito baixo'), ordered = T)) %>%
  ggplot()+
  aes(x = binary, y = n, fill = budget_2013_cat)+
  geom_col()+
  geom_text(aes(label = perc), position = position_stack(vjust = 0.5),
            color = 'white', size = 4.5)+
  labs(title = 'Orçamento',
       y = 'n', x = 'Bechdel')+
  theme_minimal()+
  theme(legend.title = element_blank())

# A proporção de alto e medio das variavies financeiras representam uma menor fatia no gropo
# de filmes que passaram no  teste. Em media 55% dos filmes que fracasaram tiveram indices alto ou medio
# esse mesmo percentual cai para 45% entre os filmes que passaram no teste

# A Maior discrepancia esta em orçamento, no qual 30% dos filmes que fracassaram no teste se enquadram nessa categoria
# contra apenas 19% nos que passaram no teste

library(patchwork)
(p1 + p4)/(p2 + p3)

# Analise ao longo do tempo
time_movies = movies %>%
  mutate(intgross_2013 = as.numeric(intgross_2013)) %>%
  mutate(lucro = intgross_2013 - budget) %>%
  group_by(year) %>%
  summarise(n = n(),
            Bechdel = sum(binary == 'PASS'),
            across(
    .cols = c(budget, intgross_2013, lucro),
    .fns = mean, na.rm = T),
    ) %>%
  mutate(perc_pass = Bechdel/n) %>%
  mutate(perc_pass_plot = perc_pass*290684515) %>%
  filter(n>9) %>%
  arrange((year))


# neg_movies = movies %>%
#   mutate(intgross_2013 = as.numeric(intgross_2013)) %>%
#   mutate(lucro = intgross_2013 - budget) %>%
#   group_by(year) %>%
#   summarise(n = n(), neg = sum(lucro<0)) %>%
#   filter(n>9) %>%
#   mutate(perc_neg = neg/n)

# Considerando apenas anos com, ao menos 10 filmes, o orçamendo ao logo dos anos vem aumentando,
# no entanto o lucro corrigido medio tem tendicia de queda. Isso aparentemente se deve
# ao fato dos filmes mais atuais assumirem a mais riscos, aumentando despesas e inserção internacional.
# O grafico seguinte mostra que o prejuzo medio dos filmes que faturaram abaixo do orçamento
# tem tendencia de aumento

# A boa noticia é que a taxa de aprovação do teste vem aumentando com o passar dos anos
# reflexo das mudanças sociais e maior inseção da importancia de igualdade no cimena.

colors <- c("Orçamento" = "red", "Lucro" = "blue", "Aprovação" = "green")

p5 = time_movies %>%
  ggplot()+
    geom_smooth(aes(x =year, y = budget, color = 'Orçamento'), method = 'lm',se = T)+
    geom_smooth(aes(x =year, y = lucro, color = 'Lucro'), method = 'lm',se = T, )+
    geom_smooth(aes(x =year, y = perc_pass_plot, color = 'Aprovação'), method = 'lm',se = T)+
    scale_y_continuous(sec.axis = sec_axis( trans= ~./290684515, name="% aprovação Bechdel"))+
    labs(title = 'Tedencia de indices economicos e performance no teste de Bechdel',x = 'Ano', y = 'US$')+
    scale_color_manual(values = colors)+
    theme_minimal()+
    theme(legend.position = 'bottom',
      legend.title = element_blank(),
          )

p6 = movies %>%
  mutate(intgross_2013 = as.numeric(intgross_2013)) %>%
  mutate(lucro = intgross_2013 - budget) %>%
  filter(lucro<0) %>%
  group_by(year) %>%
  summarise(neg = abs(mean(lucro))) %>%
  ggplot()+
  geom_smooth(aes(x = year, y = neg), method = 'lm')+
  labs(y = 'US$', x = 'ano', title = 'Prejuizo medio',
       subtitle = 'Filmes que fatutaram abaixo do orçamento')+
  theme_minimal()


p7 = movies %>%
  mutate(intgross_2013 = as.numeric(intgross_2013)) %>%
  mutate(lucro = intgross_2013 - budget) %>%
  mutate(lucro_cat = case_when(
    lucro < 0 ~ 'Prejuizo',
    lucro >= 0 ~ 'Lucro')) %>%
  select(binary, lucro_cat) %>%
  group_by(binary) %>%
  summarise(n = n(),
         lucro = sum(lucro_cat == 'Lucro', na.rm = T)/n,
         prejuizo = sum(lucro_cat == 'Prejuizo', na.rm = T)/n) %>%
  pivot_longer(cols = c(lucro, prejuizo), names_to = 'type', values_to = 'value') %>%
  ggplot()+
  geom_col(aes(x = binary , y = value, fill = type))+
  theme_minimal()+
  labs(x = 'Bechdel test', y = '%', title = 'Percentual de lucro/prejuizo vs Bechdel test')+
  theme(legend.title = element_blank())

library(patchwork)
(p5)/(p6 + p7)

# budget_2013
# domgross_2013
# intgross_2013
# int_lucro = intgross_2013  - budget_2013

# language - cat
# Director - cat
# country - car
# writer - cat




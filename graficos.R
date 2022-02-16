
library(ggplot2)
library(viridis)
library(hrbrthemes)


#calculando a variacao nos valores das classes agregadas  de uso da terra entre 2020 e 1985
variacao = LULC_ACF %>%
  filter(year == c(1985, 2020))
variacao = variacao %>%
  group_by(aggregate_class) %>%
  summarise(variation = area[year == 2020] - area[year == 1985])

#retirando a variacao da floresta pra otimizar o grafico
variacao_sem_floresta_e_pasto = variacao %>%
  filter(aggregate_class!= "Forest") %>%
  filter(aggregate_class!= "Pasture")
#variacao apenas do pasto e floresta
variacao_fp = variacao %>%
  filter(aggregate_class!= "Agriculture")%>%
  filter(aggregate_class!= "Non-vegetated area")%>%
  filter(aggregate_class!= "Non forest Natural Formation")%>%
  filter(aggregate_class!= "Non observed")%>%
  filter(aggregate_class!= "Water")

#plotando a variacao
ggplot(variacao, aes(fill=aggregate_class, y=variation, x=aggregate_class)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_viridis(discrete = T) +
  ggtitle("Federal Conservation Areas", "Land Cover Variation: 1985-2020") +
  theme_ipsum() +
  xlab("") +
  ylab("Variation (Km²)") +
  theme(axis.title.x=element_blank(),
  axis.text.x=element_blank(),
  axis.ticks.x=element_blank())

#plotando a time series para as variaveis sem floresta
ggplot(sem_floresta, aes(fill=aggregate_class, y=area, x=year)) + 
  geom_area(position="stack", stat="identity") +
  scale_fill_viridis(discrete = T) +
  ggtitle("Federal Conservation Areas", "Land Cover Time Series: 1985-2020") +
  theme_ipsum() +
  xlab("") +
  ylab("Area (Km²)")


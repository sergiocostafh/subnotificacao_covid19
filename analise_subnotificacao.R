library(ggplot2)
library(tidyverse)
library(ggrepel)

subn <- readxl::read_excel('dados.xlsx')%>%filter(!Causa%in%c('OUTRAS'))%>%
  mutate(Grupo_causa=case_when(Causa=='COVID'~'COVID-19',
                               TRUE~'SRAG/SEPSE/INSUFICIENCIA RESPIRATORIA/PNEUMONIA/INDETERMINADA'))%>%
  group_by(Ano,Grupo_causa,Município)%>%
  summarise(Óbitos=sum(Óbitos*100000/População))

subn_comp <- filter(subn,Grupo_causa!='COVID-19')%>%spread(Ano,Óbitos)%>%
  mutate(dif=`2020`/`2019`)%>%
  arrange(-dif)

top5 <- subn_comp$Município[1:5]
subn_comp <- left_join(subn_comp,spread(filter(subn,Grupo_causa=='COVID-19'),Grupo_causa,Óbitos)%>%ungroup%>%select(-Ano))%>%
  mutate(excedente=`2020`-`2019`,razao=excedente/`COVID-19`)


subn$Município <- factor(subn$Município,levels=rev(unique(subn_comp$Município)),ordered=TRUE)

ggplot()+
  geom_line(aes(x=Ano,y=`Óbitos`,color=`Município`,size=`Município`),filter(subn,Grupo_causa!='COVID-19'),show.legend = FALSE)+
  geom_point(aes(x=Ano,y=`Óbitos`,color=`Município`),size=2,filter(subn,Grupo_causa!='COVID-19'),show.legend = FALSE)+
  geom_text_repel(aes(x=2019,y=`Óbitos`,label=Município,color=Município),force=.001,size=4,nudge_x=-.01,hjust=1,filter(subn,Grupo_causa!='COVID-19',Ano==2019,Município%in%top5),show.legend=FALSE)+
  geom_text(aes(x=2020,y=`2020`,label=paste0('+',round((dif-1)*100),'%'),color=Município),nudge_x=.01 ,size=4,hjust=0,filter(subn_comp,Município%in%top5),show.legend=FALSE)+
  geom_text(aes(x=2020.07,y=180,
                label='As capitais em destaque \napresentaram aumento superior \na 20% no número de óbitos \npor causas respiratórias \n(exceto COVID-19) e septicemia.'),
            hjust=0,fontface='bold')+
  labs(y='Óbitos a cada 100 mil habitantes',
    title='Óbitos por causas respiratórias* (exceto COVID-19) e septicemia nas capitais: \nAcumulado de 1º de janeiro a 31 de maio nos anos de 2019 e 2020',
       subtitle='*SRAG, Pneumonia, Insuficiência respiratória e complicações respiratórias indeterminadas',
       caption='Fonte de dados: Central de Informações do Registro Civil - CRC Nacional
    Obs: A cidade de Cuiabá não entrou na análise por falta de dados
    Autor: Sérgio Costa (https://github.com/sergiocostafh/subnotificacao_covid19)')+
  scale_color_manual(values=c(rep('gray',21),rep('darkred',4),'firebrick3'))+
  scale_size_manual(values=c(rep(.7,21),rep(1,4),1.3))+
  scale_x_continuous(limits=c(2018.88,2020.5),breaks=c(2019,2020))+
  scale_y_continuous(expand=c(0,0),limits=c(0,260))+
  cowplot::theme_cowplot(font_size = 13)+
  theme(axis.line = element_line(color='darkgray'),
        axis.ticks = element_line(color='darkgray'),
        axis.text = element_text(color='black',face='bold'),
        axis.title.y = element_text(color='black',face='bold'),
        axis.title.x = element_blank(),
        plot.title.position = 'plot')

  ggsave('subn.png',plot=last_plot(),dpi=300,height=8,width=9)

  
subn_comp <- subn_comp%>%arrange(razao)%>%mutate(Município=factor(Município,levels=unique(Município),ordered=TRUE))
ggplot(filter(subn_comp,razao>0))+
  geom_segment(aes(x=Município,xend=Município,y=0,yend=razao,color=Município),size=1,show.legend=FALSE)+
  geom_point(aes(x=Município,y=razao,color=Município),size=5,show.legend=FALSE)+
  geom_text(aes(x=Município,y=razao+.1,color=Município,label=prettyNum(round(razao,2),decimal.mark = ',')),hjust=0,size=5,show.legend=FALSE)+
  geom_text(aes(x=11,y=1.9,
                label='Em Belo Horizonte, para cada óbito por COVID-19 \nexistem 3,89 óbitos por outras causas respiratórias ou \nsepticemia excedentes em relação ao mesmo período \nde 2019.'),
            hjust=0,size=4)+
  scale_y_continuous(expand=c(0,0),limits=c(NA,4.5))+
  scale_color_manual(values=c(rep('gray',5),rep('darkred',7),'firebrick3'))+
  labs(title='Mortes excedentes* atribuídas a problemas respiratórios** ou septicemia para \ncada uma atribuída à COVID-19, Capitais',
       subtitle='*Excedente calculado pela diferença do total de óbitos em 2020 e 2019 no período de 1º de janeiro a 31 de maio\n**SRAG, Pneumonia, Insuficiência respiratória e complicações respiratórias indeterminadas',
       caption='Obs¹: Capitais não listadas no gráfico tiveram menos óbitos por problemas respiratórios ou septicemia em 2020
       Obs²: A cidade de Cuiabá não entrou na análise por falta de dados
       Fonte de dados: Central de Informações do Registro Civil - CRC Nacional
    Autor: Sérgio Costa (https://github.com/sergiocostafh/subnotificacao_covid19)')+
  coord_flip()+
  cowplot::theme_cowplot(font_size = 13)+
  theme(axis.text.y = element_text(color='black',face='bold'),
        axis.title = element_blank(),
        plot.title.position = 'plot',
        axis.text.x=element_blank(),
        axis.line.x = element_blank(),
        axis.ticks = element_blank())

ggsave('subn2.png',plot=last_plot(),dpi=300,height=6,width=8)  



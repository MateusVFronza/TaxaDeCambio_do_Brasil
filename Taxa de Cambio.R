library(ggplot2)
library(dplyr)
library(readxl)
TaxaDeCambio <- read.csv("C:/Users/Matheus/Downloads/STP-20210822222258885.csv",
                         sep = ';', stringsAsFactors = FALSE, dec = ',')
View(TaxaDeCambio)

#Manipulando#

is.na(TaxaDeCambio)
TaxaDeCambio[is.na(TaxaDeCambio) == TRUE] = 0
colnames(TaxaDeCambio)[2]= 'Taxa de Câmbio (1 R$/US$)'
length(TaxaDeCambio$Data)
TaxaDeCambio = TaxaDeCambio[-5186,]
  # Formato como numero #
TaxaDeCambio$`Taxa de Câmbio (1 R$/US$)` = sub(',','.',TaxaDeCambio$`Taxa de Câmbio (1 R$/US$)`)
TaxaDeCambio$`Taxa de Câmbio (1 R$/US$)` = as.numeric(TaxaDeCambio$`Taxa de Câmbio (1 R$/US$)`)
  # Formato como data #
TaxaDeCambio$Data = as.Date(TaxaDeCambio$Data, '%d/%m/%Y') #Y maisc = 4 dígitos#
View(TaxaDeCambio)

# return a row number #
which(TaxaDeCambio$Data == '2014-06-05') # retorna o nº da linha da data#

# Grafico #

  #1#
TaxaSelecionada <- TaxaDeCambio
colnames(TaxaSelecionada)[2] <- 'cambio'
TaxaSelecionada <- select(TaxaSelecionada,Data,cambio)
d <- c('1','504','1509','2514','3522','3939','4522') #posições dos nomes#
e = as.numeric(d) + 500
e[1] = c('250') #adiantando fhc#
e[5] = c('3720') #dilma mais p esquerda#
e[6] = c('4250')
DatasNumericas = as.numeric(TaxaSelecionada$Data)

ggplot(TaxaSelecionada) + aes(Data,cambio) + geom_line() +
  labs(x = "", y = "Taxa de Câmbio", 
       title = "Taxa de Câmbio do Brasil (R$/US$)",
       subtitle = 'Brasil, janeiro de 2001 a agosto de 2021',
       caption = 'Fonte: SGS - Banco Central do Brasil\n@MateusFronza') +
  scale_y_continuous(breaks = seq(from = 1, to = 8, by = 1),
                     labels = c(1:8)) +
  scale_x_date(limits = c(TaxaSelecionada$Data[1],TaxaSelecionada$Data[5185]))+
  geom_vline(xintercept=c(DatasNumericas[1],
                          DatasNumericas[504],
                          DatasNumericas[1509],
                          DatasNumericas[2514],
                          DatasNumericas[3522],
                          DatasNumericas[3939],
                          DatasNumericas[4522]),
             color=c('blue', 
                     'red',
                     'red',
                     'red',
                     'red',
                     'blue',
                     'blue'), size=1, linetype=2)+
  annotate('text', x = (TaxaSelecionada$Data[as.numeric(e)]),
           y = 6.20, label = c(c('FHC II',
                                 'Lula I',
                                 'Lula II',
                                 'Dilma I',
                                 'Dilma II',
                                 'Temer I',
                                 'Bolsonaro I')), color = 'black', size = 3.75)+
  theme(panel.background = element_rect(fill = "gray98",
                                        colour = "black",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white")) 
  

  
  
  
  
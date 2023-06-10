library(dplyr)
library(fpp3)
library(forecast)
library(ggplot2)
library(rvest)
library(lubridate)
library(writexl)
library(readxl)
library(usethis)
x=1
dados <- rvest::read_html("https://www.hfbrasil.org.br/br/banco-de-dados-precos-medios-dos-hortifruticolas.aspx?produto=1&regiao%5B%5D=41&periodicidade=diario&ano_inicial=2010&ano_final=2023") %>%
  rvest::html_nodes(xpath = "//*[@id='imagenet-conteudo']/div/div/div[1]/div[3]/div[2]/table") %>%
  rvest::html_table() %>%
  dplyr::bind_rows()

while (x<=200) {
  x=x+1
  dados<-bind_rows(dados, rvest::read_html(paste("https://www.hfbrasil.org.br/br/banco-de-dados-precos-medios-dos-hortifruticolas.aspx?produto=1&regiao%5B%5D=41&periodicidade=diario&ano_inicial=2010&ano_final=2023&pagina=",as.character(x),sep = "")) %>%
                     rvest::html_nodes(xpath = "//*[@id='imagenet-conteudo']/div/div/div[1]/div[3]/div[2]/table") %>%
                     rvest::html_table() %>%
                     dplyr::bind_rows())
}

dados<-dados[which(dados$Produto=="Asterix especial - atacado"),]
dados<-dados %>% 
  mutate(data= ymd(paste(as.character(dados$Ano),as.character(dados$Mês),as.character(dados$Dia),sep="/")))

dados<-dados[,c(8,9)]
dados$Preço <-as.numeric(gsub(",", ".", dados$Preço))
dados<-rev(dados[nrow(dados):1,])


fit <-  arima(dados$Preço, 
              order = c(1, 1, 0), 
              seasonal = list(order = c(0,0,0))
)


previsao<-forecast(fit, h = 1)$mean
hoje<-dados[1,1]+1
atual<-data.frame(DATA=hoje,Previsto=previsao)
cam<-paste(getwd(),"/previsoes.xlsx",sep = "")

if (file.exists(cam)){
  teste<-read_excel(cam)
atual<-bind_rows(atual,teste)
}

#writexl::write_xlsx(x=atual,path = cam)




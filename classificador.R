rm(list=ls())
fnomra_Nvar<-function(x,m,k,n)((1/(sqrt((2*pi)^n*(det(k))))*exp(-0.5*(t(x-m)%*%(solve(k))%*%(x-m)))))
tx_acertos <- matrix(0,ncol = 30)
tabela<-matrix(0,nrow=3,ncol=3)



#importa a biblioteca
dados <- as.matrix(read.csv("waveform.data", header=FALSE))
#separa os rotulos
rotulo<-dados[,22]

# A ideia aqui é avaliar o classificador varias vezes usando amostras distintintas de teste e validação
# Com o intuito de  avaliar a media e a variancia dos acertos.
Q<- seq(1,30,1)
for (j in Q) # repete a classificação 30 vezes.
{
  
  # separa a classe 0
  classe_0<-dados[which(rotulo==0),]
  # embaralha a classe 0
  r<-sample(nrow(classe_0))
  classe_0<-classe_0[r,]
  
  # separa a classe 1
  classe_1<-dados[which(rotulo==1),]
  # embaralha a classe 1
  r<-sample(nrow(classe_1))
  classe_2<-classe_1[r,]
  
  # separa a classe 2
  classe_2<-dados[which(rotulo==2),]
  # embaralha a classe 2
  r<-sample(nrow(classe_2))
  classe_2<-classe_2[r,]
  
  # divide-se o banco de dados em duas partes, 70% do banco serão usado para treinamento do classificador
  # e os 30% restantes serão utilizados para validar o classificador
  
  # calcula a media e a variancia para os dados de treinamento da waveform/classe 0
  dados_classe_0<-classe_0[1:round(0.7*dim(classe_0)[1]),]
  media_classe_0<-as.numeric(matrix(colMeans(dados_classe_0[,1:21]),nrow=1))
  var_classe_0<-cov(dados_classe_0[,1:21])
  
  # calcula a media e a variancia para os dados de treinamento da waveform/classe 1
  dados_classe_1<-classe_1[1:round(0.7*dim(classe_1)[1]),]
  media_classe_1<-as.numeric(matrix(colMeans(dados_classe_1[,1:21]),nrow=1))
  var_classe_1<-cov(dados_classe_1[,1:21])
  
  # calcula a media e a variancia para os dados de treinamento da waveform/classe 2
  dados_classe_2<-classe_2[1:round(0.7*dim(classe_2)[1]),]
  media_classe_2<-as.numeric(matrix(colMeans(dados_classe_2[,1:21]),nrow=1))
  var_classe_2<-cov(dados_classe_2[,1:21])
  
  
  # separa os 30% restantes para validação classificador
  dados_teste<-classe_0[round(0.7*dim(classe_0)[1]):dim(classe_0)[1],]
  dados_teste<-rbind(dados_teste,classe_1[round(0.7*dim(classe_1)[1]):dim(classe_1)[1],])
  dados_teste<-rbind(dados_teste,classe_2[round(0.7*dim(classe_2)[1]):dim(classe_2)[1],])
  r<-sample(nrow(dados_teste))
  dados_embaralhados<-dados_teste[r,]
  dados_teste<-dados_embaralhados # os dados de teste são novamente embaralhados
  
  # Inicializa algumas variaveis.
  len<-seq(1,dim(dados_teste)[1],1)
  val="00"
  n=dim(dados_teste)[2]-1 # 4
  total_len=dim(dados_teste)[1]
  n_acertos=0
  
  #Para validação, usa-se o classificador nos dados_teste e comparando a sua resposta com a classe real a que as amostras pertencem
  for (i in len){
    x<-as.numeric(dados_teste[i,1:21]) #para cada ponto
    nome_real<-dados_teste[i,22]
    #Calcula a pertinencia
    
    resultado_0<-fnomra_Nvar(x,media_classe_0,var_classe_0,n) # calcula a probabilidade da amostra pertencer a classe 0
    resultado_1<-fnomra_Nvar(x,media_classe_1,var_classe_1,n) # calcula a probabilidade da amostra pertencer a classe 1
    resultado_2<-fnomra_Nvar(x,media_classe_2,var_classe_2,n) # calcula a probabilidade da amostra pertencer a classe 2
    
    resultado_geral<-t(c(resultado_0,resultado_1,resultado_2)) # combina os resultados
    
    classe_predita<-switch(max.col(resultado_geral),"0","1","2") # atribui a classe_pretida a classe que obteve maior probabilidade 
    if (as.integer(classe_predita)==nome_real) # compara a classe predita com a classe real da amostra de teste
    {
      n_acertos=n_acertos+1 
    }
  }
  tx_acertos[1,j]<-n_acertos/total_len #taxas de acertos
}

MEDIA_TOTAL<-mean(tx_acertos) # calcula a media de acertos dos testes
DESVIO_PADRAO<-sd(tx_acertos) # calcula o desvio padrão da media de acertos do teste

# imprime o resultado do teste para o classificador.
print(MEDIA_TOTAL)
print(DESVIO_PADRAO)
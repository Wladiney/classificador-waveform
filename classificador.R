fnomra_Nvar<-function(x,m,k,n)((1/(sqrt((2*pi)^n*(det(k))))*exp(-0.5*(t(x-m)%*%(solve(k))%*%(x-m)))))
tx_acertos <- matrix(0,ncol = 30)
tabela<-matrix(0,nrow=3,ncol=3)


Q<- seq(1,30,1)
#captura a biblioteca
dados<-waveform
rotulo<-dados[,22]

for (j in Q)
{
  
  tabela<-matrix(0,nrow=3,ncol=3)
  ################## separa dados de treinamento
  #separa a classe 0
  classe_0<-dados[which(rotulo==0),]
  #embaralha a classe 2
  r<-sample(nrow(classe_0))
  classe_0<-classe_0[r,]
  
  #separa a classe 1
  classe_1<-dados[which(rotulo==1),]
  #embaralha a classe 2
  r<-sample(nrow(classe_1))
  classe_2<-classe_1[r,]
  
  #separa a classe 2
  classe_2<-dados[which(rotulo==2),]
  #embaralha a classe 2
  r<-sample(nrow(classe_2))
  classe_2<-classe_2[r,]
  
  
  ############ calcula dados para 0
  dados_classe_0<-classe_0[1:round(0.7*dim(classe_0)[1]),]
  media_classe_0<-as.numeric(matrix(colMeans(dados_classe_0[,1:21]),nrow=1))
  var_classe_0<-cov(dados_classe_0[,1:21])
  
  ############ calcula dados 1
  dados_classe_1<-classe_1[1:round(0.7*dim(classe_1)[1]),]
  media_classe_1<-as.numeric(matrix(colMeans(dados_classe_1[,1:21]),nrow=1))
  var_classe_1<-cov(dados_classe_1[,1:21])
  
  ############ calcula dados 1
  dados_classe_2<-classe_2[1:round(0.7*dim(classe_2)[1]),]
  media_classe_2<-as.numeric(matrix(colMeans(dados_classe_2[,1:21]),nrow=1))
  var_classe_2<-cov(dados_classe_2[,1:21])
  
  
  
  # separa_dados_teste ###################################
  dados_teste<-classe_0[round(0.7*dim(classe_0)[1]):dim(classe_0)[1],]
  dados_teste<-rbind(dados_teste,classe_1[round(0.7*dim(classe_1)[1]):dim(classe_1)[1],])
  dados_teste<-rbind(dados_teste,classe_2[round(0.7*dim(classe_2)[1]):dim(classe_2)[1],])
  r<-sample(nrow(dados_teste))
  dados_embaralhados<-dados_teste[r,]
  dados_teste<-dados_embaralhados #dados_embaralhados
  
  #testar varias vezes e calcular media e desvio padrão
  len<-seq(1,dim(dados_teste)[1],1)
  val="00"
  n=dim(dados_teste)[2]-1 # 4
  total_len=dim(dados_teste)[1]
  n_acertos=0
  for (i in len){
    x<-as.numeric(dados_teste[i,1:21]) #para cada ponto
    nome_real<-dados_teste[i,22]
    #Calcula a pertinencia
    
    resultado_0<-fnomra_Nvar(x,media_classe_0,var_classe_0,n)
    resultado_1<-fnomra_Nvar(x,media_classe_1,var_classe_1,n)
    resultado_2<-fnomra_Nvar(x,media_classe_2,var_classe_2,n)
    
    resultado_geral<-t(c(resultado_0,resultado_1,resultado_2))
    
    nome_predito<-switch(max.col(resultado_geral),"0","1","2")
    
    if (as.character(nome_predito)==as.character(nome_real))
    {
      n_acertos=n_acertos+1
    }
  }
  tx_acertos[1,j]<-n_acertos/total_len #taxas de acertos
}

MEDIA_TOTAL<-mean(tx_acertos)
DESVIO_PADRAO<-sd(tx_acertos)
MEDIA_TOTAL
DESVIO_PADRAO
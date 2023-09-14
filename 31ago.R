

Dataset <- readXL("/home/2022.1.08.023/Downloads/r/Dados_Aula_Pratica_Amostragem.xlsx", 
                  rownames=FALSE, header=TRUE, na="", sheet="Plan1", stringsAsFactors=TRUE)


#-----------simples
dadosIdade<-Dataset$Idade
N<-length(dadosIdade)
n<-80
m<-sample(N, ns)
amostraCS<-c()

for(i in 1:ns)
  amostraCS[i] <- dadosIdade[m[i]]

print(amostraCS)
mediaCS<-mean(amostraCS)
mediaCS


#----------sistematica
jump <- round(N/ns, 0)
observacaoSistematica <- c()
observacaoSistematica[1] <- sample(jump, 1)
jump

for(i in 2:ns)
  observacaoSistematica[i] <- observacaoSistematica[i-1]+jump

observacaoSistematica


amostraSistematica<-c()
for(i in 1:ns)
  amostraSistematica[i]<-dadosIdade[observacaoSistematica[i]]

amostraSistematica

mediaSistematica <- mean(amostraSistematica)
mediaSistematica


#---------Estratificada-------#

#Obtendo separadamente cada area de atuacao
atuacao<-Dataset$Atuacao
atuacao 

summary(atuacao)

popAprendizes<- 491
popProfissionais<-6710
popOutros<- 511


tamAmostraAprendiz <- round((popAprendizes/N) * ns)
tamAmostraAprendiz

tamAmostraOutros <- round(((popOutros/N) * ns))
tamAmostraOutros

tamAmostraProfissionais <- round(((popProfissionais/N) * ns))
tamAmostraProfissionais

soma<-tamAmostraAprendiz+tamAmostraProfissionais+tamAmostraOutros
soma

idadeAprendizes <- c()
idadeProfissionais <- c()
idadeOutros <- c()

a<-1
b<-1
c<-1

for(i in 1:N)
{
  
  if (atuacao[i] == "A") 
  {
    idadeAprendizes[a] <- dadosIdade[i] 
    a<- a + 1
  }
  if (atuacao[i] == "P") 
  {
    idadeProfissionais[b] <- dadosIdade[i] 
    b<- b + 1
  }
  if (atuacao[i] == "O") 
  {
    idadeOutros[c] <- dadosIdade[i] 
    c<- c + 1
  }
  
}

idadeAprendizes
idadeOutros
idadeProfissionais



amostraAprendiz <- sample(idadeAprendizes, tamAmostraAprendiz)

amostraOutros <- sample(idadeOutros, tamAmostraOutros)

amostraProfissional <- sample(idadeProfissionais, tamAmostraProfissionais)

amostraEst <- c(amostraAprendiz, amostraOutros, amostraProfissional)


mean(amostraEst)



#==============================================================
#     Construindo intervalo de Confiança para a Média (Mu)
#      QUANDO TIVER AS ESTATASTICAS DESCRITIVAS DA AMOSTRA
#==============================================================
#  
#     Informando a média, desvio padrão e tamanho da amostra y #
#===============================================================
# EXEMPLO 1.4.2

My= 29.77       # m�dia amostral
DPy= 5.18      # desvio padrão amostral
ny=  18         # tamanho da amostra 
conf=0.99      # nível de confiança#
alfa=1-conf;alfa  # nível de significância# 
me_mi=qt((alfa/2),ny-1,lower.tail = FALSE)*(DPy/sqrt(ny));me_mi    #calculando a margem de erro#
IC_mi=cbind(My-me_mi,My+me_mi);IC_mi

#==============================================================
# construindo intervalo de Confiança para a Proporção (pi) 
#==============================================================
# EXEMPLO 1.4.6

n=1600      #tamanho da amostra#
np=645       # n�mero de sucessos em n#
pe=np/n;
pe     # propor��o amostral
conf=0.99  # n�vel de confian�a#
alfa=1-conf;
alfa  # n�vel de signific�ncia#
me_p = qnorm(1-alfa/2)*(sqrt(pe*(1-pe))/sqrt(n));
me_p #calculando a margem de erro#
IC_p = cbind(pe-me_p, pe +me_p);
IC_p

#==============================================================
# cálculo do tamanho da amostra para margem de erro na proporção(PI) #
#==============================================================

pap= 0.41      # propor��o na amostra piloto#
merrod=  0.02   #margem de erro desejada, deve sempre ser em DECIMAL#
alfa=0.05 # n�vel de signific�ncia#
n_novo_pi=((qnorm(alfa/2,lower.tail = FALSE)*(sqrt(pap*(1-pap))/merrod)))^2  ;  n_novo_pi


#==============================================================
# c�lculo do tamanho da amostra com margem de erro para Mu #
#==============================================================

DPap= 3   # desvio Padr�o da amostra piloto
nap=  10   #tamanho da amostra piloto
merrod= 1.2  #margem de erro desejada, NA MESMA UNIDADE DA M�DIA#
n_novo_mi=(qt((alfa/2),nap-1,lower.tail = FALSE)*DPap/merrod)^2   ; n_novo_mi

#-----------------------------------------------------------------
  
  #EXERCÍCIO 1
  
  amostra <- c(25,28,24,32,30,38,22,27,31,40,23,28,34,30,29,31,26,38)
  
  mediaamostral <- mean(amostra) 
  
  mediaamostral #29.77 
  
  desviopadraoamostral <- sd(amostra) ## 5.18
  
  
  
  #Arredondamento para duas casas decimais. 
  round(mediaamostral, 2) #29.78
  round(desviopadraoamostral, 2) #5.19
  
  
  
  # cálculo da margem de erro do intervalo de 99% de confiança: 
  
  My= 29.78       # m�dia amostral
  DPy= 5.19      # desvio padrão amostral
  ny=  18         # tamanho da amostra 
  conf=0.99      # nível de confiança#
  alfa=1-conf;alfa  # nível de significância# 
  me_mi=qt((alfa/2),ny-1,lower.tail = FALSE)*(DPy/sqrt(ny));me_mi    #calculando a margem de erro#
  IC_mi=cbind(My-me_mi,My+me_mi);IC_mi
  
  
  
  #EXERCICIO 2
  
  n=1600      #tamanho da amostra
  np=645       # número de sucessos em n
  pe=np/n;
  pe     # proporçãoo amostral
  conf=0.99  # nível de confiança
  alfa=1-conf;
  alfa  # nível de significância
  me_p = qnorm(1-alfa/2)*(sqrt(pe*(1-pe))/sqrt(n));
  me_p # calculando a margem de erro 
  IC_p = cbind(pe-me_p, pe +me_p);
  IC_p
  
  
  round(43.47, 2)

 
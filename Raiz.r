#######metodo de Newton Raphson######
#######Alessandro - 01/03/2018#######

f=function(x)(x^3 - 1.7*x^2 - 12.78*x - 10.08) ### funcao do prof
plot(f, from = -4, to = 6,lwd=1)
abline(v=0, col = "blue",lwd=1) ### criando o eixo y
abline(h=0, col = "blue",lwd=1) ### criando o eixo X
 
a=-3;b=-1.55 # primeiro intervalo escolhido
abline(v=a,col = "red")
abline(v=b, col = "red")

a=-1.5;b=-0.5 # segundo intervalo escolhido
abline(v=a,col = "green")
abline(v=b, col = "green")

a=4;b=6 # terceiro intervalo escolhido
abline(v=a,col = "yellow")
abline(v=b, col = "yellow")

##############################################
e=expression (x^3 - 1.7*x^2 - 12.78*x - 10.08)
e
d=D(e,"x")
d
df=function(x) (3 * x^2 - 1.7 * (2 * x) - 12.78)
df
#############################################   ### apenas para derivar F(x)

f #função
df #derivada da função

NR = function(a, b, e, df){ 
  #print(a)
  #print(b)
  k=0     ###k=0. Contador para o numero de iteracoes
  x1=(a+b)/2                                  ### x1 escolhido por um numero medio do intervalo
  x2=x1-(f(x1)/df(x1))                        ### x2 escolhido pela formula x1-(f(x1)/df(x1))
# print(x1)
# print(x2)
  while((abs(x1)-abs(x2))>e){                ### Enquanto a diferenca for maior que e faca
    x1=x2                                    ### x1 recebe x2 
    x2=x2-(f(x2)/df(x2))                     ### x2 escolhido pela formula x1-(f(x1)/df(x1))
#    print(x1)
#    print(x2)
    k=k+1                       ### Atualizamos o contador
  }                             ### Fim do Enquanto
  return (c(x1,k))          ###retorna a raiz e o numero de iteracoes
}

### A funcao Newton Raphson recebe os parametros.

###OBS:quanto menor o erro, maior a quantidade de passos              
###porem, maior precisao no resultado

### Resultados graficos

a=-3;b=-1.55 #primeiro intervalo escolhido
p=NR(-3,-1.55,0.0001,df);p;p=p[1]
plot(f, from = -4, to = 6,lwd=1);abline(v=0, col = "blue",lwd=1);abline(h=0, col = "blue",lwd=1);abline(v=p, col = "red",lwd=1);text(-2.5,5,"-2.1", col = "red")

a=-1.5;b=-0.5 #segundo intervalo escolhido
p=NR(-1.5,-0.5,0.0001,df);p;p=p[1]
plot(f, from = -4, to = 6,lwd=1);abline(v=0, col = "blue",lwd=1);abline(h=0, col = "blue",lwd=1);abline(v=p, col = "red",lwd=1);text(-1.4,7,"-1", col = "red")

a=4;b=6 #terceiro intervalo escolhido
p=NR(4,6,0.0001,df);p;p=p[1]
plot(f, from = -4, to = 6,lwd=1);abline(v=0, col = "blue",lwd=1);abline(h=0, col = "blue",lwd=1);abline(v=p, col = "red",lwd=1);text(4.5,7,"4.8", col = "red")










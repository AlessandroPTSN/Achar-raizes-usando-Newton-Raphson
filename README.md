# Achar-raizes-usando-Newton-Raphson-R
O método de Newton é um método para procurar as raízes de uma função, para isso, escolhemos um ponto proximo. Depois disso, calculamos a equação da reta tangente (por meio da derivada) da função nesse ponto e a interseção dela com o eixo das abcissas, a fim de encontrar uma melhor aproximação para a raiz. Repetindo o processo varias vezes, podemos ter o valor aproximado da raiz.

# Gráfico da função
```R
###Definindo a funcao a ser adotada
f=function(x)(x^3 - 1.7*x^2 - 12.78*x - 10.08)
plot(f, from = -4, to = 6,lwd=1)
abline(v=0, col = "blue",lwd=1) ### criando o eixo y
abline(h=0, col = "blue",lwd=1) ### criando o eixo X
```
![1](https://user-images.githubusercontent.com/50224653/71480709-56733a00-27d9-11ea-94d5-3a7c30f8e11b.PNG)

# Intervalos de interesse
```R
###Definindo os intervalos de interesse
f=function(x)(x^3 - 1.7*x^2 - 12.78*x - 10.08)
plot(f, from = -4, to = 6,lwd=1)
abline(v=0, col = "blue",lwd=1) 
abline(h=0, col = "blue",lwd=1) 
 
a=-3;b=-1.55 # primeiro intervalo escolhido (red)
abline(v=a,col = "red")
abline(v=b, col = "red")

a=-1.5;b=-0.5 # segundo intervalo escolhido (green)
abline(v=a,col = "green")
abline(v=b, col = "green")

a=4;b=6 # terceiro intervalo escolhido (yellow)
abline(v=a,col = "yellow")
abline(v=b, col = "yellow")
```
![2](https://user-images.githubusercontent.com/50224653/71480710-56733a00-27d9-11ea-8de7-9e0c10c04e8e.PNG)

# Derivada da função
```R
##############################################
e=expression (x^3 - 1.7*x^2 - 12.78*x - 10.08)
e
d=D(e,"x")
d
df=function(x) (3 * x^2 - 1.7 * (2 * x) - 12.78)
df
#############################################   ### apenas para derivar F(x)
```
# Método Newton Raphson
```R
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
```
# Resultados gráficos
## Primeira Raiz
```R
a=-3;b=-1.55 #primeiro intervalo escolhido (red)
p=NR(-3,-1.55,0.0001,df);p;p=p[1] #Raiz | Número de passos para obter a raiz
plot(f, from = -4, to = 6,lwd=1);abline(v=0, col = "blue",lwd=1);abline(h=0, col = "blue",lwd=1);abline(v=p, col = "red",lwd=1);text(-2.5,5,"-2.1", col = "red")
```
```R
[1] -2.1  3.0
```
![3](https://user-images.githubusercontent.com/50224653/71480711-570bd080-27d9-11ea-92d6-4ea812f14142.PNG)

## Segunda Raiz
```R
a=-1.5;b=-0.5 #segundo intervalo escolhido (green)
p=NR(-1.5,-0.5,0.0001,df);p;p=p[1] #Raiz | Número de passos para obter a raiz
plot(f, from = -4, to = 6,lwd=1);abline(v=0, col = "blue",lwd=1);abline(h=0, col = "blue",lwd=1);abline(v=p, col = "red",lwd=1);text(-1.4,7,"-1", col = "red")
```
```R
[1] -1  0
```
![4](https://user-images.githubusercontent.com/50224653/71480712-570bd080-27d9-11ea-96fe-acc227ca4a64.PNG)

## Terceira Raiz
```R
a=4;b=6 #terceiro intervalo escolhido (yellow)
p=NR(4,6,0.0001,df);p;p=p[1] #Raiz | Número de passos para obter a raiz
plot(f, from = -4, to = 6,lwd=1);abline(v=0, col = "blue",lwd=1);abline(h=0, col = "blue",lwd=1);abline(v=p, col = "red",lwd=1);text(4.5,7,"4.8", col = "red")
```
```R
[1] 4.800042 2.000000
```
![5](https://user-images.githubusercontent.com/50224653/71480714-570bd080-27d9-11ea-8c7f-282b503852af.PNG)

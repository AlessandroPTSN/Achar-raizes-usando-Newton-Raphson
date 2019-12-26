# Achar-raizes-usando-Newton-Raphson
O método de Newton é um método para procurar as raízes de uma função, para isso, escolhemos um ponto proximo. Depois disso, calculamos a equação da reta tangente (por meio da derivada) da função nesse ponto e a interseção dela com o eixo das abcissas, a fim de encontrar uma melhor aproximação para a raiz. Repetindo o processo varias vezes, podemos ter o valor aproximado da raiz.

# Gráfico da função
```R
###Definindo a funcao a ser adotada
f=function(x)(x^3 - 1.7*x^2 - 12.78*x - 10.08)
plot(f, from = -4, to = 6,lwd=1)
abline(v=0, col = "blue",lwd=1) ### criando o eixo y
abline(h=0, col = "blue",lwd=1) ### criando o eixo X
```

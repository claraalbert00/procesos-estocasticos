lambda <- runif(min=0.90, max=1, n=1)
Z0 <- sample(20:30, size=1) 
print(lambda)
print(Z0)


offspringPois <- function(lambda, Z) {
  sum(rpois(lambda=lambda, n=Z))
}

repeticions=1000
numindividus=numeric(repeticions)

for (k in 1:repeticions){
  Z=Z0
  for (i in 1:20){
    Z <-offspringPois(lambda = lambda, Z=Z)
  }
  numindividus[k]=Z
}

hist(numindividus)
mean(numindividus)

prob=length(subset(numindividus,numindividus<(Z0/2)))/length(numindividus)



print("La mitjana teòrica és:")
print((1/lambda)^20)
print("La meva estimació de la mitjana teòrica és:")
print(mean(numindividus))
print("La meva estimació de la probabilitat de menys de la meitat a la generació 20 és:")
print(prob)      

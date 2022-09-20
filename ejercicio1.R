# SÓC Clara Albert I DURANT L'EXAMEN M'HE COMUNICAT AMB: NINGÚ

matrixpower <- function(mat, k) {
 if (k == 0)
  return (diag(dim(mat)[1]))
 if (k == 1)
  return(mat)
 if (k > 1)
  return(mat %*% matrixpower(mat, k - 1))
}

llet <- matrix(c(
  0.63, 0.35, 0.02,
  0   , 0.79, 0.21,
  0.40, 0.00, 0.60
  ), nrow=3, byrow=TRUE) 


#a
inicial=c(300000,150000,75000)
2025-2019
consumidors=inicial%*%matrixpower(llet,6)
print(round(consumidors/sum(inicial)*100,3))

#b-i
print(round(matrixpower(llet, 2), 3))

print("k=17")
print(round(matrixpower(llet, 17), 3))

print("k=18")
print(round(matrixpower(llet, 18), 3))



#b-ii
eigen(t(llet)) #Veiem que només hi ha un valor propi igual a 1
x=eigen(t(llet))$vector[,1]
estacionaria <- x/sum(x)
print(round(estacionaria,3))


#b-iii
# recodifiquem A,B,C per 1,2,3:
mida <- 1000
state <- numeric(mida)
state[1] <- 1
for (i in 1:(mida-1)){
  state[i+1]=sample(1:3, prob=llet[state[i],], size=1)
}
round(table(state)/mida,3)



#### 
print("a) Els percentatges estimats de consumidors el 2025 seran:")
print("Marca A: 26.894%, Marca B: 47.235%, Marca C: 25.871%")

print("b) Hi haurà distribució limit de la cadena?")
print("Sí que hi haurà distribució limit ja que la cadena al 2025 no té cap valor proper a 0 i per tant la matriu s'estabilitzarà. A més a més, veiem 
      fàcilment que és una matriu regular (nomès cal calcular P*P)")

print("b-i) Les potències s'estabilitzen a la potència:")
print("S'estabilitza en K=18")

print("I la distribució límit és")
print("lambda=(0.278,0.464,0.258)")



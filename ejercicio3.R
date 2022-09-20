lambda <- runif(min=0.5, max=5, n=1)
print(lambda)

t=15



N <- rpois(lambda=lambda*t, n=1)
arribades <- runif(min=0, max=t, n=N)
arribades=sort(arribades)

trajectoria=stepfun(x=arribades,y=0:N)
plot(trajectoria, verticals = FALSE, xlab="Temps", ylab="Arribades")




print("a) Els n instants d'arribada en [0,t] es distribueixen:")
print("Es distribueixen uniformement en [0,t]. ")
print("b) El nombre d'arribades ha estat:")
print(N)
print("c) Els instants d'arribada, ordenats, han estat:")
print(sort(arribades))
print("d) La última arribada ha estat a l'instant:")
print(max(arribades))

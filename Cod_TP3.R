library(ExpDE)

a<-4
k<-(a*(a-1))/2
alpha<-0.05
alpha_adj<-alpha/k
delta<-10
potencia_desejada<-0.85
sd<-40

test_pow_N <- power.t.test(delta = delta, power = potencia_desejada, sd = sd, sig.level = alpha_adj, alternative = "two.sided", type = "two.sample")
print(test_pow_N)


selpars  <- list(name  = "selection_standard")
stopcrit <- list(names = "stop_maxeval", maxevals = 50000, maxiter = 1000)
probpars <- list(name  = "sphere", xmin  = -seq(1,20), xmax  = 20 + 5 * seq(5, 24))


# Equipe Marcelo, Brayan, William, André

## Config 1
recpars1 <- list(name = "recombination_blxAlphaBeta", alpha = 0, beta = 0)
mutpars1 <- list(name = "mutation_rand", f = 4)
popsize1 <- 200

## Config 2
recpars2 <- list(name = "recombination_exp", cr = 0.6)
mutpars2 <- list(name = "mutation_best", f = 2)
popsize2 <- 130

## Config 3
recpars3 <- list(name = "recombination_blxAlphaBeta", alpha = 0.4, beta = 0.4) 
mutpars3 <- list(name = "mutation_rand", f = 4)
popsize3 <- 230

## Config 4
recpars4 <- list(name = "recombination_wright")
mutpars4 <- list(name = "mutation_best", f = 4.8)
popsize4 <- 113


a<-4
k<-(a*(a-1))/2
alpha<-0.05
alpha_adj<-alpha/k
delta<-10
potencia_desejada<-0.85
sd<-40


test_pow_N <- power.t.test(delta = delta, power = potencia_desejada, sd = sd, sig.level = alpha_adj, alternative = "two.sided", type = "two.sample")
print(test_pow_N)

# Run algorithm on problem:
Numero_it<-434
result_out1<-list()
result_out2<-list()

for (i in 1:Numero_it) {
  print(i)
  out1 <- ExpDE(popsize  = popsize1, 
                mutpars  = mutpars1, 
                recpars  = recpars1, 
                selpars  = selpars, 
                stopcrit = stopcrit, 
                probpars = probpars,
                showpars = list(show.iters = "dots",
                                showevery  = 20))
  
  # Extract observation:
  #out1$Fbest
  
  # Run algorithm on problem:
  out2 <- ExpDE(popsize  = popsize2, 
                mutpars  = mutpars2, 
                recpars  = recpars2, 
                selpars  = selpars, 
                stopcrit = stopcrit, 
                probpars = probpars,
                showpars = list(show.iters = "dots",
                                showevery  = 20))
  
  # Extract observation:
  #out2$Fbest
  
  # Run algorithm on problem:
  out3 <- ExpDE(popsize  = popsize3, 
                mutpars  = mutpars3, 
                recpars  = recpars3, 
                selpars  = selpars, 
                stopcrit = stopcrit, 
                probpars = probpars,
                showpars = list(show.iters = "dots",
                                showevery  = 20))
  
  # Extract observation:
  #out3$Fbest
  
  # Run algorithm on problem:
  out4 <- ExpDE(popsize  = popsize4, 
                mutpars  = mutpars4, 
                recpars  = recpars4, 
                selpars  = selpars, 
                stopcrit = stopcrit, 
                probpars = probpars,
                showpars = list(show.iters = "dots",
                                showevery  = 20))
  
  # Extract observation:
  #out4$Fbest
  
  result_out1[[i]]<-c(out1$Fbest,out2$Fbest,out3$Fbest,out4$Fbest)
  result_out2[[i]]<-c("out1","out2","out3","out4")
  
}

dados_total<-data.frame(label_out=(unlist(result_out2)),out=(unlist(result_out1)))
write.csv(dados_total, file = "dados_total.csv")

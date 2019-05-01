
set.seed(1234567890)

library("neuralnet")

dataset <- read.csv("~/Documentos/Mestrado/IA/bancario.csv")
head(dataset)
#Dataset: income (renda anual), age, loan (valor do emprestimo) e LTI (loan/income)

#Objetivo: criar um modelo para prever se uma negligencia vai ocorrer em 10 anos com base em LTI e age

rand <- sample(nrow(dataset),1000)
treino <- dataset[rand,]
validacao <- dataset[-rand,]

creditnet <- neuralnet(result ~ variance + skewness + curtosis + entropy, treino, hidden = 4, lifesign = "minimal", 
                       linear.output = FALSE, threshold = 0.1, stepmax=1e6)
plot(creditnet, rep = "best")

#Etapa de validacao
temp_test <- subset(validacao, select = c("variance", "skewness","curtosis","entropy"))
creditnet.results <- compute(creditnet, temp_test)

#Verificando a acuracia
results <- data.frame(verdade = validacao$result, predicao = creditnet.results$net.result)
roundedresults<-sapply(results,round,digits=0)
roundedresultsdf=data.frame(roundedresults)

table(roundedresultsdf$verdade,roundedresultsdf$predicao)


#install library
library(neuralnet)
library(MASS)
library(grid)
library (NeuralNetTools)

dat <- PTSD.AI.csv("new.csv", header = T, row.names = 1)
View(dat)

plot(nn)
nn = neuralnet( class ~ ENSG00000225811 + KRT24 + ENSG00000300626 + RP11 + 
                  LINC00955 + ENSG00000285548 + SLC10A2 + ENSG00000295799 + 
                  RBM19 + ENSG00000221882 + SLC25A29 + APBA1 + 
                  ENSG00000266994 + ENSG00000185681, data =dat, hidden = 2, threshold = 0.01, stepmax = 1e+05, rep = 1, startweights = NULL, learningrate.limit = NULL, learningrate.factor = list(minus = 0.5, plus = 1.2), learningrate = NULL, lifesign = "none", lifesign.step = 1000, algorithm = "rprop+", err.fct = "ce", act.fct = "logistic", linear.output = FALSE, exclude = NULL, constant.weights = NULL, likelihood = FALSE)
nn
plot(nn)
nn$net.result #overall result, that is, output for each replication
nn$weights
nn$result.matrix

nn$covariate
dat$class
nn$net.result[[1]]
nn1 = ifelse(nn$net.result[[1]]>0.5,1,0) #To ckeck results are more than 50%, if they are it is given value 1 else 0
nn1

#Calculate how much classification error you have made
misClassificationError = mean(dat$class != nn1)
misClassificationError


#Determine what errors you have made in predicting the control and patient case
OutPutVsPred = cbind(dat$class,nn1)
OutPutVsPred  



#Result visualization
#Divide output display into 4 parts
par(mfrow=c(2,2))

#Visualize the effect of each variable
gwplot(nn,selected.covariate = "ENSG00000225811", highlight = TRUE, col = "blue")
gwplot(nn,selected.covariate = "KRT24", highlight = TRUE, col = "blue")
gwplot(nn,selected.covariate = "ENSG00000300626", highlight = TRUE, col = "blue")
gwplot(nn,selected.covariate = "RP11", highlight = TRUE, col = "blue")
gwplot(nn,selected.covariate = "LINC00955", highlight = TRUE, col = "blue")
gwplot(nn,selected.covariate = "ENSG00000285548", highlight = TRUE, col = "blue")
gwplot(nn,selected.covariate = "SLC10A2", highlight = TRUE, col = "blue")
gwplot(nn,selected.covariate = "ENSG00000295799", highlight = TRUE, col = "blue")
gwplot(nn,selected.covariate = "RBM19", highlight = TRUE, col = "blue")
gwplot(nn,selected.covariate = "ENSG00000221882", highlight = TRUE, col = "blue")
gwplot(nn,selected.covariate = "SLC25A29", highlight = TRUE, col = "blue")
gwplot(nn,selected.covariate = "APBA1", highlight = TRUE, col = "blue")
gwplot(nn,selected.covariate = "ENSG00000266994", highlight = TRUE, col = "blue")
gwplot(nn,selected.covariate = "ENSG00000185681", highlight = TRUE, col = "blue")

# Visualization 
plotnet(nn)       #Improved version of GW plot
garson(nn)        #Relative importance of input variables
olden(nn)         #Direction & strength of variable effect
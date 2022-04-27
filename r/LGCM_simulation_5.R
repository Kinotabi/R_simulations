packages = c('semPlot', 'tidyverse', 'mnormt', 'SimDesign', 'lavaan', 'lme4', 'simstudy', 'dplyr', 'ggplot2', 'reshape2')

#load/Install required packages
for (i in packages){
  if (!require(i, character.only = TRUE)){
    install.packages(i, dependencies = TRUE)
  } else {
    library(i, character.only = TRUE)
  }
}

theme_set(theme_classic() +
            theme(panel.grid.major.y = element_line(color = "grey92")))



#Function for generating data
gen_lgm_data = function(N, alpha, phi, lambda, theta){
  #generate latent factor scores
  eta = rmnorm(N, mean = alpha, varcov = phi)
  #generate residuals
  e = rmnorm(N, varcov = theta)
  #compute outcome scores
  y = tcrossprod(eta, lambda) + e
  colnames(y) = paste0('y', 1:5)
  #make it a data frame
  as.data.frame(y)
  return (y)
}

#lavaan syntax
#true model
growth_model <- 'i =~ 1*y1 + 1*y2 + 1*y3 + 1*y4 + 1*y5
                 s =~ 0*y1 + 1*y2 + 2*y3 + 3*y4 + 1*y5
                 i ~~ 1 * i
                 s ~~ 1 * s
                 y1 ~~ 0.5*y1
                 y2 ~~ 0.5*y2
                 y3 ~~ 0.5*y3
                 y4 ~~ 0.5*y4
                 y5 ~~ 0.5*y5
                 i ~ 1*1
                 s ~ 0.5*1'
semPaths(semPlotModel_lavaanModel(growth_model))

#design factors
designfactor = expand.grid(
  reliability = c(0.3, 0.6, 0.9),
  sample_size = c(100, 500, 5000, 10000),
  time_points = c(3, 5, 10),
  missing_rate = c(0.05, 0.20),
  trend = c('linear', 'quadratic', 'exponential', 'weibull'),
  replication = 100
)
designfactor_list = rowid_to_column(designfactor, 'condition')

#Data generation
#NREP = 100
alpha1 = c(1, 0.5)#latent mean of intercepts
phi1 = matrix(c(1, 0, 0, 1), nrow=2) #intercept variance
lambda1 = cbind(c(1,1,1,1,1), c(0,1,2,3,4))
theta1 = diag(0.5, nrow = 5)

#df = gen_lgm_data(N = NREP, 
#                  alpha = alpha1, 
#                  phi = phi1, 
#                  lambda = lambda1, 
#                  theta = theta1)


#generate simulation data from growth_model
head(df)
cor(df)

lavaan::simulateData(growth_model) %>% head()

##run simulation
runsim = function(to_run, nrep, 
                  alpha = alpha1,
                  phi = phi1,
                  lambda = lambda1,
                  theta = theta1,
                  designfactors = designfactor_list) {
  #extract design parameters for the given condition
  N = designfactors[to_run, 'N']
  phi = designfactors[to_run,]
}
  

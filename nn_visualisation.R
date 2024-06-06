# https://beckmw.wordpress.com/2013/11/14/visualizing-neural-networks-in-r-update/
library(nnet)
library(clusterGeneration)

seed.val<-2
set.seed(seed.val)

num.vars<-8
num.obs<-1000

#input variables
cov.mat<-genPositiveDefMat(num.vars,covMethod=c("unifcorrmat"))$Sigma
rand.vars<-mvrnorm(num.obs,rep(0,num.vars),Sigma=cov.mat)

#output variables
parms<-runif(num.vars,-10,10)
y1<-rand.vars %*% matrix(parms) + rnorm(num.obs,sd=20)
parms2<-runif(num.vars,-10,10)
y2<-rand.vars %*% matrix(parms2) + rnorm(num.obs,sd=20)

#final datasets
rand.vars<-data.frame(rand.vars)
resp<-data.frame(y1,y2)
names(resp)<-c('Y1','Y2')
dat.in<-data.frame(resp,rand.vars)

#nnet function from nnet package
library(nnet)
set.seed(seed.val)
mod1<-nnet(rand.vars,resp,data=dat.in,size=10,linout=T)

#neuralnet function from neuralnet package, notice use of only one response
library(neuralnet)
form.in<-as.formula('Y1~X1+X2+X3+X4+X5+X6+X7+X8')
set.seed(seed.val)
mod2<-neuralnet(form.in,data=dat.in,hidden=10)

#mlp function from RSNNS package
library(RSNNS)
set.seed(seed.val)
mod3<-mlp(rand.vars, resp, size=10,linOut=T)

#import the function from Github
library(devtools)
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')

mod1
mod2
#plot each model
plot.nnet(mod1)
plot.nnet(mod2)
plot.nnet(mod3)


wts.in<-mod1$wts
struct<-mod1$n
plot.nnet(wts.in,struct=struct)


mod.in<-c(13.12,1.49,0.16,-0.11,-0.19,-0.16,0.56,-0.52,0.81)
struct<-c(2,2,1) #two inputs, two hidden, one output
plot.nnet(mod.in,struct=struct)


mod4<-train(Y1~.,method='nnet',data=dat.in,linout=T)
plot.nnet(mod4,nid=T)


fact<-factor(sample(c('a','b','c'),size=num.obs,replace=T))
form.in<-formula('cbind(Y2,Y1)~X1+X2+X3+fact')
mod5<-nnet(form.in,data=cbind(dat.in,fact),size=10,linout=T)
plot.nnet(mod5,nid=T)

library(RSNNS)

#neural net with three hidden layers, 9, 11, and 8 nodes in each
mod<-mlp(rand.vars, resp, size=c(9,11,8),linOut=T)
par(mar=numeric(4),family='serif')
plot.nnet(mod)

library(neuralnet)

#response
AND<-c(rep(0,7),1)
OR<-c(0,rep(1,7))

#response with predictors
binary.data<-data.frame(expand.grid(c(0,1),c(0,1),c(0,1)),AND,OR)

#model
net<-neuralnet(AND+OR~Var1+Var2+Var3, binary.data,hidden=c(6,12,8),rep=10,err.fct="ce",linear.output=FALSE)

#plot output
par(mar=numeric(4),family='serif')
plot.nnet(net)

#example showing use of separate colors for input layer
#color based on relative importance using 'gar.fun'

##
#create input data
seed.val<-3
set.seed(seed.val)

num.vars<-8
num.obs<-1000

#input variables
library(clusterGeneration)
cov.mat<-genPositiveDefMat(num.vars,covMethod=c("unifcorrmat"))$Sigma
rand.vars<-mvrnorm(num.obs,rep(0,num.vars),Sigma=cov.mat)

#output variables
parms<-runif(num.vars,-10,10)
y1<-rand.vars %*% matrix(parms) + rnorm(num.obs,sd=20)

#final datasets
rand.vars<-data.frame(rand.vars)
resp<-data.frame(y1)
names(resp)<-'Y1'
dat.in<-data.frame(resp,rand.vars)

##
#create model
library(nnet)
mod1<-nnet(rand.vars,resp,data=dat.in,size=10,linout=T)

##
#relative importance function
library(devtools)
source_url('https://gist.github.com/fawda123/6206737/raw/2e1bc9cbc48d1a56d2a79dd1d33f414213f5f1b1/gar_fun.r')

#relative importance of input variables for Y1
rel.imp<-gar.fun('Y1',mod1,bar.plot=F)$rel.imp

#color vector based on relative importance of input values
cols<-colorRampPalette(c('green','red'))(num.vars)[rank(rel.imp)]

##
#plotting function
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')

#plot model with new color vector
#separate colors for input vectors using a list for 'circle.col'
plot(mod1,circle.col=list(cols,'lightblue'))

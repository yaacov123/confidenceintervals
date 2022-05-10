data<-read.csv("stresstest.csv")
c<-cov(data[,2:3])
yy<-summary(lm(data[,1]~data[,2]+data[,3]))
covbeta<-solve(cov(data[,2:3]))*5.038^2/188
stressvector<-rbind(c(1,1))
conf<-sqrt(stressvector%*%covbeta%*%t(stressvector))
simResid<-matrix(rnorm(188000,0,sd=5.038),ncol=1000)
yysim<-(-0.7697)+1.2619*data[,2]+1.0574*data[,3]+simResid 
j<-function(y,d1,d2){r<-lm(y~d1+d2); gg<-coefficients(r)[2:3];return(gg)}
res<-apply(yysim,2,j,d1=data[,2],d2=data[,3])
Beta <-
function (params) 
{
    alpha = params[1]
    beta  = params[2]
    
    tmp <- list(
                Model = "Beta"
                ,
                Parameters = params
                ,
                Pdf = function(x)
                {
                    dbeta(x, alpha, beta)
                },
                Cdf = function(x)
                {
                    pbeta(x, alpha, beta)
                },
                Quantile = function(q)
                {
                    qbeta(q, alpha, beta)
                },
                Roll = function(sampleSize)
                {
                    rbeta(sampleSize, alpha, beta)
                },
                Mean<-params[1]*params[2]
                ,
                Variance<- params[1]^2
    )
    class(tmp)="RandomVar"
    tmp
}
Binomial <-
function (params) 
{
    n = params[1]
    p = params[2]
    
    tmp <- list(
                Model = "Binomial"
                ,
                Parameters = params
                ,
                Pdf = function(x)
                {
                    dbinomial(x, n, p)
                },
                Cdf = function(x)
                {
                    pbinomial(x, n, p)
                },
                Quantile = function(q)
                {
                    qbinomial(q, n, p)
                },
                Roll = function(sampleSize)
                {
                    rbinomial(sampleSize, n, p)
                },
                Mean<-params[1]*params[2]
                ,
                Variance<- params[1]^2
    )
    class(tmp)="RandomVar"
    tmp
}
Exponential <-
function (params) 
{
    rate = 1/params[1]
    
    tmp <- list(
                Model = "Exponential"
                ,
                Parameters = params
                ,
                Pdf = function(x)
                {
                    dexp(x,rate)
                },
                Cdf = function(x)
                {
                    pexp(x,rate)
                },
                Quantile = function(q)
                {
                    qexp(q,rate)
                },
                Roll = function(sampleSize)
                {
                    rexp(sampleSize,rate)
                },
                Mean<-params[1]
                ,
                Variance<- params[1]^2
    )
    
    #tmp = list(Model="Exponential", Parameters=params, Pdf=myPdf,Cdf=myCdf,Quantile=myQuantile,Roll=myRoll, Mean=myMean, Variance=myVariance)
    class(tmp)="RandomVar"
    tmp
}
Gamma <-
function (params) 
{
    rate = 1/params[2]
    
    tmp <- list(
                Model = "Gamma"
                ,
                Parameters = params
                ,
                Pdf = function(x)
                {
                    dgamma(x, params[1], rate)
                },
                Cdf = function(x)
                {
                    pgamma(x, params[1], rate)
                },
                Quantile = function(q)
                {
                    qgamma(q, params[1], rate)
                },
                Roll = function(sampleSize)
                {
                    rgamma(sampleSize, params[1], rate)
                },
                Mean<-params[1]*params[2]
                ,
                Variance<- params[1]^2
    )
    class(tmp)="RandomVar"
    tmp
}
peak <-
function (x,y, radius=1, n=100) 
{
  theta = pi*(0:n)/n
  X = x + radius*cos(theta)
  Y = y + radius*sin(theta)
  lines(X,Y)
}
plot.NexenPinion <-
function (x,...)
{
  tmp=x$R+3*x$r
  plot(c(-tmp,tmp),c(-tmp,tmp), type ="n")
  circle(0,0,radius=x$r)
  rho=0:(x$N-1)
  
}
Poisson <-
function (params) 
{
    lambda = params[1]
    
    tmp <- list(
                Model = "Poisson"
                ,
                Parameters = params
                ,
                Pdf = function(x)
                {
                    dpois(x,lambda)
                },
                Cdf = function(x)
                {
                    ppois(x,lambda)
                },
                Quantile = function(q)
                {
                    qpois(q,lambda)
                },
                Roll = function(sampleSize)
                {
                    rpois(sampleSize,lambda)
                },
                Mean<-params[1]
                ,
                Variance<- params[1]^2
    )
    class(tmp)="RandomVar"
    tmp
}
print.RandomVar <-
function (x, ..., digits = NULL, quote = FALSE, right = TRUE, 
	row.names = TRUE) 
{
    l<-length(x$Parameters)
	cat(x$Model,"(", x$Parameters[1],")\n")
	invisible(x)
}
Weibull <-
function (params) 
{
    rate = 1/params[2]
    
    tmp <- list(
                Model = "Weibull"
                ,
                Parameters = params
                ,
                Pdf = function(x)
                {
                    dweibull(x, params[1], rate)
                },
                Cdf = function(x)
                {
                    pweibull(x, params[1], rate)
                },
                Quantile = function(q)
                {
                    qweibull(q, params[1], rate)
                },
                Roll = function(sampleSize)
                {
                    rweibull(sampleSize, params[1], rate)
                },
                Mean<-params[1]*params[2]
                ,
                Variance<- params[1]^2
    )
    class(tmp)="RandomVar"
    tmp
}

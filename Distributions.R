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
                Quantile = function(p)
                {
                    qbeta(p, alpha, beta)
                },
                Roll = function(n)
                {
                    rbeta(n, alpha, beta)
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
circle <-
function (x,y, radius=1, n=100) 
{
  theta = 2*pi*(0:n)/n
  X = x + radius*cos(theta)
  Y = y + radius*sin(theta)
  lines(X,Y)
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
                Quantile = function(p)
                {
                    qexp(p,rate)
                },
                Roll = function(n)
                {
                    rexp(n,rate)
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
                Quantile = function(p)
                {
                    qgamma(p, params[1], rate)
                },
                Roll = function(n)
                {
                    rgamma(n, params[1], rate)
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
                Quantile = function(p)
                {
                    qpois(p,lambda)
                },
                Roll = function(n)
                {
                    rpois(n,lambda)
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
                Quantile = function(p)
                {
                    qweibull(p, params[1], rate)
                },
                Roll = function(n)
                {
                    rweibull(n, params[1], rate)
                },
                Mean<-params[1]*params[2]
                ,
                Variance<- params[1]^2
    )
    class(tmp)="RandomVar"
    tmp
}

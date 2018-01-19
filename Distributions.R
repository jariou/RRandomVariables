Distributions<-list(
			"Beta", 
			"Binomial", 
			"Exponential", 
			"Gamma", 
			"Poisson", 
			"Weibull", 
			"CentralT", 
			"NonCentralT", 
			"Geometric", 
			"Lognormal",
			"Normal",
			"Uniform"
			)
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
CentralT <-
function (params) 
{
    df  = params[1]

    tmp <- list(
                Model = "CentralT"
                ,
                Parameters = params
                ,
                Pdf = function(x)
                {
                    dt(x, df, 0)
                },
                Cdf = function(x)
                {
                    pt(x, df, 0)
                },
                Quantile = function(q)
                {
                    qt(q, df, 0)
                },
                Roll = function(sampleSize)
                {
                    rt(sampleSize, df, 0)
                },
                Mean<-params[1]*params[2]
                ,
                Variance<- params[1]^2
    )
    class(tmp)="RandomVar"
    tmp
}
Geometric <-
function (params) 
{
    p = params[1]
    
    tmp <- list(
                Model = "Geometric"
                ,
                Parameters = params
                ,
                Pdf = function(x)
                {
                    dgeom(x,p)
                },
                Cdf = function(x)
                {
                    pgeom(x,p)
                },
                Quantile = function(q)
                {
                    qgeom(q,p)
                },
                Roll = function(sampleSize)
                {
                    rgeom(sampleSize,p)
                },
                Mean<-params[1]
                ,
                Variance<- params[1]^2
    )
    class(tmp)="RandomVar"
    tmp
}
Lognormal <-
function (params) 
{
    mu      = params[1]
    sigmaSq = params[2]
    
    tmp <- list(
                Model = "Lognormal"
                ,
                Parameters = params
                ,
                Pdf = function(x)
                {
                    dlnorm(x, mu, sigmaSq)
                },
                Cdf = function(x)
                {
                    plnorm(x, mu, sigmaSq)
                },
                Quantile = function(p)
                {
                    qlnorm(p, mu, sigmaSq)
                },
                Roll = function(sampleSize)
                {
                    rlnorm(sampleSize, mu, sigmaSq)
                },
                Mean<-params[1]*params[2]
                ,
                Variance<- params[1]^2
    )
    class(tmp)="RandomVar"
    tmp
}
NonCentralT <-
function (params) 
{
    df  = params[1]
    ncp = params[2]
    
    tmp <- list(
                Model = "NonCentralT"
                ,
                Parameters = params
                ,
                Pdf = function(x)
                {
                    dt(x, df, ncp)
                },
                Cdf = function(x)
                {
                    pt(x, df, ncp)
                },
                Quantile = function(p)
                {
                    qt(p, df, ncp)
                },
                Roll = function(n)
                {
                    rt(n, df, ncp)
                },
                Mean<-params[1]*params[2]
                ,
                Variance<- params[1]^2
    )
    class(tmp)="RandomVar"
    tmp
}
Normal <-
function (params) 
{
    mu      = params[1]
    sigmaSq = params[2]
    
    tmp <- list(
                Model = "Normal"
                ,
                Parameters = params
                ,
                Pdf = function(x)
                {
                    dnorm(x, mu, sigmaSq)
                },
                Cdf = function(x)
                {
                    pnorm(x, mu, sigmaSq)
                },
                Quantile = function(p)
                {
                    qnorm(p, mu, sigmaSq)
                },
                Roll = function(n)
                {
                    rnorm(n, mu, sigmaSq)
                },
                Mean<-params[1]*params[2]
                ,
                Variance<- params[1]^2
    )
    class(tmp)="RandomVar"
    tmp
}
Uniform <-
function (params) 
{
    minu      = params[1]
    max = params[2]
    
    tmp <- list(
                Model = "Uniform"
                ,
                Parameters = params
                ,
                Pdf = function(x)
                {
                    dunif(x, min, max)
                },
                Cdf = function(x)
                {
                    punif(x, min, max)
                },
                Quantile = function(q)
                {
                    qunif(q, min, max)
                },
                Roll = function(sampleSize)
                {
                    runif(sampleSize, min, max)
                },
                Mean<-params[1]*params[2]
                ,
                Variance<- params[1]^2
    )
    class(tmp)="RandomVar"
    tmp
}


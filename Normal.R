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

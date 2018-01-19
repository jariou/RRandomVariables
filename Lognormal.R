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

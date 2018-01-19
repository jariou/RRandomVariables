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

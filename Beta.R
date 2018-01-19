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

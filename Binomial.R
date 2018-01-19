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

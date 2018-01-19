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

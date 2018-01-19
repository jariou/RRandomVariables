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

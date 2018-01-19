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

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
                Quantile = function(q)
                {
                    qexp(q,rate)
                },
                Roll = function(sampleSize)
                {
                    rexp(sampleSize,rate)
                },
                Mean<-params[1]
                ,
                Variance<- params[1]^2
    )
    
    #tmp = list(Model="Exponential", Parameters=params, Pdf=myPdf,Cdf=myCdf,Quantile=myQuantile,Roll=myRoll, Mean=myMean, Variance=myVariance)
    class(tmp)="RandomVar"
    tmp
}

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

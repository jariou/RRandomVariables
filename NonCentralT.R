NonCentralT <-
function (params) 
{
    df  = params[1]
    ncp = params[2]
    
    tmp <- list(
                Model = "NonCentralT"
                ,
                Parameters = params
                ,
                Pdf = function(x)
                {
                    dt(x, df, ncp)
                },
                Cdf = function(x)
                {
                    pt(x, df, ncp)
                },
                Quantile = function(p)
                {
                    qt(p, df, ncp)
                },
                Roll = function(n)
                {
                    rt(n, df, ncp)
                },
                Mean<-params[1]*params[2]
                ,
                Variance<- params[1]^2
    )
    class(tmp)="RandomVar"
    tmp
}

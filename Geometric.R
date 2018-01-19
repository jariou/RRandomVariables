Geometric <-
function (params) 
{
    p = params[1]
    
    tmp <- list(
                Model = "Geometric"
                ,
                Parameters = params
                ,
                Pdf = function(x)
                {
                    dgeom(x,p)
                },
                Cdf = function(x)
                {
                    pgeom(x,p)
                },
                Quantile = function(q)
                {
                    qgeom(q,p)
                },
                Roll = function(sampleSize)
                {
                    rgeom(sampleSize,p)
                },
                Mean<-params[1]
                ,
                Variance<- params[1]^2
    )
    class(tmp)="RandomVar"
    tmp
}

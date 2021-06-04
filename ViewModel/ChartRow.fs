namespace ViewModel

open LiveCharts.Wpf
open System
open LiveCharts

type ChartRow = 
    {
        Product: string
        Date: DateTime
        Series: LineSeries
    }

type ChartRowViewModel( row : ChartRow) = 
    inherit ViewModelBase()

    let mutable configRec = row
    let sq x = x*x    
    let logNor (r: float)  (vol: float) (S: float) (x: float) =
        1.0/(x*vol*sqrt(2.0*Math.PI))*exp(sq (log x - log S - r)/(-2.0*sq vol))

    member this.Product
        with get() = configRec.Product
        and set(x) = 
            configRec <- {configRec with Product = x }
            base.Notify("Product")
    
    member this.Date
        with get() = configRec.Date
        and set(x) = 
            configRec <- {configRec with Date = x }
            base.Notify("Date") 

    member this.Series
        with get() = configRec.Series
        and set(x) = 
            configRec <- {configRec with Series = x }
            base.Notify("Series") 

    member this.Calculate(data : DataConfiguration, prices : ProductsInfo) = 
        let (f, e) = match (prices.ContainsKey this.Product) with
                | true -> 
                    let (price, _, vol, _)  = prices.[this.Product]
                    let T = (float)(this.Date - DateTime.Now).TotalDays / 365.25
                    let r = if data.ContainsKey "MONEY::R_YEARLY" then float data.[ "MONEY::R_YEARLY" ] else 0.0 // R_YEARLY
                    (logNor (r*T) (vol*T) price, (int)(price*1.6))
                | _ -> 
                    let g = fun (x: float) -> 0.0
                    (g, 10)
        let ls = LineSeries()
        ls.Title <- sprintf "%s" this.Product
        let series = seq { for i in 1 .. e do yield (f (double i)) }
        ls.Values <- ChartValues<float> series

        this.Series <- ls



    
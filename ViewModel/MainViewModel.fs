namespace ViewModel

open System
open System.Collections.ObjectModel
open LiveCharts;
open LiveCharts.Wpf;

//Strating point of the viewmodel that drives the UI
//It aggregates all relevant parts of the UI, and exposes them via properties
type MainViewModel() = 
    inherit ViewModelBase()

    let barier = ObservableCollection<BarierViewModel>()
    let options = ObservableCollection<OptionViewModel>()
    let payments = ObservableCollection<PaymentViewModel>()
    let swaps = ObservableCollection<SwapViewModel>()
    let data = ObservableCollection<ConfigurationViewModel>()
    let calculationParameters = ObservableCollection<ConfigurationViewModel>()
    let products = ObservableCollection<ProductViewModel>()

    let getProducts () = products |> Seq.map (fun p -> (p.Name , (p.Price, p.Currency,  p.Volatility, p.Drift))) |> Map.ofSeq
    let getDataConfiguration () = data |> Seq.map (fun conf -> (conf.Key , conf.Value)) |> Map.ofSeq
    let getCalculationConfiguration () = calculationParameters |> Seq.map (fun conf -> (conf.Key , conf.Value)) |> Map.ofSeq
    
    (* add some dummy data rows *)
    do
        products.Add(ProductViewModel { Name = "AAPL"; Price=124.97; Currency="USD"; Volatility=0.12; Drift=0.06})
        products.Add(ProductViewModel { Name = "MSFT"; Price=248.28; Currency="USD"; Volatility=0.10; Drift=0.05})
        products.Add(ProductViewModel { Name = "GOOG"; Price=2316.16; Currency="USD"; Volatility=0.8; Drift=0.055})

        data.Add(ConfigurationViewModel { Key = "FX::USDPLN"; Value = "3.76" })
        data.Add(ConfigurationViewModel { Key = "FX::USDEUR"; Value = "0.87" })
        data.Add(ConfigurationViewModel { Key = "FX::EURGBP"; Value = "0.90" })
        data.Add(ConfigurationViewModel { Key = "MONEY::R_YEARLY"; Value = "0.03" })

        calculationParameters.Add(ConfigurationViewModel { Key = "t"; Value = "1.0" })
        calculationParameters.Add(ConfigurationViewModel { Key = "monteCarlo::runs"; Value = "100" })
        calculationParameters.Add(ConfigurationViewModel { Key = "binomial::size"; Value = "100" })
        calculationParameters.Add(ConfigurationViewModel { Key = "valuation::baseCurrency"; Value = "USD" })
        calculationParameters.Add(ConfigurationViewModel { Key = "valuation::knownCurrencies"; Value = "USD PLN EUR GBP" })
        calculationParameters.Add(ConfigurationViewModel { Key = "swap::steps"; Value = "100" })

    let summary = ObservableCollection<SummaryRow>()

    (* trade commands *)
    let refreshSummary() = 
        summary.Clear()
        let all = seq{yield! Seq.map (fun x -> x:> Trade) payments;
                      yield! Seq.map (fun x -> x:> Trade) options;
                      yield! Seq.map (fun x -> x:> Trade) barier;
                      yield! Seq.map (fun x -> x:> Trade) swaps}
        
        all 
        |> Seq.choose(fun t -> t.getValue) // find correctly evaluated trades
        |> Seq.groupBy(fun m -> m.Currency)  // group by currency
        |> Seq.map(fun (ccy, v) -> { Currency = ccy; Value = v |> Seq.map (fun m -> m.Value) |> Seq.sum }) // extract values, calculate a sum
        |> Seq.iter(summary.Add) // add to summary page

    let calculateFun _ = do
            payments |> Seq.iter(fun trade -> trade.Calculate(getDataConfiguration (), getCalculationConfiguration ()))
            options |> Seq.iter(fun trade -> trade.Calculate(getDataConfiguration (), getCalculationConfiguration (), getProducts ()))
            barier |> Seq.iter(fun trade -> trade.Calculate(getDataConfiguration (), getCalculationConfiguration (), getProducts ()))
            swaps |> Seq.iter(fun trade -> trade.Calculate(getDataConfiguration (), getCalculationConfiguration (), getProducts ()))
            refreshSummary()

    let calculate = SimpleCommand calculateFun

    let addTrade = SimpleCommand(fun _ -> 
            let currentConfig = getCalculationConfiguration ()
            PaymentRecord.Random currentConfig |> PaymentViewModel |> payments.Add
            )

    let addOption = SimpleCommand(fun _ -> 
            let currentConfig = getCalculationConfiguration ()
            OptionRecord.Random currentConfig |> OptionViewModel |> options.Add
            )

    let addBarier = SimpleCommand(fun _ -> 
        let currentConfig = getCalculationConfiguration ()
        BarierRecord.Random currentConfig |> BarierViewModel |> barier.Add
        )

    let addSwap = SimpleCommand(fun _ -> 
        let currentConfig = getCalculationConfiguration ()
        SwapRecord.Random currentConfig |> SwapViewModel |> swaps.Add
        )

    let removeTrade = SimpleCommand(fun trade -> payments.Remove (trade :?> PaymentViewModel) |> ignore)
    let clearTrades = SimpleCommand(fun _ -> payments.Clear () )

    let removeOption = SimpleCommand(fun option -> options.Remove (option :?> OptionViewModel) |> ignore)
    let clearOptions = SimpleCommand(fun _ -> options.Clear () )

    let removeBarier = SimpleCommand(fun b -> barier.Remove (b :?> BarierViewModel) |> ignore)
    let clearBarier = SimpleCommand(fun _ -> barier.Clear () )

    let removeSwap = SimpleCommand(fun swap -> swaps.Remove (swap :?> SwapViewModel) |> ignore)
    let clearSwap = SimpleCommand(fun _ -> swaps.Clear () )

    (* charting *)
    
    let chartRows = ObservableCollection<ChartRowViewModel>()
    let chartSeries = SeriesCollection()

    let updateSeriesFun () =
        chartSeries.Clear()
        Seq.iter (fun (row: ChartRowViewModel) -> row.Calculate (getDataConfiguration (), getProducts ())) chartRows
        Seq.iter (fun (x : ChartRowViewModel) -> chartSeries.Add(x.Series)) chartRows

    let clearSeriesFun () =
        chartRows.Clear ()
        updateSeriesFun ()

    let removeSeriesFun (row: obj) =
        chartRows.Remove (row :?> ChartRowViewModel) |> ignore
        updateSeriesFun ()

    let predefinedChartFunctions = [| (fun x -> sin x); (fun x -> x); (fun x -> x*x) |] 

    let addChartSeriesFun _ = do
        let knownProducts = [| "AAPL"; "GOOG"; "MSFT" |]
        let random = new Random ()
        let row = ChartRowViewModel {
            Product = knownProducts.[ random.Next(knownProducts.Length) ]
            Date = (DateTime.Now.AddMonths (PaymentRecord.sysRandom.Next(1, 6))).Date
            Series = LineSeries()
        }
        row.Calculate (getDataConfiguration (), getProducts ())
        chartRows.Add(row)
        chartSeries.Add(row.Series)

    let removeSeries = SimpleCommand(fun row -> removeSeriesFun row)
    let clearSeries = SimpleCommand(fun _ -> clearSeriesFun () )    

    let updateSeries = SimpleCommand(fun _ -> updateSeriesFun())

    let addChartSeries = SimpleCommand addChartSeriesFun

    (* add a few series for a good measure *)
    do
        addChartSeriesFun ()

    (* market data commands *)
    let addMarketDataRecord = SimpleCommand (fun _ -> data.Add(ConfigurationViewModel { Key = ""; Value = "" }))
    let removeMarketDataRecord = SimpleCommand (fun record -> data.Remove(record :?> ConfigurationViewModel) |> ignore)
    let clearMarketDataRecord = SimpleCommand (fun _ -> data.Clear ())

    (* calculation parameters commands *)
    let addCalcParameterRecord = SimpleCommand (fun _ -> calculationParameters.Add(ConfigurationViewModel { Key = ""; Value = "" }))
    let removeCalcParameterRecord = SimpleCommand (fun record -> calculationParameters.Remove(record :?> ConfigurationViewModel) |> ignore)
    let clearCalcParameterRecord = SimpleCommand (fun _ -> calculationParameters.Clear ())

    let addProductParameterRecord = SimpleCommand (fun _ -> products.Add(ProductViewModel { Name = ""; Price=0.0; Currency=""; Volatility=0.0; Drift=0.0}))
    let removeProductParameterRecord = SimpleCommand (fun record -> products.Remove(record :?> ProductViewModel) |> ignore)
    let clearProductParameterRecord = SimpleCommand (fun _ -> products.Clear ())

    (* automatically update summary when dependency data changes (entries added/removed)  *)
    do
        payments.CollectionChanged.Add calculateFun
        options.CollectionChanged.Add calculateFun
        barier.CollectionChanged.Add calculateFun
        data.CollectionChanged.Add calculateFun
        swaps.CollectionChanged.Add calculateFun
        calculationParameters.CollectionChanged.Add calculateFun

    (* commands *)
    member this.AddTrade = addTrade 
    member this.RemoveTrade = removeTrade
    member this.ClearTrades = clearTrades
    member this.Calculate = calculate

    member this.AddOption = addOption
    member this.RemoveOption = removeOption
    member this.ClearOptions = clearOptions

    member this.AddBarier = addBarier
    member this.RemoveBarier = removeBarier
    member this.ClearBarier = clearBarier

    member this.AddSwap = addSwap
    member this.RemoveSwap = removeSwap
    member this.ClearSwap = clearSwap

    member this.AddMarketData = addMarketDataRecord
    member this.RemoveMarketData = removeMarketDataRecord
    member this.ClearMarketData = clearMarketDataRecord
    
    member this.AddCalcParameter = addCalcParameterRecord 
    member this.RemoveCalcParameter = removeCalcParameterRecord 
    member this.ClearCalcParameter = clearCalcParameterRecord
    
    member this.AddProductParameter = addProductParameterRecord 
    member this.RemoveProductParameter = removeProductParameterRecord 
    member this.ClearProductParameter = clearProductParameterRecord 


    (* data fields *)
    member this.Trades = payments
    member this.Options = options
    member this.Bariers = barier
    member this.Data = data
    member this.Swaps = swaps
    member this.CalculationParameters = calculationParameters
    member this.Products = products
    member this.ChartRows = chartRows
    member this.Summary = summary

    (* charting *)

    member this.ChartSeries = chartSeries
    member this.AddChartSeries = addChartSeries
    member this.UpdateChartSeries = updateSeries
    member this.ClearChartSeries = clearSeries
    member this.RemoveChartSeries = removeSeries
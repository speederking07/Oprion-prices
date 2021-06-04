namespace ViewModel

open System

type SwapType 
    = Higher
    | Lower

type SwapRecord =
    {
        TradeName  : string
        Type       : SwapType
        Product    : string
        Expiry     : DateTime
        Varience   : float
        Notional   : float
    }
    
    (* Simple utility method for creating a random payment. *)
    static member sysRandom = System.Random()
    static member Random(configuration : CalculationConfiguration) = 
        (* We pick a random currency either from given short list, or from valuation::knownCurrencies config key *)
        let knownProducts = [| "AAPL"; "GOOG"; "MSFT" |]
        let types = [| Higher; Lower |]
        
        {
            TradeName = sprintf "Option%04d" (SwapRecord.sysRandom.Next(9999))
            Type      = types.[ SwapRecord.sysRandom.Next(types.Length) ]
            Product   = knownProducts.[ SwapRecord.sysRandom.Next(knownProducts.Length) ]
            Expiry    = (DateTime.Now.AddMonths (SwapRecord.sysRandom.Next(1, 6))).Date
            Varience  = 20.0*(SwapRecord.sysRandom.NextDouble())
            Notional  = 10.0*(SwapRecord.sysRandom.NextDouble())
        }


type SwapValuationInputs = 
    {
        Swap : SwapRecord
        Data : DataConfiguration
        CalculationsParameters: CalculationConfiguration
        Prices : ProductsInfo
    }

type SwapValuationModel(inputs: SwapValuationInputs) = 
    let erf x =
        if (Double.IsNegativeInfinity x) then 0.0
        elif (Double.IsPositiveInfinity x) then 2.0
        else
            let z = abs x
            let t = 1.0 / (1.0 + 0.5 * z) 
            let res = t * exp (-z * z - 1.26551223 + t * (1.00002368 + t * (0.37409196 + t * (0.09678418 + t * (-0.18628806 + t * (0.27886807 + t * (-1.13520398 + t * (1.48851587 + t * (-0.82215223 + t * 0.17087277)))))))))  
            if (x <= 0.0) then res else 2.0 - res

    let phi x = 0.5 * erf (x/sqrt(2.0))

    let currencyConvertion (m: Money) =
        let targetCcy =
            match inputs.CalculationsParameters.TryFind "valuation::baseCurrency" with
                | Some ccy -> ccy
                | None -> m.Currency
        let fxRateKey = sprintf "FX::%s%s" targetCcy m.Currency         
        let fxRate = if inputs.Data.ContainsKey fxRateKey then float inputs.Data.[ fxRateKey ] else 1.0 // lookup FX rate
        let finalCcy = if inputs.Data.ContainsKey fxRateKey then targetCcy else m.Currency
        { Value = m.Value / fxRate; Currency = finalCcy }

    member this.Calculate() : Money option = 
        let product_data = lazy(inputs.Prices.[inputs.Swap.Product])
        let rYearly = if inputs.Data.ContainsKey "MONEY::R_YEARLY" then float inputs.Data.[ "MONEY::R_YEARLY" ] else 0.0 // R_YEARLY
        let daysDiff = (inputs.Swap.Expiry - DateTime.Now).TotalDays
        let moneyValue = exp(-rYearly*daysDiff/365.25)

        let calcEurCall (currPrice: float) (callPrice: float) (vol: float) (r: float) (T: float) =
            let d_n = log (currPrice / callPrice) + (r + vol*vol/2.) * T
            let d_d = vol * sqrt(T)
            let d = d_n / d_d
            currPrice * phi(d) - callPrice * exp(-r*T) *phi (d - vol * sqrt(T))

        let calcEurPut (currPrice: float) (putPrice: float) (vol: float) (r: float) (T: float) =
            let d_n = log (currPrice / putPrice) + (r + vol*vol/2.) * T
            let d_d = vol * sqrt(T)
            let d = d_n / d_d
            putPrice * exp(-r*T) * phi (vol*sqrt(T) - d) - currPrice * phi (-d)

        let calcSwap (currPrice: float) (vol: float) (r: float) (T: float) (varience: float) (steps: int) =
            let step = 1.0 / (float)steps
            let future = exp(r*T) * currPrice
            let listOfPut = [ for i in 1 .. steps -> ((float)(i-1)* step, (float)i* step) ]
            let listOfCall = [ for i in (steps+1) .. steps*2 -> ((float)(i-1)* step, (float)i* step) ]
            let putPrices = List.map (fun (prev, curr) -> (curr - prev)/(curr*curr) * (calcEurPut currPrice (currPrice*curr) vol r T) / future) listOfPut
            let callPrices = List.map (fun (prev, curr) -> (curr - prev)/(curr*curr) * (calcEurCall currPrice (currPrice*curr) vol r T) / future) listOfCall
            let sumOfOptions = 2.0 * (List.sum putPrices + List.sum callPrices) / T
            sumOfOptions - exp(-r*T) * varience**2.0

        if inputs.Prices.ContainsKey inputs.Swap.Product
        then 
            let (_, currency, vol, _) = product_data.Force()
            let value = 
                match inputs.Swap.Type with
                    | Higher -> (vol * (daysDiff / 365.25) - inputs.Swap.Varience)
                    | Lower -> -(vol * (daysDiff / 365.25) - inputs.Swap.Varience)
            Some (currencyConvertion({Value = value*moneyValue*(float)inputs.Swap.Notional; Currency=currency}))
        else None
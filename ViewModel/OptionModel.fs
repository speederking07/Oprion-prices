namespace ViewModel

open System
open MonteCarlo

type OptionType 
    = EuropeanCall 
    | EuropeanPut
    | AmericanCall
    | AmericanPut
    | AsianCall
    | AsianPut
    | LookbackCall
    | LookbackPut

type OptionRecord =
    {
        TradeName  : string
        Type       : OptionType
        Product    : string
        Expiry     : DateTime
        Price      : float
        Delta      : int
    }
    
    (* Simple utility method for creating a random payment. *)
    static member sysRandom = System.Random()
    static member Random(configuration : CalculationConfiguration) = 
        (* We pick a random currency either from given short list, or from valuation::knownCurrencies config key *)
        let knownProducts = [| "AAPL"; "GOOG"; "MSFT" |]
        let types = [| EuropeanCall; EuropeanPut; AmericanCall; AmericanPut; AsianCall; AsianPut; LookbackCall; LookbackPut |]
        
        {
            TradeName = sprintf "Option%04d" (OptionRecord.sysRandom.Next(9999))
            Type      = types.[ OptionRecord.sysRandom.Next(types.Length) ]
            Product   = knownProducts.[ OptionRecord.sysRandom.Next(knownProducts.Length) ]
            Expiry    = (DateTime.Now.AddMonths (OptionRecord.sysRandom.Next(1, 6))).Date
            Price     = 50.0 + 100.0*(OptionRecord.sysRandom.NextDouble())
            Delta     = OptionRecord.sysRandom.Next(1, 20)
        }


type OptionValuationInputs = 
    {
        Option : OptionRecord
        Data : DataConfiguration
        CalculationsParameters: CalculationConfiguration
        Prices : ProductsInfo
    }

type OptionValuationModel(inputs: OptionValuationInputs) = 
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
        let product_data = lazy(inputs.Prices.[inputs.Option.Product])
        let steps = if inputs.CalculationsParameters.ContainsKey "binomial::size" then int inputs.CalculationsParameters.[ "binomial::size" ] else 100
        let runs = if inputs.CalculationsParameters.ContainsKey "monteCarlo::runs" then int inputs.CalculationsParameters.[ "monteCarlo::runs" ] else 100
        let rYearly = if inputs.Data.ContainsKey "MONEY::R_YEARLY" then float inputs.Data.[ "MONEY::R_YEARLY" ] else 0.0 // R_YEARLY
        let daysDiff = (inputs.Option.Expiry - DateTime.Now).TotalDays
        let moneyValue = exp(-rYearly*daysDiff/365.25)

        let calcLookCall (currPrice: float) (vol: float) (r: float) (T: float) (runs: int) =
            let paths = get_seq_of_paths (int (ceil (T*365.))) currPrice r vol T (new Random())
            let value (p: float list) =
                let last = List.last p
                let min = List.min p
                last - min
            let res = Seq.map value paths
            let list = Seq.toList (Seq.take runs res)
            List.average list

        let calcLookPut (currPrice: float) (vol: float) (r: float) (T: float) (runs: int) =
            let paths = get_seq_of_paths (int (ceil (T*365.))) currPrice r vol T (new Random())
            let value (p: float list) =
                let last = List.last p
                let max = List.max p
                max - last
            let res = Seq.map value paths
            let list = Seq.toList (Seq.take runs res)
            List.average list

        let calcAsianCall (currPrice: float) (callPrice: float) (vol: float) (r: float) (T: float) (runs: int) =
            let paths = get_seq_of_paths (int (ceil (T*365.))) currPrice r vol T (new Random())
            let value (p: float list) =
                let avg = List.average p
                max 0.0 (avg - callPrice)
            let res = Seq.map value paths
            let list = Seq.toList (Seq.take runs res)
            List.average list

        let calcAsianPut (currPrice: float) (callPrice: float) (vol: float) (r: float) (T: float) (runs: int) =
            let paths = get_seq_of_paths (int (ceil (T*365.))) currPrice r vol T (new Random())
            let value (p: float list) =
                let avg = List.average p
                max 0.0 (callPrice - avg)
            let res = Seq.map value paths
            let list = Seq.toList (Seq.take runs res)
            List.average list

        let calcAmCall (currPrice: float) (callPrice: float) (vol: float) (r: float) (T: float) (steps: int) =
            let stepVol = vol * sqrt (T / (float)steps)
            let stepR = r * T / (float)steps
            let moveUp = exp(stepVol + stepR) 
            let moveDown = 1.0/moveUp
            let probOfMoveUp = (exp(stepR)-moveDown)/(moveUp-moveDown)
            let getPrice (move: int) (s: int) =
                currPrice * moveUp ** (float)((move+s)/2) * moveDown ** (float)((s-move)/2)
            let rec go (move: int) (s: int) (S: int) (anc: Map<int*int, float>) =
                if s = S 
                then (max 0.0 (getPrice move s - callPrice), anc)
                else 
                    if Map.containsKey (move, s) anc
                    then (Map.find (move, s) anc, anc)
                    else
                        let (upValue, upAnc) = go (move+1) (s+1) S anc
                        let (downValue, downAnc) = go (move-1) (s+1) S upAnc
                        let solution = max ((getPrice move s - callPrice) * (float)(S - s) *stepR) (probOfMoveUp * upValue + (1. - probOfMoveUp) * downValue)
                        (solution, Map.add (move, s) solution downAnc)
            let (res, _) = go 0 0 steps Map.empty
            res

        let calcAmPut (currPrice: float) (callPrice: float) (vol: float) (r: float) (T: float) (steps: int) =
            let stepVol = vol * sqrt (T / (float)steps)
            let stepR = r * T / (float)steps
            let moveUp = exp(stepVol + stepR) 
            let moveDown = 1.0/moveUp
            let probOfMoveUp = (exp(stepR)-moveDown)/(moveUp-moveDown)
            let getPrice (move: int) (s: int) =
                currPrice * moveUp ** (float)((move+s)/2) * moveDown ** (float)((s-move)/2)
            let rec go (move: int) (s: int) (S: int) (anc: Map<int*int, float>) =
                if s = S 
                then (max 0.0 (callPrice - getPrice move s), anc)
                else 
                    if Map.containsKey (move, s) anc
                    then (Map.find (move, s) anc, anc)
                    else
                        let (upValue, upAnc) = go (move+1) (s+1) S anc
                        let (downValue, downAnc) = go (move-1) (s+1) S upAnc
                        let solution = max ((callPrice - getPrice move s) * (float)(S - s) *stepR) (probOfMoveUp * upValue + (1. - probOfMoveUp) * downValue)
                        (solution, Map.add (move, s) solution downAnc)
            let (res, _) = go 0 0 steps Map.empty
            res

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

        if inputs.Prices.ContainsKey inputs.Option.Product
        then 
            let (price, currency, vol, _) = product_data.Force()
            let value = 
                match inputs.Option.Type with
                    | AmericanPut -> calcAmPut price inputs.Option.Price vol rYearly (daysDiff / 365.25) steps
                    | AmericanCall -> calcAmCall price inputs.Option.Price vol rYearly (daysDiff / 365.25) steps
                    | EuropeanCall -> calcEurCall price inputs.Option.Price vol rYearly (daysDiff / 365.25)
                    | EuropeanPut -> calcEurPut price inputs.Option.Price vol rYearly (daysDiff / 365.25)
                    | AsianCall -> calcAsianCall price inputs.Option.Price vol rYearly (daysDiff / 365.25) runs
                    | AsianPut -> calcAsianPut price inputs.Option.Price vol rYearly (daysDiff / 365.25) runs
                    | LookbackCall -> calcLookCall price vol rYearly (daysDiff / 365.25) runs
                    | LookbackPut -> calcLookPut price vol rYearly (daysDiff / 365.25) runs
            Some (currencyConvertion({Value = value*moneyValue*(float)inputs.Option.Delta; Currency=currency}))
        else None
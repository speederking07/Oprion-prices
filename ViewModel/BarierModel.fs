namespace ViewModel

open System
open MonteCarlo

type BarierType 
    = KnockOutCall
    | KnockOutPut
    | KnockInCall
    | KnockInPut

type BarierRecord =
    {
        TradeName  : string
        Type       : BarierType
        Product    : string
        Expiry     : DateTime
        Price      : float
        Barier     : float
        Delta      : int
    }
    
    (* Simple utility method for creating a random payment. *)
    static member sysRandom = System.Random()
    static member Random(configuration : CalculationConfiguration) = 
        (* We pick a random currency either from given short list, or from valuation::knownCurrencies config key *)
        let knownProducts = [| "AAPL"; "GOOG"; "MSFT" |]
        let types = [| KnockOutCall |]
        
        {
            TradeName = sprintf "Barier%04d" (BarierRecord.sysRandom.Next(9999))
            Type      = types.[ BarierRecord.sysRandom.Next(types.Length) ]
            Product   = knownProducts.[ BarierRecord.sysRandom.Next(knownProducts.Length) ]
            Expiry    = (DateTime.Now.AddMonths (BarierRecord.sysRandom.Next(1, 6))).Date
            Price     = 50.0 + 100.0*(BarierRecord.sysRandom.NextDouble())
            Barier    = 60.0 + 80.0*(BarierRecord.sysRandom.NextDouble())
            Delta     = BarierRecord.sysRandom.Next(1, 10)
        }


type BarierValuationInputs = 
    {
        Option : BarierRecord
        Data : DataConfiguration
        CalculationsParameters: CalculationConfiguration
        Prices : ProductsInfo
    }

type BarierValuationModel(inputs: BarierValuationInputs) = 
    let currencyConvertion (m: Money) =
        let targetCcy =
            match inputs.CalculationsParameters.TryFind "valuation::baseCurrency" with
                | Some ccy -> ccy
                | None -> m.Currency
        let fxRateKey = sprintf "FX::%s%s" targetCcy m.Currency         
        let fxRate = if inputs.Data.ContainsKey fxRateKey then float inputs.Data.[ fxRateKey ] else 1.0 // lookup FX rate
        let finalCcy = if inputs.Data.ContainsKey fxRateKey then targetCcy else m.Currency
        { Value = m.Value / fxRate; Currency = finalCcy }

    let calcCallKO (currPrice: float) (callPrice: float) (barier: float) (vol: float) (r: float) (T: float) (steps: int) =
        let stepVol = vol * sqrt (T / (float)steps)
        let stepR = r * T / (float)steps
        let moveUp = exp(stepVol + stepR) 
        let moveDown = 1.0/moveUp
        let probOfMoveUp = (exp(stepR)-moveDown)/(moveUp-moveDown)
        let getPrice (move: int) (s: int) =
            currPrice * moveUp ** (float)((move+s)/2) * moveDown ** (float)((s-move)/2)
        let rec go (move: int) (s: int) (S: int) (anc: Map<int*int, float>) =
            let price = getPrice move s
            if price > barier
            then (0.0, anc)
            else
                if s = S 
                then (max 0.0 (price - callPrice), anc)
                else 
                    if Map.containsKey (move, s) anc
                    then (Map.find (move, s) anc, anc)
                    else
                        let (upValue, upAnc) = go (move+1) (s+1) S anc
                        let (downValue, downAnc) = go (move-1) (s+1) S upAnc
                        let solution = probOfMoveUp * upValue + (1. - probOfMoveUp) * downValue
                        (solution, Map.add (move, s) solution downAnc)
        let (res, _) = go 0 0 steps Map.empty
        res

    let calcPutKO (currPrice: float) (callPrice: float) (barier: float) (vol: float) (r: float) (T: float) (steps: int) =
        let stepVol = vol * sqrt (T / (float)steps)
        let stepR = r * T / (float)steps
        let moveUp = exp(stepVol + stepR) 
        let moveDown = 1.0/moveUp
        let probOfMoveUp = (exp(stepR)-moveDown)/(moveUp-moveDown)
        let getPrice (move: int) (s: int) =
            currPrice * moveUp ** (float)((move+s)/2) * moveDown ** (float)((s-move)/2)
        let rec go (move: int) (s: int) (S: int) (anc: Map<int*int, float>) =
            let price = getPrice move s
            if price < barier
            then (0.0, anc)
            else
                if s = S 
                then (max 0.0 (callPrice - price), anc)
                else 
                    if Map.containsKey (move, s) anc
                    then (Map.find (move, s) anc, anc)
                    else
                        let (upValue, upAnc) = go (move+1) (s+1) S anc
                        let (downValue, downAnc) = go (move-1) (s+1) S upAnc
                        let solution = probOfMoveUp * upValue + (1. - probOfMoveUp) * downValue
                        (solution, Map.add (move, s) solution downAnc)
        let (res, _) = go 0 0 steps Map.empty
        res

    let calcCallKI (currPrice: float) (callPrice: float) (barier: float) (vol: float) (r: float) (T: float) (steps: int) =
        let stepVol = vol * sqrt (T / (float)steps)
        let stepR = r * T / (float)steps
        let moveUp = exp(stepVol + stepR) 
        let moveDown = 1.0/moveUp
        let probOfMoveUp = (exp(stepR)-moveDown)/(moveUp-moveDown)
        let getPrice (move: int) (s: int) =
            currPrice * moveUp ** (float)((move+s)/2) * moveDown ** (float)((s-move)/2)
        let rec go (move: int) (s: int) (S: int) (knock: bool) (anc: Map<bool*int*int, float>) =
            let price = getPrice move s
            let knockIn = knock || (price > barier)
            if s = S 
            then 
                if knockIn
                then (max 0.0 (price - callPrice), anc)
                else (0.0, anc)
            else 
                if Map.containsKey (knockIn, move, s) anc
                then (Map.find (knockIn, move, s) anc, anc)
                else
                    let (upValue, upAnc) = go (move+1) (s+1) S knockIn anc
                    let (downValue, downAnc) = go (move-1) (s+1) S knockIn upAnc
                    let solution = probOfMoveUp * upValue + (1. - probOfMoveUp) * downValue
                    (solution, Map.add (knockIn, move, s) solution downAnc)
        let (res, _) = go 0 0 steps false Map.empty
        res

    let calcPutKI (currPrice: float) (callPrice: float) (barier: float) (vol: float) (r: float) (T: float) (steps: int) =
        let stepVol = vol * sqrt (T / (float)steps)
        let stepR = r * T / (float)steps
        let moveUp = exp(stepVol + stepR) 
        let moveDown = 1.0/moveUp
        let probOfMoveUp = (exp(stepR)-moveDown)/(moveUp-moveDown)
        let getPrice (move: int) (s: int) =
            currPrice * moveUp ** (float)((move+s)/2) * moveDown ** (float)((s-move)/2)
        let rec go (move: int) (s: int) (S: int) (knock: bool) (anc: Map<bool*int*int, float>) =
            let price = getPrice move s
            let knockIn = knock || (price < barier)
            if s = S 
            then 
                if knockIn
                then (max 0.0 (callPrice - price), anc)
                else (0.0, anc)
            else 
                if Map.containsKey (knockIn, move, s) anc
                then (Map.find (knockIn, move, s) anc, anc)
                else
                    let (upValue, upAnc) = go (move+1) (s+1) S knockIn anc
                    let (downValue, downAnc) = go (move-1) (s+1) S knockIn upAnc
                    let solution = probOfMoveUp * upValue + (1. - probOfMoveUp) * downValue
                    (solution, Map.add (knockIn, move, s) solution downAnc)
        let (res, _) = go 0 0 steps false Map.empty
        res

    member this.Calculate() : Money option = 
        let product_data = lazy(inputs.Prices.[inputs.Option.Product])
        let steps = if inputs.CalculationsParameters.ContainsKey "binomial::size" then int inputs.CalculationsParameters.[ "binomial::size" ] else 100
        let rYearly = if inputs.Data.ContainsKey "MONEY::R_YEARLY" then float inputs.Data.[ "MONEY::R_YEARLY" ] else 0.0 // R_YEARLY
        let daysDiff = (inputs.Option.Expiry - DateTime.Now).TotalDays
        let moneyValue = exp(-rYearly*daysDiff/365.25)

        if inputs.Prices.ContainsKey inputs.Option.Product
        then 
            let (price, currency, vol, _) = product_data.Force()
            let value = 
                match inputs.Option.Type with
                    | KnockOutCall -> calcCallKO price (inputs.Option.Price) (inputs.Option.Barier) vol rYearly (daysDiff / 365.25) steps
                    | KnockOutPut -> calcPutKO price (inputs.Option.Price) (inputs.Option.Barier) vol rYearly (daysDiff / 365.25) steps
                    | KnockInCall -> calcCallKI price (inputs.Option.Price) (inputs.Option.Barier) vol rYearly (daysDiff / 365.25) steps
                    | KnockInPut -> calcPutKI price (inputs.Option.Price) (inputs.Option.Barier) vol rYearly (daysDiff / 365.25) steps
            Some (currencyConvertion({Value = value*moneyValue*(float)inputs.Option.Delta; Currency=currency}))
        else None
namespace ViewModel

type ProductRecord = 
    {
        Name : string
        Price : float
        Volatility : float
        Drift : float
        Currency: string
    }
    
type ProductsInfo = Map<string, float*string*float*float>

type ProductViewModel( prodRec : ProductRecord) = 
    inherit ViewModelBase()

    let mutable prodRec = prodRec

    member this.Name
        with get() = prodRec.Name
        and set(x) = 
            prodRec <- {prodRec with Name = x }
            base.Notify("Name")
    
    member this.Price
        with get() = prodRec.Price
        and set(x) = 
            prodRec <- {prodRec with Price = x }
            base.Notify("Price")

    member this.Volatility
        with get() = prodRec.Volatility
        and set(x) = 
            prodRec <- {prodRec with Volatility = x }
            base.Notify("Volatility")

    member this.Drift
        with get() = prodRec.Drift
        and set(x) = 
            prodRec <- {prodRec with Drift = x }
            base.Notify("Drift")

    member this.Currency
        with get() = prodRec.Currency
        and set(x) = 
            prodRec <- {prodRec with Currency = x }
            base.Notify("Currency")
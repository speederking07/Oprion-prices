namespace ViewModel

type SwapViewModel(input : SwapRecord) = 
    inherit ViewModelBase()

    let mutable userInput = input
    let mutable value : Money option = None

    member this.TradeName 
        with get() = userInput.TradeName
        and set(x) = 
            userInput <- {userInput with TradeName = x }
            base.Notify("TradeName")
    
    member this.Type 
        with get() = userInput.Type
        and set(x) = 
            userInput <- {userInput with Type = x }
            base.Notify("Type")

    member this.Product 
        with get() = userInput.Product
        and set(x) = 
            userInput <- {userInput with Product = x }
            base.Notify("Product")

    member this.Expiry 
        with get() = userInput.Expiry
        and set(x) = 
            userInput <- {userInput with Expiry = x }
            base.Notify("Expiry")

    member this.Varience 
        with get() = userInput.Varience
        and set(x) = 
            userInput <- {userInput with Varience = x }
            base.Notify("Varience")

    member this.Notional 
        with get() = userInput.Notional
        and set(x) = 
            userInput <- {userInput with Notional = x }
            base.Notify("Notional")

    member this.Value
        with get() = value
        and set(x) = 
            value <- x
            base.Notify("Value")

    interface Trade with
        member Trade.getValue = value

    member this.Calculate(data : DataConfiguration, calculationParameters : CalculationConfiguration, prices : ProductsInfo) = 
        
        //capture inputs
        let swapInputs : SwapValuationInputs = 
            {
                Swap = 
                         {
                             TradeName = this.TradeName
                             Expiry    = this.Expiry
                             Type      = this.Type
                             Product   = this.Product
                             Varience  = this.Varience
                             Notional  = this.Notional
                         }
                Data = data
                CalculationsParameters = calculationParameters
                Prices = prices
            }
        //calculate
        let calc = SwapValuationModel(swapInputs).Calculate()

        //present to the user
        this.Value <- calc
namespace ViewModel

type OptionViewModel(input : OptionRecord) = 
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

    member this.Price 
        with get() = userInput.Price
        and set(x) = 
            userInput <- {userInput with Price = x }
            base.Notify("Price")

    member this.Delta 
        with get() = userInput.Delta
        and set(x) = 
            userInput <- {userInput with Delta = x }
            base.Notify("Delta")

    member this.Value
        with get() = value
        and set(x) = 
            value <- x
            base.Notify("Value")

    interface Trade with
        member Trade.getValue = value

    member this.Calculate(data : DataConfiguration, calculationParameters : CalculationConfiguration, prices : ProductsInfo) = 
        
        //capture inputs
        let optionInputs : OptionValuationInputs = 
            {
                Option = 
                         {
                             TradeName = this.TradeName
                             Expiry    = this.Expiry
                             Type      = this.Type
                             Product   = this.Product
                             Price     = this.Price
                             Delta     = this.Delta
                         }
                Data = data
                CalculationsParameters = calculationParameters
                Prices = prices
            }
        //calculate
        let calc = OptionValuationModel(optionInputs).Calculate()

        //present to the user
        this.Value <- calc
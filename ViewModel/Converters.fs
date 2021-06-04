namespace ViewModel

open System
open System.Windows.Data
open System.Windows
open System.Globalization

type OptionTypeConverter() =
    interface IValueConverter with
        member this.Convert (value : obj, _ : Type, _ : obj, _ : CultureInfo) =
            match value with
             | null -> null
             | :? OptionType as ovalue -> 
                match ovalue with 
                | EuropeanCall -> box "EuropeanCall"
                | EuropeanPut -> box "EuropeanPut"
                | AmericanCall -> box "AmericanCall"
                | AmericanPut -> box "AmericanPut"
                | AsianCall -> box "AsianCall"
                | AsianPut -> box "AsianPut"
                | LookbackCall -> box "LookbackCall"
                | LookbackPut -> box "LookbackPut"

        member this.ConvertBack (value : obj, _ : Type, _ : obj, _ : CultureInfo) =
            match value with
            | null -> DependencyProperty.UnsetValue
            | _ -> 
                match unbox value with 
                | "EuropeanCall" -> box EuropeanCall
                | "EuropeanPut" -> box EuropeanPut
                | "European Call" -> box EuropeanCall
                | "European Put" -> box EuropeanPut
                | "EC" -> box EuropeanCall
                | "EP" -> box EuropeanPut
                | "AmericanCall" -> box AmericanCall
                | "American Call" -> box AmericanCall
                | "AC" -> box AmericanCall
                | "American Put" -> box AmericanPut
                | "AmericanPut" -> box AmericanPut
                | "AP" -> box AmericanPut
                | "Asian Put" -> box AsianPut
                | "AsianPut" -> box AsianPut
                | "AsP" -> box AsianPut
                | "Asian Call" -> box AsianCall
                | "AsianCall" -> box AsianCall
                | "AsC" -> box AsianCall
                | "LookbackCall" -> box LookbackCall
                | "Lookback Call" -> box LookbackCall
                | "LC" -> box LookbackCall
                | "LookbackPut" -> box LookbackPut
                | "Lookback Put" -> box LookbackPut
                | "LP" -> box LookbackPut
                | _ -> DependencyProperty.UnsetValue


type BarierTypeConverter() =
    interface IValueConverter with
        member this.Convert (value : obj, _ : Type, _ : obj, _ : CultureInfo) =
            match value with
             | null -> null
             | :? BarierType as ovalue -> 
                match ovalue with 
                | KnockOutCall -> box "KnockOutCall"
                | KnockOutPut -> box "KnockOutPut"
                | KnockInCall -> box "KnockInCall"
                | KnockInPut -> box "KnockInPut"

        member this.ConvertBack (value : obj, _ : Type, _ : obj, _ : CultureInfo) =
            match value with
            | null -> DependencyProperty.UnsetValue
            | _ -> 
                match unbox value with 
                | "KnockOutCall" -> box KnockOutCall
                | "KnockOut Call" -> box KnockOutCall
                | "KOC" -> box KnockOutCall
                | "KnockOutPut" -> box KnockOutPut
                | "KnockOut Put" -> box KnockOutPut
                | "KOP" -> box KnockOutPut
                | "KnockInCall" -> box KnockInCall
                | "KnockIn Call" -> box KnockInCall
                | "KIC" -> box KnockInCall
                | "KnockInPut" -> box KnockInPut
                | "KnockIn Put" -> box KnockInPut
                | "KIP" -> box KnockInPut
                | _ -> DependencyProperty.UnsetValue

type SwapTypeConverter() =
    interface IValueConverter with
        member this.Convert (value : obj, _ : Type, _ : obj, _ : CultureInfo) =
            match value with
             | null -> null
             | :? SwapType as ovalue -> 
                match ovalue with 
                | Higher -> box "Higher"
                | Lower -> box "Lower"

        member this.ConvertBack (value : obj, _ : Type, _ : obj, _ : CultureInfo) =
            match value with
            | null -> DependencyProperty.UnsetValue
            | _ -> 
                match unbox value with 
                | "Higher" -> box Higher
                | "H" -> box Higher
                | "Lower" -> box Lower
                | "L" -> box Lower
                | _ -> DependencyProperty.UnsetValue

type PositiveConverter() =
    let parseFloat s =
        match System.Double.TryParse(s) with 
        | true, n -> Some n
        | _ -> None 

    let positive (s:float) =
        if s > 0.0 then Some s else None

    interface IValueConverter with
        member this.Convert (value : obj, _ : Type, _ : obj, _ : CultureInfo) =
            match value with
             | null -> null
             | :? float as ovalue -> box (ovalue.ToString())

        member this.ConvertBack (value : obj, _ : Type, _ : obj, _ : CultureInfo) =
            match value with
            | null -> DependencyProperty.UnsetValue
            | _ -> 
                match Option.bind positive (parseFloat (unbox value)) with
                | Some x -> box x
                | None -> DependencyProperty.UnsetValue

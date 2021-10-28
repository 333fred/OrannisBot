module Util

open Remora.Discord.Core
open Remora.Results

    let (|Defined|Undefined|) (o : Optional<'T>) =
        match o.HasValue with
        | true -> Defined(o.Value)
        | false -> Undefined(())


    let (|Ok|Error|) (r : Result) =
        match r.IsSuccess with
        | true -> Ok
        | false -> Error(r.Error)


    type ResultBuilder () =
        member this.Bind(x, f) =
            match x with
            | Result.Ok o -> f o
            | Result.Error e -> Microsoft.FSharp.Core.Result.Error e
        member this.Return(value) = Result.Ok value
        member this.ReturnFrom(value) = value


    let result = new ResultBuilder()

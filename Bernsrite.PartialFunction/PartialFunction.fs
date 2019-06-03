namespace Bernsrite.PartialFunction

type PartialFunction<'input, 'output>
    (
        isDefinedAt : 'input -> bool,
        func : 'input -> 'output
    ) =

    member __.IsDefinedAt(input) =
        isDefinedAt input

    member __.Item(input) =
        if isDefinedAt input then
            func input
        else
            failwith "Invalid input"

    member private __.Func =
        func

    static member (||.)(pfunc1 : PartialFunction<_, _>, pfunc2 : PartialFunction<_, _>) =
        let isDefinedAt input =
            pfunc1.IsDefinedAt(input) || pfunc2.IsDefinedAt(input)
        let func input =
            assert(isDefinedAt input)
            if pfunc1.IsDefinedAt(input) then
                pfunc1.Func input
            else
                pfunc2.Func input
        PartialFunction(isDefinedAt, func)

    static member (&&.)(pfunc1 : PartialFunction<_, _>, pfunc2 : PartialFunction<_, _>) =
        let func =
            pfunc1.Func >> pfunc2.Func
        PartialFunction(pfunc1.IsDefinedAt, func)

    static member (|>.)(input, pfunc : PartialFunction<_, _>) =
        pfunc.[input]

[<AutoOpen>]
module AutoOpen =

    let (=>) isDefinedAt func =
        PartialFunction(isDefinedAt, func)

module Seq =

    let pchoose (pfunc : PartialFunction<_, _>) source =
        source
            |> Seq.choose (fun item ->
                if pfunc.IsDefinedAt(item) then
                    Some pfunc.[item]
                else
                    None)

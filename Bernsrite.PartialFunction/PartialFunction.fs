namespace Bernsrite.PartialFunction

/// A partial function.
type PartialFunction<'input, 'output>
    (
        isDefinedAt : 'input -> bool,
        func : 'input -> 'output
    ) =

    /// Is the partial function defined for the given input?
    member __.IsDefinedAt(input) =
        isDefinedAt input

    /// Safely evaluates the partial function for the given input.
    member __.Item(input) =
        if isDefinedAt input then
            Some (func input)
        else
            None

    /// Unsafe access to the underlying function.
    member private __.Func =
        func

    /// Combines two partial functions, like Scala's "orElse".
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

    /// Composes two partial functions, like Scala's "andThen".
    static member (&&.)(pfunc1 : PartialFunction<_, _>, pfunc2 : PartialFunction<_, _>) =
        let func =
            pfunc1.Func >> pfunc2.Func
        PartialFunction(pfunc1.IsDefinedAt, func)

    /// Pipes an input into a partial function.
    static member (|>.)(input, pfunc : PartialFunction<_, _>) =
        pfunc.[input]

[<AutoOpen>]
module AutoOpen =

    /// Syntactic shortcut for defining a partial function, like Scala's "case".
    let (=>) isDefinedAt func =
        PartialFunction(isDefinedAt, func)

module Seq =

    /// Applies the given partial function to each element of the given sequence,
    /// answering the sequence of results by skipping inputs for which the partial
    /// function is not defined. Like Scala's "collect" function.
    let choosep (pfunc : PartialFunction<_, _>) source =
        source
            |> Seq.choose (fun item -> pfunc.[item])

module List =

    /// Applies the given partial function to each element of the given list,
    /// answering the list of results by skipping inputs for which the partial
    /// function is not defined. Like Scala's "collect" function.
    let choosep (pfunc : PartialFunction<_, _>) source =
        source
            |> List.choose (fun item -> pfunc.[item])

module Array =

    /// Applies the given partial function to each element of the given array,
    /// answering the array of results by skipping inputs for which the partial
    /// function is not defined. Like Scala's "collect" function.
    let choosep (pfunc : PartialFunction<_, _>) source =
        source
            |> Array.choose (fun item -> pfunc.[item])

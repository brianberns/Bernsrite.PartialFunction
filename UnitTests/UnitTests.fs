namespace Bernsrite.PartialFunction

open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type UnitTest() =

    let equalLists expected actual =
        Assert.AreEqual(expected |> List.length, actual |> List.length)
        for (e, a) in Seq.zip expected actual do
            Assert.AreEqual(e, a)

    [<TestMethod>]
    member __.Collatz() =

        let positive = (fun x -> x > 0) => id
        let even = (fun x -> x % 2 = 0) => (fun x -> x / 2)
        let odd = (fun x -> x % 2 = 1) => (fun x -> 3 * x + 1)
        let collatz = positive &&. (even ||. odd)
        let rec loop x =
            [
                yield x
                match collatz.[x] with
                    | Some 1 -> yield 1
                    | Some x' -> yield! loop x'
                    | None -> failwith "Undefined"
            ]

        let expected = [12; 6; 3; 10; 5; 16; 8; 4; 2; 1]
        let actual = loop 12
        equalLists expected actual

    [<TestMethod>]
    member __.Collect() =
        let greaterThan20 = (fun x -> x > 20) => id
        let expected = [45; 25]
        let actual = [1; 45; 10; 25] |> List.choosep greaterThan20
        equalLists expected actual

    [<TestMethod>]
    member __.Divide() =
        let safeDivide num =
            (fun den -> den <> 0) => (fun den -> num / den)
        Assert.AreEqual(Some 4, (safeDivide 24).[6])
        Assert.AreEqual(None, (safeDivide 10).[0])

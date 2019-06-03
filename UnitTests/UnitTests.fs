namespace Bernsrite.PartialFunction

open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type UnitTest() =

    [<TestMethod>]
    member __.Collatz() =

        let positive = (fun x -> x > 0) => id
        let even = (fun x -> x % 2 = 0) => (fun x -> x / 2)
        let odd = (fun x -> x % 2 = 1) => (fun x -> 3 * x + 1)
        let collatz = positive &&. (even ||. odd)
        let rec loop x =
            [
                yield x
                let x' = collatz.[x]
                if x' = 1 then
                    yield x'
                else
                    yield! loop x'
            ]

        let expected = [12; 6; 3; 10; 5; 16; 8; 4; 2; 1]
        let actual = loop 12
        Assert.AreEqual(expected.Length, actual.Length)
        for (e, a) in Seq.zip expected actual do
            Assert.AreEqual(e, a)

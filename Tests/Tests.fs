open Lesson4
open NUnit.Framework
open FsUnit

let fastCalculation X = Calculation X 100

[<Test>]
let ``test of norm strategy 1``() =
    fastCalculation (App(Combinators.I, Var('z'))) |> should equal (Some(Var('z')))

[<Test>]
let ``test of norm strategy 2``() =
    fastCalculation (App(Combinators.xToXxx, Combinators.xToXxx)) |> should equal null

[<Test>]
let ``test of norm strategy 3``() =
    fastCalculation (App(Combinators.K, Combinators.I)) |> should equal (Some(Combinators.K'))

[<Test>]
let ``test of norm strategy 4``() =
    fastCalculation (App(App(Combinators.S, Combinators.K), Combinators.K)) 
        |> should equal (Some(Labstraction('z', Var('z'))))

[<EntryPoint>]
let main argv =
    ``test of norm strategy 1``()
    ``test of norm strategy 2``()
    ``test of norm strategy 3``()
    ``test of norm strategy 4``()
    0 

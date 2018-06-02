open System

type Term =
    | Var of char
    | App of T1 : Term * T2 : Term
    | Labstraction of v : char * T : Term

let Calculation (t : Term) maxNumOfCalculation =
    let rec FV T =
        match T with 
        | Var(v) -> [v]
        | App(T1, T2) -> (FV T1)@(FV T2)
        | Labstraction(v, T1) -> List.except (FV T1) [v]

    let GetZ T1 T2 =
        let allChar = ['a'..'z']
        let freeVar = (List.except (FV T2) (List.except (FV T1) allChar))
        if (List.isEmpty freeVar) then
            raise (Exception("There are no free variables!"))
        List.head freeVar

    let rec Change (term1 : Term) value (term2 : Term) =
        match term1 with
        | Var(v) when v = value -> term2
        | Var(v) -> term1
        | App(T1, T2) -> App(Change T1 value term2, Change T2 value term2)
        | Labstraction(v, _) when (value = v) && (term2 <> Var(v)) -> term1    
        | Labstraction(v, T) when (not (List.contains v (FV term2))) || (not (List.contains value (FV T))) -> 
            Labstraction(v, Change T value term2)
        | Labstraction(v, T) -> 
            let newVar = GetZ term2 T
            Labstraction(newVar, Change (Change T v (Var(newVar))) value term2)

    let rec NormStrat t : Term =
        match t with
        | Var(v) -> t
        | Labstraction(v, T) -> Labstraction(v, NormStrat T)
        | App(Labstraction(v, T1), T2) -> Change T1 v T2
        | App(T1, T2) -> App(NormStrat T1, NormStrat T2)

    let rec recCalculation t numOfCalc =
        if (numOfCalc = maxNumOfCalculation)
        then None
        elif (t = (NormStrat t))
        then Some(t)
        else recCalculation (NormStrat t) (numOfCalc + 1)
    
    recCalculation t 0

module Combinators =
    let S = (Labstraction('x', Labstraction('y', Labstraction('z', App(App(Var('x'), Var('z')), App(Var('y'), Var('z')))))))
    let K = (Labstraction('x', Labstraction('y', Var('x'))))
    let K' = (Labstraction('y', Labstraction('x', Var('x'))))
    let I = (Labstraction('x', Var('x')))
    let xToXxx = (Labstraction('x', App(App(Var('x'), Var('x')), Var('x'))))

[<EntryPoint>]
let main argv =
    0 

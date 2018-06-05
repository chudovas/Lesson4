open System

type Term =
    | Var of char
    | App of T1 : Term * T2 : Term
    | LambdaAbstraction of v : char * T : Term

let calculation (t : Term) maxNumOfCalculation =
    let rec fV T =
        match T with 
        | Var(v) -> [v]
        | App(T1, T2) -> (fV T1)@(fV T2)
        | LambdaAbstraction(v, T1) -> List.except (fV T1) [v]

    let getNewVar T1 T2 =
        let allChar = ['a'..'z']
        let freeVar = (List.except (fV T2) (List.except (fV T1) allChar))
        if (List.isEmpty freeVar) then
            raise (Exception("There are no free variables!"))
        List.head freeVar

    let rec change (term1 : Term) value (term2 : Term) =
        match term1 with
        | Var(v) when v = value -> term2
        | Var(v) -> term1
        | App(T1, T2) -> App(change T1 value term2, change T2 value term2)
        | LambdaAbstraction(v, _) when (value = v) && (term2 <> Var(v)) -> term1    
        | LambdaAbstraction(v, T) when (not (List.contains v (fV term2))) || (not (List.contains value (fV T))) -> 
            LambdaAbstraction(v, change T value term2)
        | LambdaAbstraction(v, T) -> 
            let newVar = getNewVar term2 T
            LambdaAbstraction(newVar, change (change T v (Var(newVar))) value term2)

    let rec normStrat t : Term =
        match t with
        | Var(v) -> t
        | LambdaAbstraction(v, T) -> LambdaAbstraction(v, normStrat T)
        | App(LambdaAbstraction(v, T1), T2) -> change T1 v T2
        | App(T1, T2) -> App(normStrat T1, normStrat T2)

    let rec recCalculation t numOfCalc =
        if (numOfCalc = maxNumOfCalculation)
        then None
        elif (t = (normStrat t))
        then Some(t)
        else recCalculation (normStrat t) (numOfCalc + 1)
    
    recCalculation t 0

module Combinators =
    let S = (LambdaAbstraction('x', LambdaAbstraction('y', LambdaAbstraction('z', App(App(Var('x'), Var('z')), App(Var('y'), Var('z')))))))
    let K = (LambdaAbstraction('x', LambdaAbstraction('y', Var('x'))))
    let K' = (LambdaAbstraction('y', LambdaAbstraction('x', Var('x'))))
    let I = (LambdaAbstraction('x', Var('x')))
    let xToXxx = (LambdaAbstraction('x', App(App(Var('x'), Var('x')), Var('x'))))

[<EntryPoint>]
let main argv =
    0 

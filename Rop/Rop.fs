[<AutoOpen>]
module Rop
    
type Result<'TSuccess, 'TFailure> = 
    | Success of 'TSuccess
    | Failure of 'TFailure
    
module Result = 
    let retn = Success
    
    let either successFunc failureFunc = 
        function 
        | Success s -> successFunc s
        | Failure f -> failureFunc f
    
    let bind f = either f Failure
    
    let map f = either (f >> Success) Failure
    
    let getOrDefault f = either id f
    
    let get xResult = getOrDefault (fun _ -> failwith "There is no value") xResult
    
    let biMap successFunc failureFunc = either (successFunc >> Success) (failureFunc >> Failure)
    
    let tee f x = 
        f x |> ignore
        x
    
    let tryCatch f exnHandler x = 
        try 
            f x |> Success
        with ex -> exnHandler ex |> Failure
   
module Async = 

    let bind f xAsync = async.Bind(xAsync, f)
    
    let retn = async.Return 
    
    let map f xAsync = async.Bind(xAsync, fun x -> retn <| f x)

module AsyncResult = 

    let retn x = Async.retn <| Result.retn x
    
    let either successFunc failureFunc xAsyncResult = 
        async {
            let! xResult = xAsyncResult
            match xResult with
            | Success s -> return! successFunc s
            | Failure f -> return! failureFunc f
        }

    let bind f = either f (Failure >> Async.retn)
    let inline (>>=) x f = bind f x
    
    let map f = Async.map <| Result.map f
    let inline (<!>) x f = map f x
    
    let getOrDefault f = Async.map <| Result.getOrDefault f
    
    let biMap successFunc failFunc = Async.map <| Result.biMap successFunc failFunc
    
    let tryCatch asyncF exnHandler xAsync = 
        async { 
            try 
                let! x = xAsync
                let! res = asyncF x
                return Success res
            with ex -> return Failure <| exnHandler ex 
        }

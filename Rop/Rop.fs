[<AutoOpen>]
module Rop
    
type Result<'TSuccess, 'TFailure> = 
    | Success of 'TSuccess
    | Failure of 'TFailure

module Result = 
    
    let retn = Success
    let fail = Failure

    let either successFunc failureFunc = 
        function 
        | Success s -> successFunc s
        | Failure f -> failureFunc f
    
    let bind f = either f Failure
    
    let map f = either (f >> Success) Failure
    
    let getOrDefault f = either id f
    
    let biMap successFunc failFunc = either (successFunc >> Success) (failFunc >> Failure)
    
    let mapFailure f x = biMap id f x
    
    let tee f x = 
        f x |> ignore
        x
    
    let tryCatch f exnHandler x = 
        try 
            f x |> Success
        with ex -> exnHandler ex |> Failure
   
module Async = 

    let bind f x = async.Bind(x, f)
    
    let retn = async.Return 
    
    let map f x = async.Bind(x, fun x -> retn <| f x)

module AsyncResult = 

    let retn x = Async.retn <| Result.retn x
    let fail x = Async.retn <| Result.fail x

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
    
    let mapFailure f x = biMap id f x

    let tryCatch f exnHandler xAsync = 
        async { 
            try 
                let! x = xAsync
                let! res = f x
                return Success res
            with ex -> return Failure <| exnHandler ex 
        }

    let fromAsync xAsync =  Async.map Result.retn xAsync

    type AsyncResultBuilder() = 
        member __.Return value = retn value
        
        member __.ReturnFrom asyncResult = asyncResult
        
        member __.Zero () = retn ()
        
        member __.Delay generator = generator >> fromAsync
            
        member __.Bind(asyncResult, binder) = bind binder asyncResult
            
        member __.Bind(result, binder) = result |> Async.retn |> bind binder
        
        member __.TryWith(asyncResult, catchHandler) = retn <| async.TryWith(asyncResult, catchHandler)

        member __.TryFinally(asyncResult, compensation) = retn <| async.TryFinally(asyncResult, compensation)
        
        member __.Using(resource, binder) = retn <| async.Using(resource, binder)
    
    let asyncResult = AsyncResultBuilder()
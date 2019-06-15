// http://fssnip.net/7TM/title/TypeSafe-effect-builder
module Fssnip.Effects

type Eff<'Ctx, 'T> = ('Ctx -> 'T -> unit) -> 'Ctx -> unit

type EffBuilder() =
    member __.Return x : Eff<'Ctx,'T> = fun k c -> k c x
    member __.Bind(f : Eff<'Ctx, 'T>, g : 'T -> Eff<'Ctx, 'S>) : Eff<'Ctx, 'S> =
        fun k c -> f (fun c t -> g t k c) c

    member __.Zero() : Eff<'Ctx, unit> = __.Return()
    member __.ReturnFrom (x : Eff<'Ctx, 'T>) = x

let eff = new EffBuilder()

let getCtx<'Ctx> () : Eff<'Ctx, 'Ctx> = fun k c -> k c c

let run handler (eff : Eff<'Ctx, 'T>) =
    let cell = ref Unchecked.defaultof<'T>
    eff (fun _ t -> cell := t) handler
    !cell

module Logger =

    type ILogger = 
        abstract Log : string -> unit

    let log (msg : string) = eff {
        let! logger = getCtx<#ILogger> ()
        logger.Log msg
    }

    let logf fmt = Printf.ksprintf log fmt

module State =

    type IState<'T> =
        abstract Get : unit -> 'T
        abstract Set : 'T -> unit

    let get () = eff {
        let! state = getCtx<#IState<'T>>()
        return state.Get()
    }

    let set (t : 'T) = eff {
        let! state = getCtx<#IState<'T>>()
        state.Set t
    }


module DateTime =

    type IDateTime =
        abstract Now : System.DateTime

    let now() = eff {
        let! dt = getCtx<#IDateTime>()
        return dt.Now
    }

// type signature of this computation reveals effect dependencies of the workflow
let combinedEffects() = eff {
    ()
    let! date = DateTime.now()
    do! Logger.logf "Current time is: %O" date
    do! Logger.log "Reading the variable"
    let! x = State.get()
    do! Logger.log "Incrementing the variable"
    do! State.set (x + 1)
    do! Logger.log "Reading the variable again"
    return! State.get()
}

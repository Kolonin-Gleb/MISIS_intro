module Forms

open Telefunc.Core
open Telefunc.State
open Telefunc.Infrastructure
open Telegram.Bot
open Telegram.Bot.Types
open System.Threading.Tasks

type IBot = ITelegramBotClient

let send (msg: Task<Message>) =
    msg :> Task |> Async.AwaitTask |> Async.StartImmediate

let sendSync (msg: Task<Message>) =
    msg |> Async.AwaitTask |> Async.RunSynchronously

(*----------------------
    Forms ans states
----------------------*)

type SendingStatus =
    | Sent
    | NothingToSend
    with
        static member isSent status = status = Sent
type AskBranch =
    | Asking of Ask
    | Answering of (IBot * int64 -> Task<Message>)
    | HiddenExecuting of (IBot * int64 -> unit)
    | Skipping
and Ask = {
    Question: IBot * int64 -> Task<Message>
    AskedCallback: IBot * int64 * Message -> unit
    Parser: IBot -> Update -> bool
    HasAnswer: IBot -> int64 -> bool
    mutable AskBranch: AskBranch
} with
    static member makeCurried question questionAskedCallback parser hasAnswer elseCase =
        { Question = question
        ; AskedCallback = questionAskedCallback
        ; Parser = parser
        ; HasAnswer = hasAnswer
        ; AskBranch = elseCase
        }
    static member make (question, questionAskedCallback, parser, hasAnswer, elseCase) =
        { Question = question
        ; AskedCallback = questionAskedCallback
        ; Parser = parser
        ; HasAnswer = hasAnswer
        ; AskBranch = elseCase
        }
    static member checkStateOption<'a, 't when 't :> TelefuncState>
        (stateParam: 't -> 'a option)
        (bot: IBot)
        (id: int64)
        =
        maybe {
            let! state = bot.State.get id
            let state = state :?> 't
            return! stateParam state
        } |> Option.isSome
    static member checkByState<'t when 't :> TelefuncState>
        (stateParam: 't -> bool)
        (bot: IBot)
        (id: int64)
        =
        maybe {
            let! state = bot.State.get id
            let state = state :?> 't
            return stateParam state
        } |> Option.defaultValue false
    static member checkByCondition
        (cond: IBot * int64 -> bool)
        (bot: IBot)
        (id: int64)
        = cond(bot, id)
    static member emptyAskCallback (_: IBot * int64 * Message) = ()
    static member statyAskCallback<'t when 't :> TelefuncState>
        (fn: 't * Message -> unit)
        (bot: IBot, id: int64, msg: Message)
        =
        maybe {
            let! state = bot.State.get id
            let state = state :?> 't
            return fn (state, msg)
        } |> ignore

type Form (asks: Ask list) =
    member val Asks = asks with get, set
    member val private ActiveAsk = asks |> Seq.tryItem 0 with get, set
    member this.Parse (bot: IBot) (upd: Update) =
        match this.ActiveAsk with
        | Some ask ->
            let parsed = ask.Parser bot upd
            maybe {
                let! id = getId upd
                return if parsed then this.Send bot id
            } |> ignore
            parsed
        | None -> false
    member this.RecursiveAsking (bot: IBot, id: int64, ask: Ask) =
        match ask.HasAnswer bot id with
        | true ->
            match ask.AskBranch with
            | Asking branchAsk ->
                this.RecursiveAsking
                    ( bot = bot
                    , id = id
                    , ask = branchAsk
                    )
            | Answering answer ->
                ask.AskBranch <- Skipping
                answer(bot, id) |> send
                Sent
            | HiddenExecuting execFunc ->
                ask.AskBranch <- Skipping
                execFunc(bot, id)
                Sent
            | Skipping -> NothingToSend
        | _ ->
            this.ActiveAsk <- Some ask
            let msg = ask.Question(bot, id) |> sendSync
            ask.AskedCallback(bot, id, msg)
            Sent
    member this.Send (bot: IBot) id =
        this.Asks
        |> Seq.tryFind
            (fun ask ->
                SendingStatus.isSent
                <| this.RecursiveAsking
                    ( bot = bot
                    , id = id
                    , ask = ask
                    ))
        |> ignore

type IFieldAsk =
    abstract member MsgFn: IBot * int64 -> Task<Message>
    abstract member HasValue: unit -> bool
    abstract member TryParse: IBot * Update -> bool

type FieldAsk<'T>(value, msgFn, parseFn) =
    member val Value: 'T option = value with get, set
    member val ParseFn: IBot * Update -> 'T option = parseFn
    interface IFieldAsk with
        member _.MsgFn(bot, chatId) = msgFn(bot, chatId)
        member this.HasValue() = this.Value.IsSome
        member this.TryParse(bot, upd) =
            let parseResult = this.ParseFn(bot, upd)
            if parseResult.IsSome then
                this.Value <- parseResult
                true
            else
                false
type FieldAskValueless(msgFn, parseFn) =
    let mutable answered = false
    member val ParseFn: IBot * Update -> bool = parseFn
    interface IFieldAsk with
        member _.MsgFn(bot, chatId) = msgFn(bot, chatId)
        member _.HasValue() = answered
        member this.TryParse(bot, upd) =
            answered <- this.ParseFn(bot, upd)
            answered

type FilterAsk(msgFn, parseFn) =
    member _.MsgFn: IBot * int64 -> Task<Message> = msgFn
    member _.Parse: IBot * Update -> unit = parseFn

type FormType =
    abstract member AsksSequence: IFieldAsk list
    abstract member Send: IBot * int64 -> SendingStatus
    abstract member Parse: IBot * Update -> bool
    abstract member Finished: bool

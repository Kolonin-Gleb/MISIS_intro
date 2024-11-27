open System
open System.Collections.Generic
open Telegram.Bot
open Telegram.Bot.Types
open Telegram.Bot.Types.Enums
open Telegram.Bot.Types.ReplyMarkups
open Telefunc.Core
open Telefunc.State
open Forms
open Telefunc.Infrastructure
open Telegram.Bot.Exceptions
open System.Threading.Tasks

// Заглушки для хранения данных
let users = Dictionary<int64, (string * string * string)>() // chatId -> (роль, email, группа)
let grades = Dictionary<string, Dictionary<string, int list>>() // группа -> (ученик -> оценки)

type IBot = ITelegramBotClient

exception ApiRequestException_fs of ApiRequestException

let await task = task |> Async.AwaitTask |> Async.RunSynchronously

let handlePollingErrorAsync _ ex _ =
    task {
        printfn "%A"
        <| match ex with
           | ApiRequestException_fs apiReqException ->
               $"Telegram API Error:\n[{apiReqException.ErrorCode}]\n{apiReqException.Message}"
           | _ -> ex.ToString()
    }
    :> Task

type User = {
    Email: string
    Role: string
    Grades: Dictionary<string, List<int>>
}

type Group = {
    Name: string
    mutable Users: User list
}

type FormState =
    | Auth
    | AddToGroup
    | CreateGroup
    | AddGrade

let formStatesDb = Dictionary<int64, FormState>()

let usersDb = Dictionary<int64, User>()

// ================================== Заполнение БД данными для демонстрации

// Ученики students
let glebGrades = Dictionary<string, int list>()
glebGrades.Add("math", [5; 4; 5; 3; 5; 5])
glebGrades.Add("F#", [5; 5; 5; 5; 5; 4; 5])

// Пользователи добавляются с использованием своего id в телеграмме
usersDb.Add
    ( 6394408740L
    , { Email = "gleb@mail.ru"
        Role = "Student"
        Grades = glebGrades
      }
    )

// Учителя teachers
// Учитебя добавляются с использованием своего id в телеграмме
usersDb.Add
    ( 534799233L
    , { Email = "marat@mail.ru"
        Role = "Teacher"
        Grades = Dictionary<string, List<int>>()
      }
    )

let groupsDb: List<Group> = List<Group>()

let getUser id =
    try usersDb.Item id |> Some
    with | _ -> None

let start (bot: IBot) (upd: Update) =
    maybe {
        let! id = getId upd
        return bot.SendTextMessageAsync(id, "Привет, Путник!") |> await
    } |> Option.isSome

let help (bot: IBot) (upd: Update) = true

let getGrades (bot: IBot) (upd: Update) =
    maybe {
        let! id = getId upd
        let user = getUser id
        let mutable response = ""
        do
            match user with
            | Some loggedInUser ->
                for grade in loggedInUser.Grades do
                    response <- response + $"\n\n{grade.Key}:\n" + (String.Join(", ", grade.Value))
            | None ->
                response <- "Сначала пройдите авторизацию по команде /auth"
        return bot.SendTextMessageAsync
            ( id
            , if response.Length > 0
                then response
                else "У тебя пока нет оценок."
            ) |> await
    } |> Option.isSome

let parseEmail (bot: IBot) (upd: Update) =
    maybe {
        let! id = getId upd
        let! msg = if upd.Type = UpdateType.Message then Some upd.Message else None
        do
            usersDb.Add
                ( id
                , { Email = msg.Text
                    Role = "Student"
                    Grades = Dictionary<string, List<int>>()
                  }
                )
        return ()
    } |> Option.isSome

let authForm =
    Form [
        Ask.make
            ( question = (fun (bot: IBot, id: int64) -> bot.SendTextMessageAsync(id, "Введите свою почту"))
            , questionAskedCallback = (
                fun (_, id, _) ->
                    if formStatesDb.ContainsKey id
                    then formStatesDb[id] <- Auth
                    else formStatesDb.Add(id, Auth)
                )
            , parser = parseEmail
            , hasAnswer = (fun _ -> fun id -> getUser id |> Option.isSome)
            , elseCase = Skipping
            )
    ]

let parseCreatingGroup (_: IBot) (upd: Update) =
    maybe {
        let! id = getId upd
        let! user = getUser id
        let! msg = if upd.Type = UpdateType.Message then Some upd.Message else None
        do
            if user.Role = "Teacher"
            then groupsDb.Add { Name = msg.Text; Users = [] }
        return ()
    } |> Option.isSome

let creatingGroupForm =
    Form [
        Ask.make
            ( question = (fun (bot: IBot, id: int64) -> bot.SendTextMessageAsync(id, "Введите название группы"))
            , questionAskedCallback = (
                fun (_, id, _) ->
                    if formStatesDb.ContainsKey id
                    then formStatesDb[id] <- CreateGroup
                    else formStatesDb.Add(id, CreateGroup)
                )
            , parser = parseCreatingGroup
            , hasAnswer = (fun _ -> fun _ -> false)
            , elseCase = Skipping
            )
    ]

let parseAddToGroup (_: IBot) (upd: Update) =
    maybe {
        let! id = getId upd
        let! user = getUser id
        let! msg = if upd.Type = UpdateType.Message then Some upd.Message else None
        do
            if user.Role = "Teacher"
            then
                for line in msg.Text.Split '\n' do
                    if line.Contains ' '
                    then
                        let groupAndEmail = line.Split ' '
                        let (groupName, email) = (groupAndEmail[0], groupAndEmail[1])
                        maybe {
                            let! group = groupsDb |> Seq.tryFind(fun x -> x.Name = groupName)
                            let! userToAdd = usersDb |> Seq.tryFind (fun x -> x.Value.Email = email)
                            do  group.Users <- group.Users @ [ userToAdd.Value ]
                            return ()
                        } |> ignore
        return ()
    } |> Option.isSome

let addingToGroupForm =
    Form [
        Ask.make
            ( question = (fun (bot: IBot, id: int64) -> bot.SendTextMessageAsync(id, "Введите пользователя"))
            , questionAskedCallback = (
                fun (_, id, _) ->
                    if formStatesDb.ContainsKey id
                    then formStatesDb[id] <- AddToGroup
                    else formStatesDb.Add(id, AddToGroup)
                )
            , parser = parseAddToGroup
            , hasAnswer = (fun _ -> fun _ -> false)
            , elseCase = Skipping
            )
    ]

let parseAddGrade (_: IBot) (upd: Update) =
    maybe {
        let! id = getId upd
        let! user = getUser id
        let! msg = if upd.Type = UpdateType.Message then Some upd.Message else None
        do
            if user.Role = "Teacher"
            then
                for line in msg.Text.Split '\n' do
                    if line.Contains ' '
                    then
                        let groupAndEmail = line.Split ' '
                        let (email, subject, grade) = (groupAndEmail[0], groupAndEmail[1], groupAndEmail[2])
                        maybe {
                            let! userToGrade = usersDb |> Seq.tryFind (fun x -> x.Value.Email = email)
                            let success, grade = Int32.TryParse grade
                            do
                                if success then
                                    if userToGrade.Value.Grades.ContainsKey subject
                                    then userToGrade.Value.Grades[subject].Add grade
                                    else
                                        let grades = List()
                                        grades.Add grade
                                        userToGrade.Value.Grades.Add(subject, grades)
                            return ()
                        } |> ignore
        return ()
    } |> Option.isSome

let addingGradeForm =
    Form [
        Ask.make
            ( question = (fun (bot: IBot, id: int64) -> bot.SendTextMessageAsync(id, "Введите оценку"))
            , questionAskedCallback = (
                fun (_, id, _) ->
                    if formStatesDb.ContainsKey id
                    then formStatesDb[id] <- AddGrade
                    else formStatesDb.Add(id, AddGrade)
                )
            , parser = parseAddGrade
            , hasAnswer = (fun _ -> fun _ -> false)
            , elseCase = Skipping
            )
    ]

let auth (bot: IBot) (upd: Update) =
    maybe {
        let! id = getId upd
        return authForm.Send bot id
    } |> Option.isSome

let createGroup (bot: IBot) (upd: Update) =
    maybe {
        let! id = getId upd
        return creatingGroupForm.Send bot id
    } |> Option.isSome

let addToGroup (bot: IBot) (upd: Update) =
    maybe {
        let! id = getId upd
        return addingToGroupForm.Send bot id
    } |> Option.isSome

let addingGrade (bot: IBot) (upd: Update) =
    maybe {
        let! id = getId upd
        return addingGradeForm.Send bot id
    } |> Option.isSome

let nothingMatched (bot: IBot) (upd: Update) =
    maybe {
        let! id = getId upd
        return bot.SendTextMessageAsync(id, "Вы что-то не то ввели...") |> await
    } |> ignore
    true

let parseForm (bot: IBot) (upd: Update) =
    maybe {
        let! id = getId upd
        let! state =
            if formStatesDb.ContainsKey id
            then formStatesDb.Item id |> Some
            else None
        let parser =
            match state with
            | Auth -> authForm.Parse
            | CreateGroup -> creatingGroupForm.Parse
            | AddGrade -> addingGradeForm.Parse
            | AddToGroup -> addingToGroupForm.Parse
        return parser bot upd
    } |> Option.defaultValue false


// НАЧАЛО ПРОГРАММЫ
[<EntryPoint>]
let main (args: string array) =
    let token = File.ReadAllText("token.txt").Trim()
    runBot
        token // Использование телеграм токена бота считанного из файла
        [ Filter.isCommand
            [ Filter.command "start"  [ start ]
              Filter.command "help"   [ help ]
              Filter.command "grades" [ getGrades ]
              Filter.command "auth"   [ auth ]
              Filter.command "create" [ createGroup ]
              Filter.command "add"    [ addToGroup ]
              Filter.command "grade"  [ addingGrade ]
            ];
          Filter.isMessage
            [ parseForm
            ]
          nothingMatched
        ]
        handlePollingErrorAsync

    1

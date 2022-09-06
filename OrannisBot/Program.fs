// Learn more about F# at http://fsharp.org

open System.Runtime.CompilerServices
open System.Threading.Tasks
open Discord
open Discord.WebSocket
open Polls

[<assembly: InternalsVisibleTo("OrannisBot.Tests")>]
do()

module OrannisBot =

    let discord = new DiscordSocketClient()

    [<EntryPoint>]
    let main argv =
        let token = Config.readToken()
        task {
            discord.add_Ready(createPollResponder(discord))
            do! discord.LoginAsync(TokenType.Bot, token)
            do! discord.StartAsync()
            do! Task.Delay(-1)
        } |> Async.AwaitTask |> Async.RunSynchronously
        0

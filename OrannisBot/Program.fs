// Learn more about F# at http://fsharp.org

open DSharpPlus
open DSharpPlus.CommandsNext
open DSharpPlus.CommandsNext.Attributes
open DSharpPlus.Entities
open DSharpPlus.EventArgs
open Polls
open System.Threading.Tasks
open System.Runtime.CompilerServices

[<assembly: InternalsVisibleTo("OrannisBot.Tests")>]
do()

module OrannisBot =

    let botConfig = Config.readConfig()

    let discordConfig = DiscordConfiguration()
    discordConfig.set_AutoReconnect true
    discordConfig.set_LogLevel LogLevel.Debug
    discordConfig.set_Token botConfig.DiscordToken
    discordConfig.set_TokenType TokenType.Bot

    let discord = new DiscordClient(discordConfig)

    let pollOptions =
        let emojiName n = sprintf ":regional_indicator_%c:" (char (int 'a' + n))
        List.init 26 (fun n ->
            let name = emojiName n
            DiscordEmoji.FromName(discord, name) |> DiscordBackedEmoji :> Emoji)

    type BotCommands() =

        member private self.Poll(ctx: CommandContext) : Async<unit> = async {
            let originalMessage = ctx.Message.Content
            let spaceIndex = originalMessage.IndexOf(' ')

            if spaceIndex < 0 || spaceIndex + 1 = originalMessage.Length then
                ctx.Message.RespondAsync("Please provide a poll") |> Async.AwaitTask |> Async.Ignore |> ignore
            else
                let poll = parseFromInput (originalMessage.Substring(spaceIndex + 1))
                let formattedEmbed = formatPollEmbed poll pollOptions

                let! message = ctx.RespondAsync(embed = formattedEmbed) |> Async.AwaitTask

                for emoji in pollOptions |> List.take poll.Options.Length do
                    do! message.CreateReactionAsync (emoji.DiscordEmoji) |> Async.AwaitTask |> Async.Ignore
        }

        [<Command("poll"); Description("Creates a poll. Usage: $poll \"Title\" Option1 \"Option 2\"")>]
        member public self.PollAsync(ctx: CommandContext) : Task =
            self.Poll(ctx) |> Async.StartAsTask :> Task

    let commandsConfig = CommandsNextConfiguration()
    commandsConfig.set_StringPrefix "$"
    commandsConfig.set_SelfBot false

    let commands = discord.UseCommandsNext(commandsConfig)
    commands.RegisterCommands<BotCommands>()

    let getSelectedPollOption (options: PollOption list) (emojis: DiscordEmoji list) (emoji: DiscordEmoji) : PollOption =
        let rec search (options: PollOption list) (emojis: DiscordEmoji list) =
            match emoji with
            | _ when emojis = [] -> {Option = ""; Voters = []}
            | _ when emojis.Head = emoji -> options.Head
            | _ -> search options.Tail emojis.Tail

        search options emojis

    let messageReactionAdded (event: MessageReactionAddEventArgs): Async<unit> = async { 
        if event.User.IsBot || not event.Message.Author.IsCurrent || event.Message.Embeds.Count <> 1 then
            ()
        else
            let! reparsedPoll = parseFromMessage event.Message pollOptions
            do! event.Message.ModifyAsync(embed = Optional<DiscordEmbed>((formatPollEmbed reparsedPoll pollOptions))) |> Async.AwaitTask |> Async.Ignore
    }

    let messageReactionAddedAsync (event: MessageReactionAddEventArgs) : Task = messageReactionAdded event |> Async.StartAsTask :> Task

    let messageReactionRemoved (event: MessageReactionRemoveEventArgs) : Async<unit> = async {
        if event.User.IsBot || not event.Message.Author.IsCurrent || event.Message.Embeds.Count <> 1 then
            ()
        else
            let! reparsedPoll = parseFromMessage event.Message pollOptions
            do! event.Message.ModifyAsync(embed = Optional<DiscordEmbed>((formatPollEmbed reparsedPoll pollOptions))) |> Async.AwaitTask |> Async.Ignore
    }

    let messageReactionRemovedAsync (event: MessageReactionRemoveEventArgs): Task = messageReactionRemoved event |> Async.StartAsTask :> Task

    discord.add_MessageReactionAdded(AsyncEventHandler<MessageReactionAddEventArgs>(messageReactionAddedAsync))
    discord.add_MessageReactionRemoved(AsyncEventHandler<MessageReactionRemoveEventArgs>(messageReactionRemovedAsync))

    [<EntryPoint>]
    let main argv =
        discord.ConnectAsync() |> Async.AwaitTask |> Async.RunSynchronously |> ignore
        Task.Delay(-1) |> Async.AwaitTask |> Async.RunSynchronously
        0

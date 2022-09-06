module Polls

open System
open System.Linq
open System.Threading.Tasks
open Discord
open Discord.WebSocket
open System.Text.RegularExpressions

    let parseFromInput (message: string) =
        let escapeChar (chars : char list) : (string * char list) =
            match chars with
            | [] -> ("", [])
            | '\\' :: tail -> ("\\\\", tail)
            | head :: tail -> (head.ToString(), tail)

        let rec doParse (chars: char list) : string list =
            match chars with
            | [] -> []
            | ' ' :: tail -> doParse tail
            | '"' :: tail -> let (nextWord, remainder) = parseQuote tail ""
                             nextWord :: (doParse remainder)
            | _ -> let (nextWord, remainder) = parseWord chars ""
                   nextWord :: (doParse remainder)
                         

        and parseWord (chars: char list) (current : string) : (string * char list) =
            match chars with
            | [] -> (current, chars)
            | '\\' :: tail -> let (escaped, remainder) = escapeChar tail
                              parseWord remainder (current + escaped)
            | '"' :: _ -> (current, chars)
            | ' ' :: tail -> (current, tail)
            | head :: tail -> parseWord tail (current + head.ToString())

        and parseQuote (chars : char list) (current : string) : (string * char list) =
            match chars with
            | [] -> (current, chars)
            | '\\' :: tail -> let (escaped, remainder) = escapeChar tail
                              parseQuote remainder (current + escaped)
            | '"' :: tail -> (current, tail)
            | head :: tail -> parseQuote tail (current + head.ToString())

        doParse (message.ToCharArray() |> List.ofArray)

    let buildField (number: int) (option: string) : EmbedFieldBuilder =
        let builder = new EmbedFieldBuilder()
        builder.Name <- $"{number + 1} {option}"
        builder.Value <- "No votes yet"
        builder
        

    let createPollResponder (discord : DiscordSocketClient) : Unit -> Task  =
        let pollButtonCustomIdPrefix = "poll_button"
        let commandResponder(command : SocketSlashCommand) : Task =
            if command.Data.Name <> "poll" then
                Task.CompletedTask
            else
                task {
                    let embedBuilder = new EmbedBuilder()
                    embedBuilder.Title <- command.Data.Options.First().Value :?> string
                    let optionsString = command.Data.Options.ElementAt(1).Value :?> string
                    let options = parseFromInput optionsString |> List.mapi buildField

                    for option in options do
                        embedBuilder.AddField option |> ignore

                    let componentBuilder = new ComponentBuilder()

                    for i = 1 to options.Length do
                        componentBuilder.WithButton(label = i.ToString(), customId = $"{pollButtonCustomIdPrefix}{i}") |> ignore

                    do! command.RespondAsync(embed = embedBuilder.Build(), components = componentBuilder.Build())
                }

        let pollResponseRegex = new Regex($"""{pollButtonCustomIdPrefix}(\d+)""", RegexOptions.Compiled)

        let buttonResponder(messageComponent: SocketMessageComponent) : Task =
            let customIdMatch = pollResponseRegex.Match(messageComponent.Data.CustomId)
            if not customIdMatch.Success then
                Task.CompletedTask
            else
                task {
                    let pollOption = customIdMatch.Groups[1].Value |> Int32.Parse
                    let optionEmbed = messageComponent.Message.Embeds.Single().ToEmbedBuilder()
                    let votedOption = optionEmbed.Fields[pollOption - 1]
                    let currentVotesString = votedOption.Value :?> string
                    let currentVotes = match currentVotesString with
                                       | "No votes yet" -> []
                                       | _ -> currentVotesString.Split(',', (StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries)) |> List.ofArray

                    let finalVotes = if currentVotes.Contains messageComponent.User.Mention then
                                        currentVotes |> List.filter (fun el -> el <> messageComponent.User.Mention)
                                     else
                                        currentVotes |> List.append [messageComponent.User.Mention]

                    votedOption.Value <- if finalVotes.IsEmpty then "No votes yet" else (finalVotes |> String.concat ",")

                    do! messageComponent.UpdateAsync(fun(context) -> context.Embed <- optionEmbed.Build())
                }

        fun () ->
            task {
                let builder = new SlashCommandBuilder()
                builder.WithName "poll" |> ignore
                builder.WithDescription "Creates a poll" |> ignore
                builder.AddOption("title", ApplicationCommandOptionType.String, "The title of the poll", isRequired = true, choices = Array.Empty()) |> ignore
                builder.AddOption("options", ApplicationCommandOptionType.String, "The options for the poll. ", isRequired = true, choices = Array.Empty()) |> ignore
                discord.add_SlashCommandExecuted commandResponder
                discord.add_ButtonExecuted buttonResponder
                let! result =  discord.CreateGlobalApplicationCommandAsync(builder.Build())
                ()
            }

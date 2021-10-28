module Polls

open Util
open System.Threading.Tasks
open Remora.Results
open Remora.Commands.Attributes
open System.ComponentModel
open Remora.Discord.Core
open Remora.Discord.API.Abstractions.Rest
open Remora.Discord.API.Abstractions.Objects
open Remora.Discord.API.Objects
open Remora.Discord.Commands.Contexts
open System.Collections.Generic
open System.Threading
open Remora.Discord.Gateway.Responders
open Remora.Discord.API.Abstractions.Gateway.Events

    [<Struct>]
    type PollOption = { 
        Option: string
        Voters: string list
    }

    [<Struct>]
    type Poll = {
        Title: string
        Options: PollOption list
    }

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

        let parsedElements = doParse (message.ToCharArray() |> List.ofArray)

        match parsedElements with
        | [] -> {Title = ""; Options = []}
        | _ -> {Title = parsedElements.Head; Options = (parsedElements.Tail |> List.map(fun o -> {Option = o; Voters = []}))}

    type PollCommand(commandContext : ICommandContext, interactionService : IDiscordRestInteractionAPI) =

        interface IResponder<IInteractionCreate> with
            member self.RespondAsync(interactionCreate : IInteractionCreate, ct : CancellationToken) : Task<Result> =
                task {
                    let res = result {
                        let! (userId, message) =
                            if interactionCreate.Type <> InteractionType.MessageComponent then
                                Error("Invalid interaction type")
                            else
                                match (interactionCreate.Member, interactionCreate.User, interactionCreate.Message) with
                                | (Undefined, Undefined, _) -> Error("No user for the interaction")
                                | (Defined guildMember, Undefined, Defined message) -> Ok((guildMember.User.Value.ID.Value, message))
                                | (Undefined, Defined user, Defined message) -> Ok((user.ID.Value, message))
                                | _ -> Error("General validation error")

                        let! embed =
                            match message.Embeds.Count with
                            | 1 -> Ok(message.Embeds[0])
                            | _ -> Error("Message format is incorrect: embed not singular")

                        let! pollRows =
                            match embed.Fields with
                            | Defined fields -> Ok(fields)
                            | Undefined -> Error("No embed fields")

                        let! data =
                            match interactionCreate.Data with
                            | Defined d -> Ok(d)
                            | Undefined -> Error("Could not find data for the interaction")

                        let! buttonRow =
                            match data.CustomID with
                            | Defined value ->
                                match System.Int32.TryParse value with
                                | true,int -> Ok int
                                | _ -> Error("Could not parse the row from the ID")
                            | Undefined -> Error("No CustomID found")

                        let! embedRow =
                            if pollRows.Count > buttonRow then
                                Ok(pollRows[buttonRow])
                            else
                                Error($"Invalid row found {buttonRow}")
                                
                        let voters = embedRow.Value.Split(' ') |> Seq.toList

                        let mention = $"<@{userId}>"

                        let updatedVoters =
                            if voters |> List.contains mention then
                                voters |> List.filter (fun element -> element <> mention)
                            else
                                voters |> List.append [mention]

                        // TODO - list the count of voters somewhere in this.
                        let updatedEmbedRow = new EmbedField(embedRow.Name, String.concat " " updatedVoters) :> IEmbedField
                        let updatedPollRows = 
                            pollRows 
                            |> Seq.mapi (fun i original -> if i = buttonRow then updatedEmbedRow else original)
                            |> Seq.toList

                        let updatedEmbed = new Embed(embed.Title, Type=embed.Type, Fields=new Optional<IReadOnlyList<IEmbedField>>(updatedPollRows))

                        return interactionService.EditOriginalInteractionResponseAsync(
                            interactionCreate.ID,
                            interactionCreate.Token,
                            embeds=new Optional<IReadOnlyList<IEmbed>>([updatedEmbed])) 
                    }

                    match res with
                    | Result.Ok t -> 
                        let! awaitResult = t
                        return if awaitResult.IsSuccess then Result.FromSuccess() else Result.FromError(awaitResult.Error)
                    | Result.Error err -> return Result.FromError(new InvalidOperationError(err))
                }

        [<Command("poll"); Description("Creates a poll.")>]
        member public self.createPoll(message: string, ct : CancellationToken) : Task<Result> =
                let create (interactionContext : InteractionContext) =
                    task {
                        let poll = parseFromInput message

                        let embedFields =
                            poll.Options
                            |> Seq.map (fun option -> new EmbedField(option.Option, String.concat " " option.Voters) :> IEmbedField)
                            |> Seq.toList

                        let embed = new Embed(Title=poll.Title, Type=EmbedType.Rich, Fields=new Optional<IReadOnlyList<IEmbedField>>(embedFields))

                        let interactionButtons =
                            poll.Options
                            |> Seq.chunkBySize 5
                            |> Seq.mapi (fun chunk options ->
                                options
                                |> Seq.mapi (fun i option ->
                                    new ButtonComponent(
                                        ButtonComponentStyle.Primary,
                                        new Optional<string>(option.Option),
                                        CustomID=new Optional<string>((5 * chunk + i).ToString()))))
                            |> Seq.map (fun buttons -> new ActionRowComponent(buttons |> Seq.cast<IMessageComponent> |> Seq.toList) :> IMessageComponent)
                            |> Seq.toList

                        return! interactionService.CreateInteractionResponseAsync(
                                    interactionContext.ID,
                                    interactionContext.Token,
                                    new InteractionResponse(
                                        InteractionCallbackType.UpdateMessage,
                                        new Optional<IInteractionCallbackData>(
                                             new InteractionCallbackData(
                                                 Embeds=new Optional<IReadOnlyList<IEmbed>>([embed]),
                                                 Components=new Optional<IReadOnlyList<IMessageComponent>>(interactionButtons)))),
                                    ct)
                }

                match commandContext with
                | :? InteractionContext as i -> create i
                | _ -> Result.FromError(new InvalidOperationError("Cannot create a poll without an interaction context")) |> Task.FromResult


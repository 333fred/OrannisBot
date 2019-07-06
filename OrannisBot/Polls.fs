module Polls

open System.Text
open DSharpPlus.Entities
open System.Diagnostics

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

    let rec map2Uneven mapper list1 list2 =
        match list1 with
        | head :: tail when list2 <> [] -> list.Cons((mapper head (List.head list2)), (map2Uneven mapper tail (List.tail list2)))
        | _ -> []
        

    let parseFromInput (message: string) (optionEmoji: DiscordEmoji list) =
        let getPollInputs(pollText: string) =
            let builder = StringBuilder()
            let mutable pollElements = []
            let mutable curIndex = 0
            let mutable inQuote = false
            
            let finishPollElement() =
                inQuote <- false
                pollElements <- List.Cons(builder.ToString(), pollElements)
                builder.Clear() |> ignore

            while curIndex < pollText.Length do
                let curChar = pollText.Chars(curIndex)
                curIndex <- curIndex + 1
                match curChar with
                | '"' when not inQuote -> inQuote <- true
                | '"' when inQuote ->
                    finishPollElement()
                    while curIndex < pollText.Length && pollText.Chars(curIndex) = ' ' do
                        curIndex <- curIndex + 1
                | '\\' ->
                    match pollText.Chars(curIndex) with
                    | '"' -> builder.Append('"') |> ignore
                    | '\\' -> 
                        // Need to append twice, because discord will treat a single slash as an escape
                        builder.Append('\\') |> ignore
                        builder.Append('\\') |> ignore
                    | ' ' -> builder.Append(' ') |> ignore
                    | _ -> ()
                    curIndex <- curIndex + 1
                | ' ' when not inQuote -> finishPollElement()
                | ' ' when inQuote -> builder.Append(curChar) |> ignore
                | _ -> builder.Append(curChar) |> ignore

            if builder.Length <> 0 then
                finishPollElement()

            List.rev pollElements

        let pollElements = getPollInputs message
        let options = map2Uneven (fun option (emoji: DiscordEmoji) -> {Option = (sprintf "%s:\t%s" (emoji.ToString()) option); Voters = [] }) pollElements.Tail optionEmoji

        {Title = pollElements.Head; Options= options}

    let parseFromMessage (message: DiscordMessage) (possibleReactions : DiscordEmoji list) : Async<Poll> = async {
        Debug.Assert(message.Embeds.Count = 1)

        let embed = message.Embeds.Item 0
        let options = embed.Fields |> Seq.map (fun field -> field.Name) |> List.ofSeq
        let relevantReactions = possibleReactions |> List.take options.Length

        let getVotes reaction = async {
            let! votes = message.GetReactionsAsync(reaction) |> Async.AwaitTask
            return votes |> List.ofSeq |> List.filter (fun user -> not user.IsBot) |> List.map (fun user -> user.Mention)
        }

        let votes = relevantReactions |> List.map (fun reaction -> async { return! getVotes reaction })

        let pollOptions : PollOption list = (List.zip options votes) |> List.map (fun (option, vote) -> {Option = option; Voters = vote |> Async.RunSynchronously})
        return {Title = embed.Title; Options = pollOptions}
    }

    let formatPollEmbed (poll: Poll): DiscordEmbed =
        let embedBuilder = DiscordEmbedBuilder()
        embedBuilder.WithTitle(poll.Title) |> ignore

        for option in poll.Options do
            embedBuilder.AddField(option.Option, match (option.Voters |> List.fold (fun str el -> str + " " + el) "") with
                                                 | "" -> "No Votes"
                                                 | str -> str) |> ignore
            
        embedBuilder.Build()


module Polls

open DSharpPlus.Entities
open System.Diagnostics

    type Emoji =
        abstract DiscordEmoji : DiscordEmoji with get

    type DiscordBackedEmoji(emoji : DiscordEmoji) =
        interface Emoji with
            member this.DiscordEmoji = emoji
        override this.ToString() =
            emoji.ToString()

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

    let zipUneven (list1 : 'a list) (list2 : 'b list) =
        let list1Length : int = list1.Length
        let list2Length : int = list2.Length
        match (list1Length.CompareTo list2Length) with
        | 0 -> List.zip list1 list2
        | comp when comp > 0 -> List.zip (list1 |> List.take list2Length) list2
        | _ -> List.zip list1 (list2 |> List.take list1Length)

    let parseFromInput (message: string) =
        let escapeChar (chars : char list) : (string * char list) =
            match chars with
            | [] -> ("", [])
            | head :: tail -> match head with
                              | '\\' -> ("\\\\", tail)
                              | _ -> (head.ToString(), tail)

        let rec doParse (chars: char list) : string List =
            match chars with
            | [] -> []
            | head :: tail -> if head = ' ' then
                                doParse tail
                              else
                                let (nextWord, remainder) = match head with
                                                            | '"' -> parseQuote tail ""
                                                            | _ -> parseWord chars ""
                                list.Cons(nextWord, (doParse remainder))
                         

        and parseWord (chars: char list) (current : string) : (string * char list) =
            match chars with
            | [] -> (current, chars)
            | head :: tail -> match head with
                              | '\\' -> let (escaped, remainder) = escapeChar tail
                                        parseWord remainder (current + escaped)
                              | '"' -> (current, chars)
                              | ' ' -> (current, tail)
                              | _ -> parseWord tail (current + head.ToString())

        and parseQuote (chars : char list) (current : string) : (string * char list) =
            match chars with
            | [] -> (current, chars)
            | head :: tail -> match head with
                              | '\\' -> let (escaped, remainder) = escapeChar tail
                                        parseQuote remainder (current + escaped)
                              | '"' -> (current, tail)
                              | _ -> parseQuote tail (current + head.ToString())

        let parsedElements = doParse (message.ToCharArray() |> List.ofArray)

        match parsedElements with
        | [] -> {Title = ""; Options = []}
        | _ -> {Title = parsedElements.Head; Options = (parsedElements.Tail |> List.map(fun o -> {Option = o; Voters = []}))}

    let parseFromMessage (message: DiscordMessage) (possibleReactions : Emoji list) : Async<Poll> = async {
        Debug.Assert(message.Embeds.Count = 1)

        let embed = message.Embeds.Item 0
        let options = embed.Fields |>
                      Seq.map (fun field -> field.Name) |>
                      Seq.map (fun name ->
                                   let index = name.IndexOf(":")
                                   name.Substring(index + 1).Trim()) |>
                      List.ofSeq
        let relevantReactions = possibleReactions |> List.take options.Length

        let getVotes (reaction : Emoji) = async {
            let! votes = message.GetReactionsAsync(reaction.DiscordEmoji) |> Async.AwaitTask
            return votes |> List.ofSeq |> List.filter (fun user -> not user.IsBot) |> List.map (fun user -> user.Mention)
        }

        let votes = relevantReactions |> List.map (fun reaction -> async { return! getVotes reaction })

        let pollOptions : PollOption list = (List.zip options votes) |> List.map (fun (option, vote) -> {Option = option; Voters = vote |> Async.RunSynchronously})
        return {Title = embed.Title; Options = pollOptions}
    }

    let formatPollEmbed (poll: Poll) (emojis : Emoji list): DiscordEmbed =
        let embedBuilder = DiscordEmbedBuilder()
        embedBuilder.WithTitle(poll.Title) |> ignore

        for (option, emoji) in (zipUneven poll.Options emojis) do
            embedBuilder.AddField((sprintf "%s:\t%s" (emoji.ToString()) option.Option),
                                  match (option.Voters |> List.fold (fun str el -> str + " " + el) "") with
                                  | "" -> "No Votes"
                                  | str -> let voterCount = option.Voters.Length
                                           sprintf "%i Vote%s - %s" voterCount (if voterCount = 1 then "" else "s")  str) |> ignore
            
        embedBuilder.Build()


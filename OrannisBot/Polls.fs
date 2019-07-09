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
        match list1, list2 with
        | head1 :: tail1, head2 :: tail2 -> (mapper head1 head2) :: (map2Uneven mapper tail1 tail2)
        | _ -> []

    let zipUneven (list1 : 'a list) (list2 : 'b list) =
        map2Uneven (fun h1 h2 -> (h1, h2)) list1 list2

    let parseFromInput (message: string) =
        let escapeChar (chars : char list) : (string * char list) =
            match chars with
            | [] -> ("", [])
            | '\\' :: tail -> ("\\\\", tail)
            | head :: tail -> (head.ToString(), tail)

        let rec doParse (chars: char list) : string List =
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

    let parseFromMessage (message: DiscordMessage) (possibleReactions : Emoji list) : Async<Poll> = async {
        Debug.Assert(message.Embeds.Count = 1)

        let embed = message.Embeds.Item 0
        let options = embed.Fields |>
                      Seq.map (fun field -> 
                                    let name = field.Name
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
                                  match (String.concat " " option.Voters) with
                                  | "" -> "No Votes"
                                  | str -> let voterCount = option.Voters.Length
                                           sprintf "%i Vote%s - %s" voterCount (if voterCount = 1 then "" else "s")  str) |> ignore
            
        embedBuilder.Build()


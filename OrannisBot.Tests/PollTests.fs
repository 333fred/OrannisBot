module public PollTests

open Polls
open Xunit

    type TestEmoji() =
        interface Emoji with
            member this.DiscordEmoji with get() = invalidOp "Testing has no discord connection"
        override this.ToString() = ""

    let testEmoji = List.replicate 26 (TestEmoji() :> Emoji)

    let createExpected (title: string)  (options : string list) : Poll =
        {Title = title; Options = (options |>  List.map (fun o -> {Option = o; Voters = []}))}

    [<Fact>]
    let ``Simple Poll Parse``() =
        let expectedPoll = createExpected "SimplePoll" ["Option1"; "Option2"; "Option3"]
        let actualPoll = parseFromInput "SimplePoll Option1 Option2 Option3"
        Assert.Equal<Poll>(expectedPoll, actualPoll)

    [<Fact>]
    let ``Quoted Poll Parse``() =
        let expectedPoll = createExpected "Quoted Poll" ["Option 1"; "Option 2"; "Option 3"]
        let actualPoll = parseFromInput "\"Quoted Poll\" \"Option 1\" \"Option 2\" \"Option 3\""
        Assert.Equal<Poll>(expectedPoll, actualPoll)

    [<Fact>]
    let ``Escaped Quote Parse``() =
        let expectedPoll = createExpected "\"QuotedTitle" ["\"Option" ; "1"; "Option2\""]
        let actualPoll = parseFromInput "\\\"QuotedTitle \\\"Option 1 Option2\\\""
        Assert.Equal<Poll>(expectedPoll, actualPoll)

    [<Fact>]
    let ``Quote Ending Word Parse``() =
        let expectedPoll = createExpected "Title" ["Option 1"; "Option 2"]
        let actualPoll = parseFromInput "Title\"Option 1\" \"Option 2\""
        Assert.Equal<Poll>(expectedPoll, actualPoll)

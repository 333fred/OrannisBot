module Config

open Newtonsoft.Json
open System.IO

    type JsonConfig() =
        [<JsonProperty("discord_token")>]
        member val public DiscordToken : string = "" with get, set

    let readConfig() : JsonConfig = 
        let configText = File.ReadAllText("config.json")
        JsonConvert.DeserializeObject<JsonConfig>(configText)

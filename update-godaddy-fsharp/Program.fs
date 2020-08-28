// Learn more about F# at http://fsharp.org

open System
open System.IO
open FSharp.Data
open Argu

let GDKEY = Environment.GetEnvironmentVariable "GODADDY_KEY"
let GDSECRET = Environment.GetEnvironmentVariable "GODADDY_SECRET"
let authHeader = "sso-key " + GDKEY + ":" + GDSECRET

let buildDNSAPIURL (recordType : string) (recordName : string) (domain : string) =
    String.concat "/" ["https://api.godaddy.com/v1/domains"; domain; "records"; recordType; recordName]

let buildCacheFilename (recordType : string) (recordName : string) (domain : string) =
    String.concat "." ["/tmp/.updategodaddy"; recordType; recordName; domain; "addr"]

let writeCache (recordType : string) (recordName : string) (domain : string) (newIP : string) =
    let filename = buildCacheFilename recordType recordName domain
    File.WriteAllText(filename, newIP)

type GodaddyResponseJson = JsonProvider<""" [{
  "data": "string",
  "name": "string",
  "port": 0,
  "priority": 0,
  "protocol": "string",
  "service": "string",
  "ttl": 0,
  "type": "A",
  "weight": 0
}] """>

type APIResponseJson = JsonProvider<""" { "ip": "123.123.123.123." } """>

let getGodaddyRecord (recordType : string) (recordName : string) (domain : string) = 
    let response =
        Http.Request ((buildDNSAPIURL recordType recordName domain), headers=[ HttpRequestHeaders.Authorization authHeader ])
    match response.StatusCode with
    | 200 -> ()
    | _ -> failwith "Request failed."
    let responseBody =
        match response.Body with
        | Text(body) -> body |> GodaddyResponseJson.Parse
        | _ -> failwith "Invalid JSON."
    responseBody.[0].Data

let getPublicRecord (api : string) =
    let response = 
        Http.RequestString api |> APIResponseJson.Parse
    response.Ip

let updateRecord (recordType : string) (recordName : string) (domain : string) (newIP : string) (dry : bool) =
    writeCache recordType recordName domain newIP
    if (getGodaddyRecord recordType recordName domain) = newIP
    then printf "DNS record is the same as server IP"
    else
        if dry then
            printf "Dry Run: Update IP to %s" newIP
        else
            let response = Http.Request ((buildDNSAPIURL recordType recordName domain), body=FormValues ["data", newIP; "ttl", "3600"], httpMethod=HttpMethod.Put, headers=[ HttpRequestHeaders.Authorization authHeader; HttpRequestHeaders.ContentType "application/json"])
            match response.StatusCode with
            | 200 -> printf "Update successful"
            | _ -> failwith "DNS update failed."

let checkCache (recordType : string) (recordName : string) (domain : string) =
    let cacheFile = buildCacheFilename recordType recordName domain
    match (File.Exists cacheFile) with
    | true -> Some((File.ReadAllText cacheFile).Trim())
    | false -> None

let checkRecord (recordType : string) (recordName : string) (domain : string) (api : string) (dry : bool) =
    let cache = checkCache recordType recordName domain
    let pubRecord = getPublicRecord api
    let isValid =
        match recordType with
        | "A" -> pubRecord.Contains '.'
        | "AAAA" -> pubRecord.Contains ':'
        | _ -> failwith "Invalid recordType."
    match isValid with
    | true -> match cache with
              | Some(c) when c = pubRecord -> printf "IP address unchanged."
              | _ -> updateRecord recordType recordName domain pubRecord dry
    | false -> failwith "Invalid IP address."

type RecordType =
    | A
    | AAAA

type CLIArguments =
    | [<AltCommandLine("-4")>] Ipv4
    | [<AltCommandLine("-6")>] Ipv6
    | Api of url:string
    | Api6 of url:string
    | [<AltCommandLine("-n"); Mandatory>] Record_Name of string
    | [<AltCommandLine("-d"); Mandatory>] Domain_Name of string
    | Dry

    interface IArgParserTemplate with
        member this.Usage =
            match this with 
            | Ipv4 -> "Update A record."
            | Ipv6 -> "Update AAAA record."
            | Api _ -> "API to use for detecting IPv4 address."
            | Api6 _ -> "API to use for detecting IPv6 address."
            | Record_Name _ -> "DNS record to update, e.g. www, subdomain, etc."
            | Domain_Name _ -> "Domain name of the DNS record, e.g. mydomain.com"
            | Dry -> "Dry run (does not update remote DNS)"

type Exiter() =
    interface IExiter with
        member this.Name = "CLIArguments"
        member this.Exit (msg, _) =
            printf "%s" msg
            exit 1


[<EntryPoint>]
let main argv =
    printf "%s\n" GDKEY
    printf "%s\n" GDSECRET
    let parser = ArgumentParser.Create<CLIArguments>(programName = "update-godaddy.exe", errorHandler=Exiter())
    let results = parser.Parse(argv)
    let recordName = results.GetResult(Record_Name)
    let domainName = results.GetResult(Domain_Name)
    let dry = results.Contains Dry
    let recordType =
        if results.Contains Ipv6 then AAAA
        else A
    let api =
        match recordType with
        | A ->
            results.GetResult(Api, defaultValue="https://api.ipify.org?format=json")
        | AAAA ->
            results.GetResult(Api6, defaultValue="https://api6.ipify.org?format=json")
    checkRecord (recordType.ToString()) recordName domainName api dry
    
    0 // return an integer exit code

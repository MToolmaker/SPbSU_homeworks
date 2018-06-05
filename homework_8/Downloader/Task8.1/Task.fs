module Downloader
    open System.Net
    open System.IO
    open System.Text.RegularExpressions
    type Status =
        | Success of string * string * int
        | Error of string * string

    let downloadAllReferences url =
        let download url =
            async{
            try
                do printfn "Creating request for %s..." url
                let request = WebRequest.Create(url)
                use! response = request.AsyncGetResponse()
                do printfn "Getting response stream for %s..." url
                use stream = response.GetResponseStream()
                do printfn "Reading response for %s..." url
                use reader = new StreamReader(stream)
                let html = reader.ReadToEnd()
                return Success(url, html, html.Length)
            with
                | e -> return Error(url, e.Message)
            }

        let site =
            url
            |> download
            |> Async.RunSynchronously

        let regex = Regex("<a href=\"(https?://[^\"]+)\">")
        match site with
        | Success(_, html, _) ->
            let matchList = regex.Matches(html)
            let taskList =  [for item in matchList do yield item.Groups.[1].Value |> download]
            let res =
                taskList
                |> Async.Parallel
                |> Async.RunSynchronously
            Some(res)
        | Error(_, _) -> None

    let showLengths url =
        let results = downloadAllReferences url
        match results with
        | Some(statusList) ->
            for status in statusList do
                match status with
                | Success(url, _, length) ->
                    printfn "Url: %s contains %d symbols" url length
                | Error(url, message) ->
                    printfn "Url: %s . Raised error: %s" url message
        | None ->
            printfn "Url: %s . Get response error" url
            


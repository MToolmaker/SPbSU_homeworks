module Tests
    open NUnit.Framework
    open FsUnit
    open Downloader

    [<Test>]
    let ``Test1`` () =
        let res = downloadAllReferences "https://www.google.ru"
        let expectedLenght = 77636
        match res with
        | Some(statusList) ->
            match statusList.[0] with
            | Success(url, _, length) -> length |> should equal expectedLenght
            | _ -> failwith "Incorrect length"
        | None -> failwith "Cannot load https://www.google.ru"
    
    [<Test>]
    let ``Test2`` () =
        let res = downloadAllReferences "https://www.incorrectnonexistingsite.somecountry"
        match res with
        | None -> ()
        | Some(_) -> failwith "Are you kidding me?"
      

        

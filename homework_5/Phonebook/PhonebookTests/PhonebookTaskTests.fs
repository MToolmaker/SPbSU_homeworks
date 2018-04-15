module PhonebookTaskTests
    open NUnit.Framework
    open FsUnit
    open PhonebookTask
    open System.IO

    [<Test>]
    let ``findByName "Ivan" must be "+79110000000"`` () =
        let ivanRecord = {Name = "Ivan"; Number = "+79110000000"}
        let johnRecord = {Name = "John"; Number = "+79110000001"}
        let phonebook : Phonebook = ivanRecord :: johnRecord :: []
        
        findByName "Ivan" phonebook |> should equal (Some("+79110000000"))
    
    [<Test>]
    let ``findByName "John" must be "+79110000001"`` () =
        let ivanRecord = {Name = "Ivan"; Number = "+79110000000"}
        let johnRecord = {Name = "John"; Number = "+79110000001"}
        let phonebook : Phonebook = ivanRecord :: johnRecord :: []

        findByName "John" phonebook |> should equal (Some("+79110000001"))

    [<Test>]
    let ``findByName "Smith" must be None`` () =
        let ivanRecord = {Name = "Ivan"; Number = "+79110000000"}
        let johnRecord = {Name = "John"; Number = "+79110000001"}
        let phonebook : Phonebook = ivanRecord :: johnRecord :: []

        findByName "Smith" phonebook |> should equal None

    [<Test>]
    let ``findByNumber "+79110000000" must be "Ivan"`` () =
        let ivanRecord = {Name = "Ivan"; Number = "+79110000000"}
        let johnRecord = {Name = "John"; Number = "+79110000001"}
        let phonebook : Phonebook = ivanRecord :: johnRecord :: []
        
        findByNumber  "+79110000000" phonebook |> should equal (Some("Ivan"))
    
    [<Test>]
    let ``findByNumber "+79110000001" must be "John"`` () =
        let ivanRecord = {Name = "Ivan"; Number = "+79110000000"}
        let johnRecord = {Name = "John"; Number = "+79110000001"}
        let phonebook : Phonebook = ivanRecord :: johnRecord :: []
        
        findByNumber  "+79110000001" phonebook |> should equal (Some("John"))

    [<Test>]
    let ``findByNumber "+79110000002" must be None`` () =
        let ivanRecord = {Name = "Ivan"; Number = "+79110000000"}
        let johnRecord = {Name = "John"; Number = "+79110000001"}
        let phonebook : Phonebook = ivanRecord :: johnRecord :: []

        findByNumber "+79110000002" phonebook |> should equal None

    [<Test>]
    let ``addRecord {Name = "Ivan"; Number = "+79110000000"} [] must be [{Name = "Ivan"; Number = "+79110000000"}]`` () =
        addRecord {Name = "Ivan"; Number = "+79110000000"} [] |> should equal [{Name = "Ivan"; Number = "+79110000000"}]
    
    [<Test>]
    let ``addRecord johnRecord [ivanRecord] must be [johnRecord; ivanRecord]`` () =
        addRecord {Name = "John"; Number = "+79110000001"} [{Name = "Ivan"; Number = "+79110000001"}]
        |> should equal [{Name = "John"; Number = "+79110000001"}; {Name = "Ivan"; Number = "+79110000001"}]

    [<Test>]
    let ``saveRecords and readRecords test`` () =
        let ivanRecord = {Name = "Ivan"; Number = "+79110000000"}
        let johnRecord = {Name = "John"; Number = "+79110000001"}
        let phonebook : Phonebook = ivanRecord :: johnRecord :: []
        let fsOut = new FileStream("testPhonebook1", FileMode.Create)
        saveRecords fsOut phonebook
        fsOut.Close()

        let fsIn = new FileStream("testPhonebook1", FileMode.Open)
        readRecords fsIn |> should equal phonebook
        fsIn.Close()


        
    


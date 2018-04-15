module PhonebookTask

    type Record = {Name : string; Number : string}
    type Phonebook = List<Record>

    let rec findByName (name : string) (phonebook : Phonebook) =
        match phonebook with
        | [] -> None
        | head :: _ when head.Name = name -> Some(head.Number)
        | _ :: tail -> findByName name tail

    let rec findByNumber (number : string) (phonebook : Phonebook) =
         match phonebook with
        | [] -> None
        | head :: _ when head.Number = number -> Some(head.Name)
        | _ :: tail -> findByNumber number tail
    
    let addRecord (record : Record) (phonebook : Phonebook) =
        record :: phonebook

    open System.Runtime.Serialization.Formatters.Binary
    open System.IO
    open System

    let readRecords inputStream =
        let formatter = new BinaryFormatter()
        let res = formatter.Deserialize(inputStream)
        unbox res : Phonebook

    let saveRecords outputStream (phonebook : Phonebook)=
        let formatter = new BinaryFormatter()
        formatter.Serialize(outputStream, phonebook)

    let runInteractiveMode (phonebook : Phonebook) =
        printfn "Welcome. It's phonebook interactive."
        let rec handleCommand phonebook =
            printfn "Enter command: (h - help)"
            let input = Console.ReadKey().KeyChar
            printfn "\n"
            match input with
            | 'h' -> printfn "Finding by name: q"
                     printfn "Finding by phone: w"
                     printfn "Adding record: e"
                     printfn "Reading records: r"
                     printfn "Saving records: t"
                     printfn "Exit: y"
                     handleCommand phonebook

            | 'q' -> printfn "Finding by name."
                     printfn "Print name:"
                     let name = Console.ReadLine()
                     let number = findByName name phonebook
                     if number.IsNone then
                        printfn "Number with given name wasn't found."
                     else
                        printfn "%s" ("Number:" + number.Value)
                     handleCommand phonebook

            | 'w' -> printfn "Finding by phone number."
                     printfn "Print number:"
                     let number = Console.ReadLine()
                     let name = findByNumber number phonebook
                     if name.IsNone then
                        printfn "Name with given number wasn't found."
                     else
                        printfn "%s" ("Name:" + name.Value)
                     handleCommand phonebook

            | 'e' -> printfn "Adding record."
                     printfn "Enter name:"
                     let name = Console.ReadLine()
                     printfn "Enter number:"
                     let number = Console.ReadLine()
                     let newRecord = {Name = name; Number = number}
                     handleCommand (newRecord :: phonebook)
            
            | 'r' -> printfn "Reading records."
                     let inputPath = Console.ReadLine()
                     if not (File.Exists(inputPath)) then
                        printfn "Records with given path don't exist."
                        handleCommand phonebook
                     else
                        let fsIn = new FileStream(inputPath, FileMode.Open)
                        let newPhonebook = readRecords fsIn
                        printfn "Phonebook was succesfully read."
                        handleCommand newPhonebook
            
            | 't' -> printfn "Saving records."
                     printfn "Enter directory path to save records:"
                     let outDir = Console.ReadLine()
                     if not (Directory.Exists(outDir)) then
                        printfn "Given directory doesn't exist."
                     else
                        printfn "Enter new phonebook name:"
                        let name = Console.ReadLine()
                        if not (File.Exists(outDir + name)) then
                            let fsOut = new FileStream(outDir + name, FileMode.Create)
                            printfn "New phonebook has been succesfully saved."
                            saveRecords fsOut phonebook
                        else
                            printfn "File with given name is already exist."
                     handleCommand phonebook

            | 'y' -> printfn "Goodbye."

            | _ -> printfn "Incorrect command. Try again"
                   handleCommand phonebook
            
        handleCommand phonebook
                     
                        

                     



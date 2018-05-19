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
                     printfn "Enter phonebook name:"
                     let name = Console.ReadLine()
                     if not (File.Exists(name)) then
                        printfn "Records with given name don't exist."
                        handleCommand phonebook
                     else
                        use fsIn = new FileStream(name, FileMode.Open)
                        let newPhonebook = readRecords fsIn
                        printfn "Phonebook was succesfully read."
                        handleCommand newPhonebook
            
            | 't' -> printfn "Saving records."
                     printfn "Enter new phonebook name:"
                     let name = Console.ReadLine()
                     if not (File.Exists(name)) then
                         use fsOut = new FileStream(name, FileMode.Create)
                         saveRecords fsOut phonebook
                         printfn "New phonebook has been succesfully saved."
                     else
                         printfn "File with given name is already exist."
                     
                     handleCommand phonebook

            | 'y' -> printfn "Goodbye."

            | _ -> printfn "Incorrect command. Try again"
                   handleCommand phonebook
            
        handleCommand phonebook
                     
                        

                     



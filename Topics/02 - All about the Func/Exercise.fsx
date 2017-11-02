let test = "The quick brown fox jumps over the lazy dog"

let chars = ['a' .. 'z']

let assertAnswer x y =
    if x = y then
        printfn "Test pass!"
    else
        printfn "Test fails"
    ()

let panagram' (test:string) =
    ['a' .. 'z']
    |> List.map (string >> test.Contains) 
    |> List.reduce (&&) 

(*
    Above is an implementation of the test for a panagram that uses higher order functions and composition.
    Reimplement the panagram test using recusion, in a way that does a single pass over the inputString. 
    If possible, avoid using higher order Collection functions (E.g. List.map)
    Note: this exercise has been pulled from http://exercism.io/exercises/fsharp/pangram/readme
*)
let panagramRec (inputString: string) = 
    let rec getDedupLetters (inputString: char[]) (n:int) (usedLetters: char[]): char[] =
        if n = 0 then
            usedLetters
        else
            let c = inputString.[n]
            let isLetter = List.contains c chars
            let hasBeenSeenBefore = Array.contains c usedLetters
            let usedLetters = if not(isLetter) || hasBeenSeenBefore then
                                    usedLetters
                              else
                                    Array.concat [usedLetters; [|c|]]
            getDedupLetters inputString (n - 1) usedLetters
            
    let usedLetters = getDedupLetters (inputString.ToCharArray()) (inputString.Length - 1) [||]
    usedLetters.Length = (['a' .. 'z']).Length
panagramRec test |> assertAnswer true


//Implement a function that recursively sums a list of functions. 
let rec sumList list =
    let tail = List.tail list
    match tail with
        | [] -> List.head list
        | _ -> (List.head list) + sumList tail

sumList [1; 2; 3] |> assertAnswer 6

// Reimplementing standard library functions
// NOTE: The answers for these functions can be easily found online, so don't search for them until you have given it a try and you've reached trouble. 

//TODO Reimplement List.map -> https://msdn.microsoft.com/en-us/library/ee370378.aspx
// Map takes a function and a list, applies the function to each element in the list, and returns a new list of the results. 

let rec mapList f list =
    match list with
    | [] -> 
        []  
    | head::tail -> 
        // new head + new tail
        (f head) :: (mapList f tail)

mapList (fun x -> x+1) [1;2;3]

//TODO Reimplement List.filter -> https://msdn.microsoft.com/en-us/library/ee370294.aspx
// Fold takes a function to test each list element (function returns a boolean) and a list. Returns a new list containly only the elements for which the predicate returned true. 
let rec filterList predicate list =
    match list with
    | [] -> 
        []  
    | head::tail -> 
        if predicate head then
            head :: (filterList predicate tail)
        else 
            filterList predicate tail


//TODO Reimplement List.fold -> https://msdn.microsoft.com/en-us/library/ee353894.aspx
// Takes a function to update the accumulated values given the input element, the initial value, and the list. returns the final accumulated value
let rec foldList folder acc list =
    match list with
    | [] -> 
        acc
    | head::tail -> 
        foldList folder (folder (head) + acc) tail 
        
//TODO Reimplement List.reduce
// Takes a function to update the accumulated values given the input element and the list. returns the final accumulated value
//Throws an ArgumentException if the list is empty (Use invalidArg function -> https://msdn.microsoft.com/en-us/library/dd233178.aspx ) 
let rec reduceList reducer list =
    let head = List.head list
    let tail = List.tail list
    if tail.Length = 0 then
        reducer(head)
    else 
        reducer(head) + reduceList reducer tail 
    

//Using recursion, group an input list of numbers by the final digit. 
// Hint: To make things easier, you may want to use an array as apart of the output, but using a list or array is acceptable. 
let rec groupByFinalDigit list = [||]

// Using any approach (Recursion, Function composition or higher order functions), create a function that returns the squares of the positive numbers in a list
let squares list = 
    list
    |> List.filter (fun x -> x > -1)
    |> List.map (fun x -> x * x)
// Using any approach (Recursion, Function composition or higher order functions), create a function that returns the doubles (*2) of the positive numbers in a list
let doubles list =
    list
    |> List.filter (fun x -> x > -1)
    |> List.map (fun x -> x * 2)
(*
    Using the functions above, create a function that takes in an input list of numbers and does the following
    - Groups the numbers by the final digit
    - For each group, if the final digit of all the numbers are even, the numbers are squared. If the final digit of all the numbers are odd, the numbers are doubled
    - For each group, the numbers are summed 
    - The square root of each sum is taken
    - The largest and smallest square root is returned, with the digit of the original group the square root came from. 
*)
let doABunchOfMathsStuff = []
   

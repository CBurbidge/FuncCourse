// If you haven't already, write the factorial function in a tail recursive manner, using an accumulator variable. 
let rec factorial x = 0
open System
//Rewrite the factorial function in a tail recursive manner, using continuation passing
let rec factorial' x = 0

let rec fib x = 
    if x < 2 then 
        1 
    else 
        fib (x-2) + fib(x-1)

//Rewrite the fibonacci function in a tail recursive manner (Only try continuation passing if you feel like it)
let rec fibTail x = 0    

// Solve this code wars problem recursively: // https://www.codewars.com/kata/550f22f4d758534c1100025a
let exampleDirections = ["NORTH";"SOUTH";"SOUTH";"EAST";"WEST";"NORTH";"NORTH";"WEST";"SOUTH"]

                             
let reduceDir x =
    let rec reduceDirRec (dir: (int * int)) (remainingDirections: string list): (int * int) =
        let getNewDir direction currentDir = 
            let ns, ew = currentDir
            match direction with
                | "NORTH" -> (ns + 1, ew)
                | "SOUTH" -> (ns - 1, ew)
                | "EAST" -> (ns, ew + 1)
                | "WEST" -> (ns, ew - 1)
                | _ -> currentDir
        match remainingDirections with
            | [] -> dir
            | head::tail -> reduceDirRec (getNewDir head dir) tail    
    let directionToList direction =
        let ns, ew = direction
        let nsDirections = match ns with
                            | x when x > 0 -> List.replicate x "NORTH"
                            | x when x < 0 -> List.replicate (abs x) "SOUTH"
                            | _ -> []
        let ewDirections = match ew with
                            | x when x > 0 -> List.replicate x "EAST"
                            | x when x < 0 -> List.replicate (abs x) "WEST"
                            | _ -> []
        List.concat [nsDirections; ewDirections]
    let direction = reduceDirRec (0, 0) x
    directionToList direction
reduceDir exampleDirections
    

// ReImplement the library functions using recusion. modify the signature as necessary 
// (Only try continuation passing if you feel like it, its probably easier to do this using an accumulator)
// Use a seperate value so that you can compare to the real signature 
let functionToCopy = List.groupBy
// let groupBy (f: 'a -> 'b) (lis: 'a list): ('b * 'a list) list =
//     let dict = Dictionary<'b, 'a list>()
//     let groupByRec aggDict rest =
//         match rest with
//             | [] -> dir
//             | head::tail -> let k = f head
//                             if aggDict.ContainsKey(k) then aggDict.[k].append(head) 
//                             else aggDict.[k] <- [head]
//     let
let rec groupBy (f: 'a -> 'b) (agg: ('b * 'a list) list) (rest: 'a list): ('b * 'a list) list =
    match rest with
        | [] -> agg
        | head::tail -> let key = f head
                        let currentKeyList = List.filter (fun x -> (fst x) = (fst x)) agg
                        let nonKeyElements = List.filter (fun x -> (fst x) <> (fst x)) agg
                        let keyElem = if List.isEmpty currentKeyList then 
                                        (key, [head]) 
                                      else
                                        (key, (head :: (snd (List.head currentKeyList))))
                        let newAgg = keyElem :: nonKeyElements
                        groupBy f newAgg tail

groupBy (fun x -> x.ToString()) [] [1; 2; 3; 1; 2; 1; 4]
let functionToCopy2 = List.collect

let functionToCopy3 = List.windowed


// Using sequences, generate numbers using the transformation y = sin(x) + log(x) and the range beteen start and finish (inclusive) with a step
let generator start finish step = "TODO"

// Using recursion, manually create a generator for a sequence of numbers using the transformation  y = sin(x) + log(x)
// Don't use sequences!
// This should require a series of function calls by the user to build up the sequence. 
// Remember that you need some way to tell the function caller that there are no more values in the sequence. 

let generator' start finish step = "TODO"

// Generalise the function above so that it can work on any transformation function.
// You will need to modify the signature as well. 
let generator'' start finish step = "TODO"

// Generalise the function above further so that it can produce an infinite series
// You will need to modify the signature as well. 
let generator''' start finish step = "TODO"

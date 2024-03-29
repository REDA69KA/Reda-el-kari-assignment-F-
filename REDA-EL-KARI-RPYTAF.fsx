//REDA EL KARI RPYTAF


// EXRECISE01

let squareList (numbs: int list) =
    [for num in numbs -> num * num]

let numbs = [2; 4; 6; 8; 10; 12; 14; 16; 18; 20]
let squaredNumbers = squareList numbs
printfn "Squared numbers: %A" squaredNumbers


// EXRECISE02

let filterEvenNumbers (nums: int list) : int list =
    List.filter (fun x -> x % 2 = 0) nums

let nums = [4 .. 16] 
let evenNums = filterEvenNumbers nums 
printfn "Even numbers: %A" evenNums


// EXRECISE03

let sumOfPositiveNumbers (numbers: int list) : int =
    List.sum (List.filter (fun x -> x > 0) numbers)

let newNumbers = [-2; 8; -4; 6; -1; 9 ; 0 ; 11 ; -12]
let sumOfPositives = sumOfPositiveNumbers newNumbers
printfn "Sum of positive numbers: %d" sumOfPositives


// EXRECISE04

let capitalizeNames (names: string list) : string list =
    List.map (fun (name: string) -> name.ToUpper()) names

let names = [ "reda"; "el kari" ; "ila" ; "yooow" ; "body" ; "lbarba" ]
let capitalizedNames = capitalizeNames names
printfn "Capitalized names: %A" capitalizedNames

// EXRECISE05

let stringsWithLengthGreaterThanN (strings: string list) (n: int) : string list =
    List.filter (fun str -> String.length str > n) strings


let strings = ["Dwight"; "jim"; "pam"; "michael"; "stanlly" ; "the" ; "creed" ; "scranton"]
let n = 6
let filteredStrings = stringsWithLengthGreaterThanN strings n
printfn "Strings with length greater than %d: %A" n filteredStrings


// EXRECISE06

let countDivisibleBy divisor (numbers: int list) : int =
    List.filter (fun num -> num % divisor = 0) numbers |> List.length

let numbers = [5 .. 40] 
let customDivisor = 5 
let count = countDivisibleBy customDivisor numbers
printfn "Count of numbers divisible by %d: %d" customDivisor count


// EXRECISE07

let findIndicesOfElement elementToFind (list: 'a list) : int list =
    list
    |> List.mapi (fun index x -> if x = elementToFind then Some index else None)
    |> List.filter Option.isSome
    |> List.map Option.get

let numbers = [7; 5; 6; 9; 7; 5; 6; 11; 5; 7; 5]
let elementToFind = 6
let indices = findIndicesOfElement elementToFind numbers
printfn "Indices of element %A: %A" elementToFind indices


// EXRECISE08

let concatenateLongerStrings n (strings: string list) : string =
    strings
    |> List.filter (fun str -> String.length str > n)
    |> String.concat ""


let strings = ["that"; "what"; "she"; "or"; "he" ;"said" ; "michael" ; "scott"; "best";"show"]
let n = 5
let concatenatedString = concatenateLongerStrings n strings
printfn "Concatenated string of strings longer than %d: %s" n concatenatedString


// EXRECISE09

let findMaxTuple (pairs: (int * int) list) : (int * int) option =
    match pairs with
    | [] -> None 
    | _ ->
        let maxPair = List.maxBy snd pairs
        Some maxPair

let pairs = [(10, 100); (20, 50); (30, 200); (40, 150); (50, 300); (60, 250)]
match findMaxTuple pairs with
| Some (identifier, value) -> printfn "Pair with maximum value: (%i, %i)" identifier value
| None -> printfn "List is empty"


// EXRECISE10

let countOccurrences (list: 'a list) : ('a * int) list =
    lisT
    |> List.groupBy id
    |> List.map (fun (element, occurrences) -> (element, List.length occurrences))


let elements = [99; 88; 88; 88; 55; 66; 66; 66; 66; 99; 99 ; 99 ; 55;]
let occurrences = countOccurrences elements
printfn "Occurrences: %A" occurrences

// EXRECISE11

type TrafficLight =
    | Red
    | Yellow
    | Green

let nextState (currentState: TrafficLight) : TrafficLight =
    match currentState with
    | Red -> Green
    | Green -> Yellow
    | Yellow -> Red


// EXRECISE12

type ArithmeticOperation =
    | Add
    | Subtract
    | Multiply
    | Divide


let performOperation (operation: ArithmeticOperation) (x: float) (y: float) : float option =
    match operation with
    | Add -> Some (x + y)
    | Subtract -> Some (x - y)
    | Multiply -> Some (x * y)
    | Divide -> if y = 0.0 then None else Some (x / y)



let result1 = performOperation Add 3.5 4.0
let result2 = performOperation Subtract 333.0 10.1
let result3 = performOperation Multiply 35.0 67.0
let result4 = performOperation Divide 8.0 2.0


printfn "Addition result: %A" result1
printfn "Subtraction result: %A" result2
printfn "Multiplication result: %A" result3
printfn "Division result: %A" result4


// EXRECISE13

type Shape =
    | Circle of radius: float
    | Rectangle of width: float * height: float
    | Square of side: float

let calculateArea (shape: Shape) : float =
    match shape with
    | Circle radius -> System.Math.PI * radius * radius
    | Rectangle (width, height) -> width * height
    | Square side -> side * side


let circle = Circle(2.2)
let rectangle = Rectangle(6.0, 8.0)
let square = Square(4.0)

printfn "Area of the circle: %.2f" (calculateArea circle)
printfn "Area of the rectangle: %.2f" (calculateArea rectangle)
printfn "Area of the square: %.2f" (calculateArea square)



// EXRECISE14

type TemperatureScale =
    | Celsius
    | Fahrenheit

let convertTemperature (temperature: float) (fromScale: TemperatureScale) (toScale: TemperatureScale) : float =
    match fromScale, toScale with
    | Celsius, Fahrenheit -> (temperature * 9.0 / 5.0) + 32.0
    | Fahrenheit, Celsius -> (temperature - 32.0) * 5.0 / 9.0
    | _ -> temperature  

let celsiusTemperature = 22.0
let fahrenheitTemperature = 73.0

let convertedToFahrenheit = convertTemperature celsiusTemperature Celsius Fahrenheit
let convertedToCelsius = convertTemperature fahrenheitTemperature Fahrenheit Celsius

printfn "%.1f degrees Celsius is %.1f degrees Fahrenheit" celsiusTemperature convertedToFahrenheit
printfn "%.1f degrees Fahrenheit is %.1f degrees Celsius" fahrenheitTemperature convertedToCelsius


// EXRECISE15

type JsonValue =
    | JsonObject of (string * JsonValue) list
    | JsonArray of JsonValue list
    | JsonString of string
    | JsonNumber of float
    | JsonBoolean of bool

let rec prettyPrint (json: JsonValue) : string =
    match json with
    | JsonObject items ->
        let itemsAsString = List.map (fun (key, value) -> sprintf "\"%s\": %s" key (prettyPrint value)) items
        sprintf "{ %s }" (String.concat ", " itemsAsString)
    | JsonArray items ->
        let itemsAsString = List.map prettyPrint items
        sprintf "[ %s ]" (String.concat ", " itemsAsString)
    | JsonString str -> sprintf "\"%s\"" str
    | JsonNumber num -> sprintf "%f" num
    | JsonBoolean b -> if b then "true" else "false"

let json =
    JsonObject [
        ("name", JsonString "John");
        ("age", JsonNumber 28.0);
        ("isStudent", JsonBoolean false);
        ("grades", JsonArray [JsonNumber 85.0; JsonNumber 40.0; JsonNumber 75.0])
    ]

let jsonString = prettyPrint json
printfn "%s" jsonString


// EXRECISE16

let rec fibonacci n =
    if n <= 1 then
        n
    else
        fibonacci (n - 1) + fibonacci (n - 2)

let inputN = 8
let fibonacciResult = fibonacci inputN
printfn "The %dth Fibonacci number is %d" inputN fibonacciResult

// EXRECISE17

let rec binarySearch (arr: int[]) (target: int) (lowerBound: int) (upperBound: int) : int option =
    if lowerBound > upperBound then
        None
    else
        let mid = (lowerBound + upperBound) / 2
        match arr.[mid] with
        | midValue when midValue = target -> Some mid
        | midValue when midValue > target -> binarySearch arr target lowerBound (mid - 1)
        | _ -> binarySearch arr target (mid + 1) upperBound


let sortedArray = [|1; 3; 5; 7; 9; 11; 13; 15; 17; 19|]
let target = 13
match binarySearch sortedArray target 0 (Array.length sortedArray - 1) with
| Some index -> printfn "Element %d found at index %d" target index
| None -> printfn "Element %d not found in the array" target


// EXRECISE18

let rec mergeSort (lst: int list) : int list =
    let rec merge (left: int list) (right: int list) : int list =
        match left, right with
        | [], right -> right
        | left, [] -> left
        | x::xs, y::ys ->
            if x < y then x :: merge xs (y::ys)
            else y :: merge (x::xs) ys

    let rec split (lst: int list) : int list * int list =
        match lst with
        | [] -> [], []
        | [x] -> [x], []
        | x::y::rest ->
            let left, right = split rest
            x::left, y::right

    match lst with
    | [] -> []
    | [x] -> [x]
    | _ ->
        let left, right = split lst
        let sortedLeft = mergeSort left
        let sortedRight = mergeSort right
        merge sortedLeft sortedRight

// Example :
let myList = [5; 2; 7; 1; 3; 6; 4]
let sortedList = mergeSort myList
printfn "Sorted list: %A" sortedList

// EXRECISE19

type TreeNode =
    | Node of val: int * leftNode: TreeNode * rightNode: TreeNode
    | EmptyNode

let rec calculateDepth (treeNode: TreeNode) : int =
    match treeNode with
    | EmptyNode -> 0
    | Node(_, left, right) -> 1 + max (calculateDepth left) (calculateDepth right)

let binaryTree =
    Node(1,
        Node(2,
            Node(4, EmptyNode, EmptyNode),
            Node(5, EmptyNode, EmptyNode)),
        Node(3,
            Node(6, EmptyNode, EmptyNode),
            EmptyNode))

let treeDepth = calculateDepth binaryTree
printfn "Depth of the binary tree: %d" treeDepth


// EXRECISE20

let rec isPalindrome (str: string) : bool =
    match str.Length with
    | length when length <= 1 -> true  
    | _ when str.[0] <> str.[str.Length - 1] -> false
    | _ -> isPalindrome (str.[1..str.Length - 2])


printfn "%b" (isPalindrome "madam")  // true
printfn "%b" (isPalindrome "alikoum")  // false

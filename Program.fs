//
// F# program to analyze Divvy daily ride data.
//
// << Gina Gerace >>
// U. of Illinois, Chicago
// CS 341, Fall 2019
// Project #04
//

#light

module project04

//
// ParseLine and ParseInput
//
// Given a sequence of strings representing Divvy data, 
// parses the strings and returns a list of lists.  Each
// sub-list denotes one bike ride.  Example:
//
//   [ [15,22,141,17,5,1124]; ... ]
//
// The values are station id (from), station id (to), bike
// id, starting hour (0..23), starting day of week (0 Sunday-6 Saturday)
// and trip duration (secs), 
//
let ParseLine (line:string) = 
  let tokens = line.Split(',')
  let ints = Array.map System.Int32.Parse tokens
  Array.toList ints

let rec ParseInput lines = 
  let rides = Seq.map ParseLine lines
  Seq.toList rides

let rec getNth data n = 
  match n, data with
    | 0, (d::_) -> d
    | _, (_::data) -> getNth data (n-1)
    | _, [] -> 0 //innvalidArg "n" "out of bounds"

let rec contains x L = 
  match L with
    | [] -> false
    | e::_ when e=x -> true
    | _::rest -> (contains x rest)

let _bikeCount data L =
  let e = getNth data 2
  let x = contains e L
  if x=true then 0 else 1 

let rec bikeCount data L = 
  match data with
    | [] -> 0
    | x::rest -> (_bikeCount x L)+(bikeCount rest ((getNth x 2)::L))
     
let _ridesForID data id = 
  let e = getNth data 2
  if e = id then 1 else 0
    
let rec ridesForID data id = 
  match data with
    | [] -> 0
    | e::rest -> (_ridesForID e id) + (ridesForID rest id)
    
let rec _timeSpent data id = 
  let e = getNth data 2
  let t = getNth data 3
  match data with
    | [] -> 0
    | e::rest when e=id -> t + _timeSpent rest id
    | _::rest -> 0 + _timeSpent rest id 
    
let rec timeSpent data id = 
  match data with
    | [] -> 0
    | e::rest -> (_timeSpent e id) + (timeSpent rest id)
    
let _avgTimeSpent data id = 
  let e = getNth data 2
  let t = float (getNth data 5)
  if e = id then t else (float 0)
    
let rec avgTimeSpent data id = 
  match data with
    | [] -> float 0
    | e::rest -> (_avgTimeSpent e id) + (avgTimeSpent rest id)

let _ridesForStation data id = 
  let e = getNth data 1
  if e = id then 1 else 0
    
let rec ridesForStation data id = 
  match data with
    | [] -> 0
    | e::rest -> (_ridesForStation e id) + (ridesForStation rest id)

let _avgTimeSpentStation data id = 
  let e = getNth data 1
  let t = float(getNth data 5)
  if e = id then t else (float 0)
    
let rec avgTimeSpentStation data id = 
  match data with
    | [] -> float 0
    | e::rest -> (_avgTimeSpentStation e id) + (avgTimeSpentStation rest id)

let _numTrips data d =
  let e = getNth data 4
  if e = d then 1 else 0

let rec numTrips data d = 
  match data with
    | [] -> 0
    | e::rest -> (_numTrips e d) + (numTrips rest d)

let rec printstars n = 
  match n with
    | 0 -> ()
    | 1 -> printf "*"
    | _ -> printf "*"
           printstars (n-1)

let rec _containsStation s L = 
  match L with
    | [] -> false
    | e::_ when (getNth e 0)=s -> true
    | _::rest -> (_containsStation s rest)    

let _countStations s rest L=
  if (contains s L) then L else L@[s]

let rec countStations data L = 
  match data with
    | [] -> L
    | e::rest -> (countStations rest (_countStations (getNth e 1) rest L))

let rec findNumTrips ridedata data L = 
  match data with
    | [] -> L
    | e::rest -> (findNumTrips ridedata rest (L@[ridesForStation ridedata e]))

let rec getNthList data n = 
  match n, data with
    | 0, (d::_) -> d
    | _, (_::data) -> getNthList data (n-1)
    | _, [] -> [] //innvalidArg "n" "out of bounds"

let rec map stats num i L= 
  match stats with
    | [] -> L
    | e::rest -> (map rest num (i+1) ([e; getNth num i]::L))

[<EntryPoint>]
let main argv =
  //
  // input file name, then input divvy ride data and build
  // a list of lists:
  //
  printf "filename> "
  let filename = System.Console.ReadLine()
  let contents = System.IO.File.ReadLines(filename)
  let ridedata = ParseInput contents

  //printfn "%A" ridedata
  let N = List.length ridedata
  printfn ""
  printfn "# of rides: %A" N
  printfn ""
  let B = bikeCount ridedata []
  printfn "# of bikes: %A" B
  printfn ""
  printf "BikeID> "
  let bikeString = System.Console.ReadLine()
  printfn ""
  let bikeID = bikeString |> int
  let C = ridesForID ridedata bikeID
  printfn "# of rides for BikeID %A: %A" bikeID C
  printfn ""
  let T = timeSpent ridedata bikeID
  let ATsum = avgTimeSpent ridedata bikeID
  let m = ATsum / (float 60)
  let s = ATsum % (float 60)
  printfn "Total time spent riding BikeID %A: %.0f minutes %.0f seconds" bikeID m s
  printfn ""
  let AT = ATsum / (float C)
  printfn "Average time spent riding BikeID %A: %.2f seconds" bikeID AT
  printfn ""
  printf "StationID> "
  let stationString = System.Console.ReadLine()
  printfn ""
  let stationID = stationString |> int
  let S = ridesForStation ridedata stationID
  printfn "# of rides to StationID %A: %A" stationID S
  printfn ""
  let ATSsum = avgTimeSpentStation ridedata stationID
  let ATS = ATSsum / (float S)
  printfn "Average time spent on trips leading to StationID %A: %.2f seconds" stationID ATS
  printfn ""
  
  let Sun = numTrips ridedata 0
  printfn "Number of Trips on Sunday: %A" Sun
  let Mon = numTrips ridedata 1
  printfn "Number of Trips on Monday: %A" Mon
  let Tues = numTrips ridedata 2
  printfn "Number of Trips on Tuesday: %A" Tues
  let Wed = numTrips ridedata 3
  printfn "Number of Trips on Wednesday: %A" Wed
  let Thurs = numTrips ridedata 4
  printfn "Number of Trips on Thursday: %A" Thurs
  let Fri = numTrips ridedata 5
  printfn "Number of Trips on Friday: %A" Fri
  let Sat = numTrips ridedata 6
  printfn "Number of Trips on Saturday: %A" Sat

  printfn ""
  printf "%A: " 0 
  printstars (Sun/10)
  printfn " %A" Sun
  printf "%A: " 1 
  printstars (Mon/10)
  printfn " %A" Mon
  printf "%A: " 2 
  printstars (Tues/10)
  printfn " %A" Tues
  printf "%A: " 3 
  printstars (Wed/10)
  printfn " %A" Wed
  printf "%A: " 4 
  printstars (Thurs/10)
  printfn " %A" Thurs
  printf "%A: " 5 
  printstars (Fri/10)
  printfn " %A" Fri
  printf "%A: " 6 
  printstars (Sat/10)
  printfn " %A" Sat
  
  let stats = countStations ridedata []
  let num = findNumTrips ridedata stats []
  let all = map stats num 0 []
  let ssmall = List.sortBy (fun [x;y] -> -y, x) all
  let top1 = getNthList ssmall 0
  printfn ""
  printfn "# of rides to station %A: %A" (getNth top1 0) (getNth top1 1)
  let top2 = getNthList ssmall 1
  printfn "# of rides to station %A: %A" (getNth top2 0) (getNth top2 1)
  let top3 = getNthList ssmall 2
  printfn "# of rides to station %A: %A" (getNth top3 0) (getNth top3 1)
  let top4 = getNthList ssmall 3
  printfn "# of rides to station %A: %A" (getNth top4 0) (getNth top4 1)
  let top5 = getNthList ssmall 4
  printfn "# of rides to station %A: %A" (getNth top5 0) (getNth top5 1)
  let top6 = getNthList ssmall 5
  printfn "# of rides to station %A: %A" (getNth top6 0) (getNth top6 1)
  let top7 = getNthList ssmall 6
  printfn "# of rides to station %A: %A" (getNth top7 0) (getNth top7 1)
  let top8 = getNthList ssmall 7
  printfn "# of rides to station %A: %A" (getNth top8 0) (getNth top8 1)
  let top9 = getNthList ssmall 8
  printfn "# of rides to station %A: %A" (getNth top9 0) (getNth top9 1)
  let top10 = getNthList ssmall 9
  printfn "# of rides to station %A: %A" (getNth top10 0) (getNth top10 1)
  printfn ""
  0 

module MonteCarlo

open System
type num = int

let get_random (gen: System.Random) = (gen.NextDouble(), gen)

let box_muller (gen: System.Random) =
    let (u1, g1) = get_random gen
    let (u2, g2) = get_random g1
    let r = sqrt (-2.0 * log u1)
    let theta = 2.0 * Math.PI * u2
    (r*sin theta, r*cos theta, g2)

let rec normal_random (gen: System.Random) = 
    let (r1, r2, gen2) = box_muller gen
    seq { r1; r2; yield! normal_random gen2 }

let rec path (step: int) (steps: int) (price: float) (drift: float) (vol: float) (years: float) (random: seq<float>) =
    let r = Seq.head random
    let n = float steps
    let t = years
    let new_price = price * exp ((drift - (vol*vol)/2.0 )*(t/n) + vol*sqrt(t/n)*r)
    match step with
    | 0 -> [price]
    | _ -> (price)::(path (step-1) steps new_price drift vol years (Seq.tail random))

let rec list_of_pairs (l: List<'a>) =
    match l with
    | [] -> []
    | [_] -> []
    | x::y::ls -> (x,y)::(list_of_pairs (y::ls))

let list_of_R l =
    let pairs = list_of_pairs l
    List.map (fun (a, b) -> log (b/a)) pairs

let volatility l t =
    let r = list_of_R l
    let n = float (List.length r)
    let avg_r = List.sum r / n
    sqrt ((n / (t*(n-1.0))) * List.sum (List.map (fun x -> (x-avg_r)*(x-avg_r)) r))

let rec get_seq_of_paths (steps: int) (price: float) (drift: float) (vol: float) (years: float) (random: System.Random) = 
    let rand = random.Next num.MaxValue
    let p = path steps steps price drift vol years (normal_random (new Random(rand)))
    seq { p; yield! (get_seq_of_paths steps price drift vol years random) }
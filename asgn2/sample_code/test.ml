
let rec trim_zero value = 
  let rvalue = List.rev value in
  match rvalue with
    | [] -> []
    | car1::cdr1 ->
      if car1 = 0 then trim_zero (List.rev cdr1)
      else value

(* Compares 2 lists of digits
       if list 1 > list 2 return 1
       if list 1 < list 2 return -1
       if list 1 = list2 return 0 *)                 
    let rec cmp list1 list2 = 
        let list1_rev = List.rev(trim_zero list1) in 
        let list2_rev = List.rev(trim_zero list2) in
        if (List.length list1_rev) > (List.length list2_rev) then 1
        else if (List.length list1_rev) < (List.length list2_rev) then -1
        else match (list1_rev, list2_rev) with
        | [], [] -> 0
        | _ , [] -> 1
        | [], _  -> -1
        | car1::cdr1, car2::cdr2 -> if car1 = car2 then cmp cdr1 cdr2
                                    else if car1 > car2 then 1
                                    else -1


(*let rec sub' list1 list2 carry = match (list1, list2, carry) with
        | list1, [], 0      -> list1
        | [], list2, 0      -> list2
        | list1, [], carry  -> sub' list1 [carry] 0
        | [], list2, carry  -> sub' [carry] list2 0
        | car1::cdr1, car2::cdr2, carry ->
          if (cmp car1 car2) = -1
          then sub' cdr1 cdr2 1
        else sub' cdr1 cdr2 0*)


let rec mul_helper list1 num carry = 
    match (list1, num, carry) with
    | [], num, carry -> [carry]
    | car1::cdr1, num, carry ->
      let prod = car1 * num + carry in
      prod mod 10 :: mul_helper cdr1 num (prod / 10)

let rec mul_help list1 num carry = 
    if list1 = [] then [carry]
    else let prd = (List.hd list1) * num + carry in
        (prd mod 10)::(mul_help (List.tl list1) num (prd / 10))
(*
let double number = number + number

let rec divrem' dividend powof2 divisor = match (dividend, powof2, divisor) with
| dividend, powof2, []      -> [], []
| [], powof2, divisor       -> [], []
| dividend, powof2, divisor ->
   if cmp divisor dividend > 0 then [0], dividend
   else let (quotient, remainder) = divrem' dividend (double powof2) (double divisor) in 
   if cmp remainder divisor < 0 then quotient, remainder
   else add' quotient powof2 0,  chomp_zeros (sub' remainder divisor 0)


let divrem dividend divisor = divrem' dividend [1] divisor
*)
(*
let div (Bigint (neg1, value1)) (Bigint (neg2, value2)) = 
    (* If same sign, quotient is positive else negative *)
    let sign = if neg1 = neg2 then Pos else Neg in
        let quotient, _ = divrem value1 value2 in Bigint (sign, quotient)
*)
(*
let rec divrem' (dividend, powerof2, divisor') =
    if divisor' > dividend
    then 0, dividend
    else let quotient, remainder =
             divrem' (dividend, double powerof2, double divisor')
         in  if remainder < divisor'
             then quotient, remainder
             else quotient + powerof2, remainder - divisor'

let divrem (dividend, divisor') = divrem' (dividend, 1, divisor')

let div (dividend, divisor) =
    let quotient, _ = divrem (dividend, divisor)
    in quotient

let rem (dividend, divisor) =
    let _, remainder = divrem (dividend, divisor)
    in remainder
*)
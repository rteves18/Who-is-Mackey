(* $Id: bigint.ml,v 1.5 2014-11-11 15:06:24-08 - - $ *)
(* Ryan Teves cruzid: rteves | Ky Nguyen cruzid: kymnguye*)

open Printf

module Bigint = struct

    type sign     = Pos | Neg
    type bigint   = Bigint of sign * int list
    let  radix    = 10
    let  radixlen =  1

    let car       = List.hd
    let cdr       = List.tl
    let map       = List.map
    let reverse   = List.rev
    let strcat    = String.concat
    let strlen    = String.length
    let strsub    = String.sub
    let zero      = Bigint (Pos, [])

    let charlist_of_string str = 
        let last = strlen str - 1
        in  let rec charlist pos result =
            if pos < 0
            then result
            else charlist (pos - 1) (str.[pos] :: result)
        in  charlist last []

    let bigint_of_string str =
        let len = strlen str
        in  let to_intlist first =
                let substr = strsub str first (len - first) in
                let digit char = int_of_char char - int_of_char '0' in
                map digit (reverse (charlist_of_string substr))
            in  if   len = 0
                then zero
                else if   str.[0] = '_'
                     then Bigint (Neg, to_intlist 1)
                     else Bigint (Pos, to_intlist 0)

    let string_of_bigint (Bigint (sign, value)) =
        match value with
        | []    -> "0"
        | value -> let reversed = reverse value
                   in  strcat ""
                       ((if sign = Pos then "" else "-") ::
                        (map string_of_int reversed))
   
    (* trims any leading 0 for absolute subtraction *)
    let rec trim_zero value = 
        let rvalue = reverse value in
        match rvalue with
        | [] -> []
        | car1::cdr1 ->
          if car1 = 0 then trim_zero (reverse cdr1)
          else value

    (* Compares 2 lists of digits
       First, compare length of 2 lists, then
       if list 1 > list 2 return 1
       if list 1 < list 2 return -1
       if list 1 = list2 reverse 2 list and compare hi order digits *)
    let cmp list1 list2 = 
        if (List.length list1) > (List.length list2) then 1
        else if (List.length list1) < (List.length list2) then -1
        else if (List.length list1) = (List.length list2) then 
            (let rev1 = reverse list1 in let rev2 = reverse list2 in
                let rec cmp_rec value1 value2 =
                    if value1 = [] && value2 = [] then 0
                    else if car value1 < car value2 then -1
                    else if car value1 > car value2 then 1
                    else cmp_rec (cdr value1) (cdr value2) in
                        cmp_rec rev1 rev2
            )
        else 0

    (* OLD_BROKEN
    let rec cmp list1 list2 = 
        let list1_rev = reverse(trim_zero list1) in 
        let list2_rev = reverse(trim_zero list2) in
        if (List.length list1_rev) > (List.length list2_rev) 
            then 1
        else if (List.length list1_rev) < (List.length list2_rev) 
            then -1
        else match (list1_rev, list2_rev) with
        | [], [] -> 0
        | _ , [] -> 1
        | [], _  -> -1
        | car1::cdr1, car2::cdr2 -> if car1 = car2 then cmp cdr1 cdr2
                                    else if car1 > car2 then 1
                                    else -1*)

    let rec add' list1 list2 carry = match (list1, list2, carry) with
        | list1, [], 0       -> list1
        | [], list2, 0       -> list2
        | list1, [], carry   -> add' list1 [carry] 0
        | [], list2, carry   -> add' [carry] list2 0
        | car1::cdr1, car2::cdr2, carry ->
          let sum = car1 + car2 + carry
          in  sum mod radix :: add' cdr1 cdr2 (sum / radix)

    let rec sub' list1 list2 carry = match (list1, list2, carry) with
        | list1, [], 0      -> list1
        | [], list2, 0      -> list2
        | list1, [], carry  -> sub' list1 [carry] 0
        | [], list2, carry  -> sub' [carry] list2 0
        | car1::cdr1, car2::cdr2, carry ->
          if car1 - carry < car2 then
            let diff = car1 + 10 - car2 - carry
            in diff mod radix :: sub' cdr1 cdr2 1
          else 
             let diff = car1 - car2 - carry
             in diff mod radix :: sub' cdr1 cdr2 (diff / radix)

    (* Multiply helper function
       takes in a list of digits, a number and a carry
       multiplies the number to the list of digits
       returns the new list of digits
     *)
    let rec mul_helper list1 num carry = 
        match (list1, num, carry) with
        | [], num, carry -> [carry]
        | car1::cdr1, num, carry ->
          let prod = car1 * num + carry in
          prod mod radix :: mul_helper cdr1 num (prod / radix)

    let rec mul' list1 list2 =
        if (list1 = [] || list2 = []) then []
        else add' (mul_helper list1 (car list2) 0)
                    (0::(mul' list1 (cdr list2))) 0

    let double number = add' number number 0

    let rec divrem' dividend powof2 divisor = 
       if cmp divisor dividend > 0 then [0], dividend
       else let quotient, remainder = 
        divrem' dividend (double powof2) (double divisor) in 
            if cmp remainder divisor < 0 then quotient, remainder
            else (add' quotient powof2 0, 
                 trim_zero (sub' remainder divisor 0))

    let divrem dividend divisor = divrem' dividend [1] divisor

    let rec pow' list1 list2 list1' =
        let list2' = (trim_zero (sub' list2 [1] 0)) in
            if list2' = [] then list1
            else pow' (mul' list1 list1') list2' list1'

    let add (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        if neg1 = neg2
            then Bigint (neg1, add' value1 value2 0)
        else let strcmp = cmp value1 value2 in
        if strcmp < 0 
            then Bigint(neg2, trim_zero(sub' value2 value1 0))
        else if strcmp > 0 
            then Bigint(neg1, trim_zero(sub' value1 value2 0))
        else zero

    let sub (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        (* if the sign of both values are the same *)
        if neg1 = neg2
            then let strcmp = cmp value1 value2 in
            (* if value1 > value2 *)
            if strcmp > 0
                then Bigint (neg1, trim_zero(sub' value1 value2 0))
            (* if value1 < value2 *)
            else if strcmp < 0 then
                (* if both values are positive *)
                if neg1 = Pos 
                    then Bigint (Neg, trim_zero(sub' value2 value1 0))
                (* if both values are negative *)
                else if neg1 = Neg
                    (* perform regular addition & add - sign in front *)
                    then Bigint (Pos, trim_zero(sub' value2 value1 0))
                else zero
            else zero
        (* if both values have different sign *)
        else Bigint (neg1, add' value1 value2 0)

    let mul (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        if neg1 = neg2 
            then Bigint (Pos, trim_zero (mul' value1 value2))
        else Bigint(Neg, trim_zero(mul' value1 value2))

    let div (Bigint (neg1, value1)) (Bigint (neg2, value2)) = 
    (* If same sign, quotient is positive else negative *)
        let sign = if neg1 = neg2 then Pos else Neg in
            let quotient, _ = 
            divrem value1 value2 in Bigint (sign, quotient)

    let rem (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        (* If same sign, quotient is positive else negative *)
        let sign = if neg1 = neg2 then Pos else Neg in
            let _, remainder = divrem value1 value2 
                in Bigint (sign, remainder)

    let pow (Bigint (neg1, value1)) (Bigint (neg2, value2)) = 
        if neg2 = Neg then zero
        else match (value1, value2) with
            | [], value2        -> zero (* 0^anything=0 *)
            | value1, []        -> Bigint (Pos, [1]) (* anything^0=1 *)
            | value1, value2    -> 
                Bigint (neg1, (trim_zero (pow' value1 value2 value1)))

end

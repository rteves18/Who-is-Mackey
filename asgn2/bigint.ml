(* $Id: bigint.ml,v 1.5 2014-11-11 15:06:24-08 - - $ *)

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

    (* Compares 2 lists of digits
       First, compare length of 2 lists, then
       if list 1 > list 2 return 1
       if list 1 < list 2 return -1
       if list 1 = list2 return 0 *)                 
    let rec cmp list1 list2 = 
        if (List.length list1) > (List.length list2) then 1
        else if (List.length list1) < (List.length list2) then -1
        else match (reverse(list1), reverse(list2)) with
        | [], [] -> 0
        | _ , [] -> 1
        | [], _  -> -1
        | car1::cdr1, car2::cdr2 -> if car1 = car2 then cmp cdr1 cdr2
                                    else if car1 > car2 then 1
                                    else -1

    let rec add' list1 list2 carry = match (list1, list2, carry) with
        | list1, [], 0       -> list1
        | [], list2, 0       -> list2
        | list1, [], carry   -> add' list1 [carry] 0
        | [], list2, carry   -> add' [carry] list2 0
        | car1::cdr1, car2::cdr2, carry ->
          let sum = car1 + car2 + carry
          in  sum mod radix :: add' cdr1 cdr2 (sum / radix)

    let add (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        if neg1 = neg2
            then Bigint (neg1, add' value1 value2 0)
        else zero

    let rec sub' list1 list2 carry = match (list1, list2, carry) with
        | list1, [], 0      -> list1
        | [], list2, 0      -> list2
        | list1, [], carry  -> sub' list1 [carry] 0
        | [], list2, carry  -> sub' [carry] list2 0
        | car1::cdr1, car2::cdr2, carry ->
          let diff = car1 - car2 - carry
          in  diff mod radix :: sub' cdr1 cdr2 (diff / radix)
          (*if cmp car1 car2 = -1 
            then let diff = car1 + radix - car2 
                  in diff :: sub' cdr1 cdr2 1
          else let diff = car1 - car2 - carry
                in diff mod radix :: sub' cdr1 cdr2 (diff / radix)*)

    let sub (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        if neg1 = neg2
            then (if (cmp value1 value2) = 1
                  then Bigint (neg1, sub' value1 value2 0)
                  else Bigint (Neg, sub' value2 value1 0))
        else zero

    let mul = add

    let div = add

    let rem = add

    let pow = add

end


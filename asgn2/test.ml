(* Compares 2 lists of digits
       if list 1 > list 2 return 1
       if list 1 < list 2 return -1
       if list 1 = list2 return 0 *)                 
    let rec cmp list1 list2 = 
        if (List.length list1) > (List.length list2) then 1
        else if (List.length list1) < (List.length list2) then -1
        else match (List.rev(list1), List.rev(list2)) with
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
let rec cmp list1 list2 = match (List.rev(list1), List.rev(list2)) with
  | [], [] -> 0
  | _, [] -> 1
  | [], _ -> -1
  | car1::cdr1, car2::cdr2 -> if car1 = car2 then cmp cdr1 cdr2
                              else if car1 > car2 then 1
                              else -1

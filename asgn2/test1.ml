 let rec sub' list1 list2 carry = match (list1, list2, carry) with
          | list1, [], 0      -> list1
          | [], list2, 0      -> list2
          | list1, [], carry  -> sub' list1 [carry] 0
          | [], list2, carry  -> sub' [carry] list2 0
          | car1::cdr1, car2::cdr2, carry ->
            if (cmp car1 car2) = -1
              then let diff = car1 + 10 - car2
                    in diff :: sub' cdr1 cdr2 1
            else (let diff = car1 - car2 - carry
                  in diff mod radix :: sub' cdr1 cdr2 (diff / 10))

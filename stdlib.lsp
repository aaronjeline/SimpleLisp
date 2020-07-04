(begin 
    (define foldr 
        (fun (f b lst)
         (if lst
             (f (car lst) (foldr f b (cdr lst)))
             b)))
    (define sum
        (fun (xs) (foldr + 0 xs)))

    (define map
        (fun (f xs)
            (if xs
                (cons (f (car xs)) (map f (cdr xs)))
                '())))

    (define add1 (fun (x) (+ x 1)))

    (define length (fun (x)
        (if x
            (+ 1 (length (cdr x)))
            0))))


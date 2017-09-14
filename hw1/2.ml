let rec sigma (a, b, f) : int = 
    match (a ,b) with
    | (x, y) when x = y -> f x
    | (x, y) when x > y -> 0
    | (x, y) -> f x + sigma (x + 1, y, f)

(*let a21 = sigma (0, 3, function x -> x)
let a22 = sigma (3, 3, function x -> x)
let a23 = sigma (7, 10, function x -> x*x)
let a24 = sigma (11, 10, function x -> x*x)

let _ = print_int(a21)
let _ = print_endline("")
let _ = print_int(a22)
let _ = print_endline("")
let _ = print_int(a23)
let _ = print_endline("")
let _ = print_int(a24)
let _ = print_endline("")*)
let _ =
    let assert_equal (expected: int) (actual: int) =
            if expected = actual then print_endline "true"
                    else Printf.printf "Expected %d but actual %d\n" expected actual
                        in
                            let test_sigma (a: int) (b: int) (f: int->int) (expected: int) =
                                    sigma (a,b,f) |> assert_equal expected
                                        in
                                            test_sigma 1 2 (fun x -> x * x) 5;
                                                test_sigma 1 100 (fun x -> x) 5050;
                                                    test_sigma 1 1 (fun x -> x) 1;
                                                        test_sigma 1 0 (fun x -> x) 0;
                                                            test_sigma 101 200 (fun x -> -x) (-15050);

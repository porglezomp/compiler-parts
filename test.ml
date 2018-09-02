let () =
    let intervals: int Conflict.interval list = [
        { label = 0; start =  0; finish = 15 };
        { label = 1; start =  5; finish = 10 };
        { label = 2; start = 13; finish = 20 };
    ] in
    print_endline (Conflict.svg string_of_int intervals)

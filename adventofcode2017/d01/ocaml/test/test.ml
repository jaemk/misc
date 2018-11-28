open OUnit2


let suite =
    "suite">:::
    (List.map
        (fun (func, input, res) ->
            let name = Lib.Main.string_of_list input "%d" in
            name >::
            (fun _ ->
                assert_equal (func input) res))
        [(Lib.Main.checksum_a, [1; 1; 2; 2], 4);
         (Lib.Main.checksum_a, [1; 1; 1; 1], 4);
         (Lib.Main.checksum_a, [1; 2; 3; 4], 1);
         (Lib.Main.checksum_a, [9; 1; 2; 1; 2; 1; 2; 9], 9);
        ])
    @
    (List.map
        (fun (func, input, res) ->
            let name = Lib.Main.string_of_list input "%d" in
            name >::
            (fun _ ->
                assert_equal (func input) res))
        [(Lib.Main.checksum_b, [1; 2; 1; 2], 6);
         (Lib.Main.checksum_b, [1; 2; 2; 1], 0);
         (Lib.Main.checksum_b, [1; 2; 3; 4; 2; 5], 4);
         (Lib.Main.checksum_b, [1; 2; 3; 1; 2; 3], 12);
         (Lib.Main.checksum_b, [1; 2; 1; 3; 1; 4; 1; 5], 4);
         ])


let () =
    run_test_tt_main suite

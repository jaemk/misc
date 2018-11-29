open OUnit2


let suite =
    "suite">:::
    (List.map
        (fun (func, input, res) ->
            let name = Advent.Utils.string_of_list input "%d" in
            name >::
            (fun _ ->
                assert_equal (func input) res))
        [(Advent.D02.checksum_a, [5; 1; 9; 5], 8);
         (Advent.D02.checksum_a, [7; 5; 3], 4);
         (Advent.D02.checksum_a, [2; 4; 6; 8], 6);
        ])
    @
    ["a">:: (fun _ -> assert_equal 0 0)]


let () =
    run_test_tt_main suite

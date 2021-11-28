let () = Random.self_init ()
let random_char () = char_of_int (Random.int 256)
let random_string n = String.init n (fun _i -> random_char ())

module StringSet = Set.Make (String)

let expected_error_rate = 0.001

let create_and_fill size =
  let bf = Bloomf.create ~error_rate:expected_error_rate size in
  let rec loop i acc =
    if i = 0 then acc
    else
      let r = random_string 1024 in
      let () = Bloomf.add bf r in
      loop (i - 1) (StringSet.add r acc)
  in
  let elts = loop size StringSet.empty in
  (bf, elts)

let test_mem () =
  let sizes = [ 1_000; 10_000; 100_000 ] in
  List.iter
    (fun i ->
      let bf, elts = create_and_fill i in
      StringSet.iter
        (fun r -> Alcotest.(check bool) "mem (empty)" true (Bloomf.mem bf r))
        elts)
    sizes

let test_errors () =
  let sizes = [ 1_000; 10_000; 100_000 ] in
  List.iter
    (fun i ->
      let bf, elts = create_and_fill i in
      let attempts = 100_000 in
      let rec loop i count =
        if i = 0 then count
        else
          let r = random_string 1024 in
          if StringSet.mem r elts then loop i count
          else loop (i - 1) (if Bloomf.mem bf r then count + 1 else count)
      in
      let count = loop attempts 0 in
      let error_rate = float_of_int count /. float_of_int attempts in
      if error_rate > 1.15 *. expected_error_rate then
        Alcotest.failf "error_rate: expecting@\n%f, got@\n%f"
          expected_error_rate error_rate
      else ())
    sizes

let test_size () =
  let sizes = [ 1_000; 10_000; 100_000 ] in
  List.iter
    (fun i ->
      let bf, _ = create_and_fill i in
      let len = Bloomf.size_estimate bf in
      if abs (len - i) > int_of_float (0.15 *. float_of_int i) then
        Alcotest.failf "size_estimate: expecting@\n%d, got@\n%d" i len)
    sizes

let test_op msg bop sop =
  let sizes = [ 1_000; 10_000; 100_000 ] in
  List.iter
    (fun i ->
      let bf1, elts1 = create_and_fill i in
      let bf2, elts2 = create_and_fill i in
      let bf3 = bop bf1 bf2 in
      let elts3 = sop elts1 elts2 in
      StringSet.iter
        (fun r -> Alcotest.(check bool) msg true (Bloomf.mem bf3 r))
        elts3)
    sizes

let test_union () = test_op "union" Bloomf.union StringSet.union
let test_inter () = test_op "intersection" Bloomf.inter StringSet.inter

let test_bytes () =
  let sizes = [ 1_000; 10_000; 100_000 ] in
  List.iter
    (fun i ->
      let bf1, _ = create_and_fill i in
      match Bloomf.to_bytes bf1 |> Bloomf.of_bytes with
      | Ok bf2 ->
          Alcotest.(check bool)
            "serialisation / deserialisation" true (bf1 = bf2)
      | Error _ -> Alcotest.failf "deserialisation failed")
    sizes

let suite =
  [
    ("Mem returns true when element was added", `Quick, test_mem);
    ( "False positive rate is as specified (15% error allowed)",
      `Slow,
      test_errors );
    ("Size estimate is correct", `Slow, test_size);
    ("Union", `Quick, test_union);
    ("Intersection", `Quick, test_inter);
    ("Serialisation", `Quick, test_bytes);
  ]

let () = Alcotest.run "Bloomf" [ ("bloomf", suite) ]

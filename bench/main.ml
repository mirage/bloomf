open Bechamel
open Toolkit

let () = Random.self_init ()

let random_char () = char_of_int (Random.int 256)

let random_string n = String.init n (fun _i -> random_char ())

let create size = Staged.stage (fun () -> Bloomf.create size)

let add size =
  let bf = Bloomf.create size in
  let r = random_string 1024 in
  Staged.stage (fun () -> Bloomf.add bf r)

let fill_bf bf n =
  let rec loop i =
    if i = 0 then ()
    else
      let r = random_string 1024 in
      let () = Bloomf.add bf r in
      loop (i - 1)
  in
  loop n

let mem_absent size =
  let bf = Bloomf.create size in
  let () = fill_bf bf size in
  let r = random_string 1024 in
  Staged.stage (fun () -> ignore (Bloomf.mem bf r))

let mem_present size =
  let bf = Bloomf.create size in
  let () = fill_bf bf size in
  let r = random_string 1024 in
  let () = Bloomf.add bf r in
  Staged.stage (fun () -> ignore (Bloomf.mem bf r))

let size_estimate size =
  let bf = Bloomf.create size in
  let () = fill_bf bf size in
  Staged.stage (fun () -> ignore (Bloomf.size_estimate bf))

let test =
  Test.make_grouped ~name:"bloomf"
    [
      Test.make_indexed ~name:"create" ~fmt:"%s %d"
        ~args:[ 10_000; 100_000; 1_000_000 ]
        create;
      Test.make_indexed ~name:"add" ~fmt:"%s %d"
        ~args:[ 10_000; 100_000; 1_000_000 ]
        add;
      Test.make_indexed ~name:"mem (absent)" ~fmt:"%s %d"
        ~args:[ 10_000; 100_000; 1_000_000 ]
        mem_absent;
      Test.make_indexed ~name:"mem (present)" ~fmt:"%s %d"
        ~args:[ 10_000; 100_000; 1_000_000 ]
        mem_present;
      Test.make_indexed ~name:"size_estimate" ~fmt:"%s %d"
        ~args:[ 10_000; 100_000; 1_000_000 ]
        size_estimate;
    ]

let benchmark () =
  let config = Benchmark.(cfg ~limit:100 ~quota:(Time.millisecond 100.)) () in
  let ols =
    Analyze.ols ~bootstrap:0 ~r_square:true ~predictors:Measure.[| run |]
  in
  let instances =
    Instance.[ minor_allocated; major_allocated; monotonic_clock ]
  in
  let raw_results = Benchmark.all config instances test in
  List.map (fun instance -> Analyze.all ols instance raw_results) instances
  |> Analyze.merge ols instances

let () = Bechamel_notty.Unit.add Instance.monotonic_clock "ns"

let () = Bechamel_notty.Unit.add Instance.minor_allocated "w"

let () = Bechamel_notty.Unit.add Instance.major_allocated "mw"

let img (window, results) =
  Bechamel_notty.Multiple.image_of_ols_results ~rect:window
    ~predictor:Measure.run results

open Notty_unix

let rect w h = Bechamel_notty.{ w; h }

let () =
  let window =
    match winsize Unix.stdout with
    | Some (_, _) -> Bechamel_notty.{ w = 80; h = 1 }
    | None -> { w = 80; h = 1 }
  in
  img (window, benchmark ()) |> eol |> output_image

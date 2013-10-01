
type t1 = A | B of int | C | D of int * int | E of (int * int) deriving (Json, Typerepr)
type t2 = { x : int ; y : string } deriving (Json, Typerepr)
type t3 = t1 list * float deriving (Json, Typerepr)
type ('a, 'b, 'c) t4 = ('a * 'b * 'c) option deriving (Json, Typerepr)
type t5 = [ `A | `B of int | `C of int * int | `D of (int * int) | `E ] deriving (Json, Typerepr)
type t = (t1, t2, t3) t4 deriving (Json, Typerepr)

let () =
  let test t json x =
    let json' = Eliom_form_generator.json_of_typerepr t in
    let str = Deriving_Json.to_string json' x in
    Printf.ksprintf (fun s -> Ocsigen_messages.console (fun () -> s))
      "test %s: %S" (Deriving_Typerepr.show t x) str;
    assert (str = Deriving_Json.to_string json x);
    assert (Deriving_Json.from_string json' str
              = Deriving_Json.from_string json str);
  in
  test Typerepr.t<t1 list> Json.t<t1 list> [];
  test Typerepr.t<t1 list> Json.t<t1 list> [A; B 3; C; D (1,2)];
  test Typerepr.t<t1 option> Json.t<t1 option> None;
  test Typerepr.t<t1 option> Json.t<t1 option> (Some (B 3));
  test Typerepr.t<t1> Json.t<t1> A;
  test Typerepr.t<t1> Json.t<t1> (D (1,2));
  test Typerepr.t<t1> Json.t<t1> (E (1,2));
  test Typerepr.t<t1> Json.t<t1> (B 3);
  test Typerepr.t<t2> Json.t<t2> { x = 100; y = "abc" };
  test Typerepr.t<t2 * int> Json.t<t2 * int> ({ x = 100; y = "abc" }, 45);
  test Typerepr.t<t3> Json.t<t3> ([], 123.456);
  test Typerepr.t<t3> Json.t<t3> ([A; B 3; C; D (1,2)], 123.456);
  test Typerepr.t<t1 * t2 * t3> Json.t<t1 * t2 * t3> (A, {x=2; y="abc"}, ([A;B 1;C; D (1,2)], 2.3));
  test Typerepr.t<int option> Json.t<int option> (Some 3);
  test Typerepr.t<t5> Json.t<t5> `A;
  test Typerepr.t<t5> Json.t<t5> (`B 1);
  test Typerepr.t<t5> Json.t<t5> (`C (2, 3));
  test Typerepr.t<t5> Json.t<t5> (`D (2, 3));
  test Typerepr.t<t5> Json.t<t5> `E;
  let test = test Typerepr.t<t> Json.t<t> in
  test None;
  test (Some (A, {x=2; y="abc"}, ([A;C;B 3], 123.456)));
  ()

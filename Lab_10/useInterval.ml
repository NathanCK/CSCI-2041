open Intervals
open IntInterval
open FloatInterval
open RationalInterval

let i1 = Int_interval.create 5 10

let f1 = Float_interval.create 7.5 15.2

let r1 = Rational_interval.create (5,8) (3,6)
let r2 = Rational_interval.create (4,8) (16,3)
let r3 = Rational_interval.create (2,18) (4,1)
let r4 = Rational_interval.create (3,7) (5,3)
let r5 = Rational_interval.create (15, 6) (8, 2)

let () =
  print_endline ("The r5 is : " ^ Rational_interval.to_string r5 ^ "\n");
  print_endline ("This is an interval : " ^
                    Int_interval.to_string i1);
  print_endline ("This is an empty interval : " ^
                    Rational_interval.to_string r1 ^ "\n");

  print_endline ("This is an interval : " ^
                    Float_interval.to_string f1);
  print_endline ("Does this interval contain 11.2? " ^
                    (if Float_interval.contains f1 11.2
                     then "yes"
                     else "no"));
  print_endline ("Does this interval contain 7? " ^
                   (if Float_interval.contains f1 7.0
                    then "yes"
                    else "no") ^ "\n");

  print_endline ("Here is an interval : " ^
                    Rational_interval.to_string r2);
  print_endline ("Here is another interval : " ^
                    Rational_interval.to_string r3);
  print_endline ("Here is a third interval : " ^
                    Rational_interval.to_string r4);
  print_endline ("This is the intersection of these three intervals : " ^
                    Rational_interval.to_string (
                      Rational_interval.intersect r2
                        (Rational_interval.intersect r3 r4)));
  print_endline ("Intersection is associative so this gives the same result : " ^
                    Rational_interval.to_string (
                      Rational_interval.intersect
                        (Rational_interval.intersect r2 r3) r4) ^ "\n");
  (* now some additional tests using asserts. *)
  assert ((Float_interval.to_string (Float_interval.create (-3.2) (-5.8))) =
            "Empty");
  assert (Float_interval.to_string (Float_interval.create 5.33 5.33) =
            "(5.33, 5.33)");
  assert (Float_interval.intersect (Float_interval.create (-5.2) (-3.1))
                                   (Float_interval.create (-7.424) (-3.333333)) =
            Float_interval.create (-5.2) (-3.333333));
  assert (not (Float_interval.contains (Float_interval.create 3.62 4.2) 4.22));


  assert (Rational_interval.to_string (Rational_interval.create (19, 7) (22, 7)) =
            "(19/7, 22/7)");
  assert (Rational_interval.to_string (Rational_interval.create (15, 6) (8, 2)) =
            "(5/2, 4/1)");
  assert (Rational_interval.contains (Rational_interval.create (2, 3) (15, 7)) (6, 5));
  assert (not (Rational_interval.contains (Rational_interval.create (2, 3) (15, 7)) (2, 8)));
  assert (Rational_interval.intersect (Rational_interval.create ((-2), 1) (1, 1))
                                      (Rational_interval.create ((-5), 3) (3, 2)) =
            Rational_interval.create ((-5), 3) (1, 1));
  assert (Rational_interval.to_string
            (Rational_interval.intersect (Rational_interval.create ((-5), 2) (6, 10))
                                         (Rational_interval.create (5, 7) ((-3), 5))) =
            "Empty");
  print_endline ("Match Done!")

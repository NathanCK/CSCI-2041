open LazeeModules
open StreamModules
open Hwk5Modules

module Stream_Lazy = Stream(Lazee_v1)
module Stream_Slow = Stream(Lazee_v2)

module Hwk5_Lazy = Hwk5( Stream_Lazy )
module Hwk5_Slow = Hwk5( Stream_Slow )

module Hwk6_Test (Hwk5: Hwk5Sig) = struct
  let () =
    assert (Hwk5.take 4 (Hwk5.nats) = [1; 2; 3; 4]);
    assert (Hwk5.take 10 (Hwk5.nats) = [1; 2; 3; 4; 5; 6; 7; 8; 9; 10]);
    assert (Hwk5.take 4 (Hwk5.cubes_from 1) = [1; 8; 27; 64]);
    assert (Hwk5.take 4 (Hwk5.cubes_from_zip 1) = [1; 8; 27; 64]);
    assert (Hwk5.take 4 (Hwk5.cubes_from_map 1) = [1; 8; 27; 64]);
    assert (Hwk5.take 2 (Hwk5.drop 3 (Hwk5.cubes_from 3 ) ) = [ 216; 343 ] );
    assert (Hwk5.head (Hwk5.drop_until (fun v -> v > 35) Hwk5.nats) = 36 ) ;
    assert (Hwk5.sum_positive_prefix (
                Hwk5.zip ( - ) (Hwk5.from 1000) (Hwk5.cubes_from 1)) = 7020);
    assert (Hwk5.take 10 Hwk5.primes = [2; 3; 5; 7; 11; 13; 17; 19; 23; 29])
end

module Test_Lazy = Hwk6_Test(Hwk5_Lazy)
module Test_Slow = Hwk6_Test(Hwk5_Slow)

let () =
  print_endline ("Success, all tests passed.")

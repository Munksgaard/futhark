-- Iota with an unsigned type where zero-extension is important.
--
-- ==
-- input { 128u8}
-- output { [0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8, 8i8, 9i8, 10i8, 11i8, 12i8, 13i8, 14i8,
--           15i8, 16i8, 17i8, 18i8, 19i8, 20i8, 21i8, 22i8, 23i8, 24i8, 25i8, 26i8, 27i8,
--           28i8, 29i8, 30i8, 31i8, 32i8, 33i8, 34i8, 35i8, 36i8, 37i8, 38i8, 39i8, 40i8,
--           41i8, 42i8, 43i8, 44i8, 45i8, 46i8, 47i8, 48i8, 49i8, 50i8, 51i8, 52i8, 53i8,
--           54i8, 55i8, 56i8, 57i8, 58i8, 59i8, 60i8, 61i8, 62i8, 63i8, 64i8, 65i8, 66i8,
--           67i8, 68i8, 69i8, 70i8, 71i8, 72i8, 73i8, 74i8, 75i8, 76i8, 77i8, 78i8, 79i8,
--           80i8, 81i8, 82i8, 83i8, 84i8, 85i8, 86i8, 87i8, 88i8, 89i8, 90i8, 91i8, 92i8,
--           93i8, 94i8, 95i8, 96i8, 97i8, 98i8, 99i8, 100i8, 101i8, 102i8, 103i8, 104i8,
--           105i8, 106i8, 107i8, 108i8, 109i8, 110i8, 111i8, 112i8, 113i8, 114i8, 115i8,
--           116i8, 117i8, 118i8, 119i8, 120i8, 121i8, 122i8, 123i8, 124i8, 125i8, 126i8,
--           127i8] }

import "/futlib/math"


let main(n: u8): []u8 =
  u8.iota n

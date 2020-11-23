let popcnt x =
  let x = (x land 0x5555555555555555) + ((x lsr 1) land 0x5555555555555555) in
  let x = (x land 0x3333333333333333) + ((x lsr 2) land 0x3333333333333333) in
  let x = (x land 0x0f0f0f0f0f0f0f0f) + ((x lsr 4) land 0x0f0f0f0f0f0f0f0f) in
  let x = (x land 0x00ff00ff00ff00ff) + ((x lsr 8) land 0x00ff00ff00ff00ff) in
  let x = (x land 0x0000ffff0000ffff) + ((x lsr 16) land 0x0000ffff0000ffff) in
  (x land 0x00000000ffffffff) + ((x lsr 32) land 0x00000000ffffffff)

let ceil_log2 x =
  let x = x - 1 in
  let x = x lor (x lsr 1) in
  let x = x lor (x lsr 2) in
  let x = x lor (x lsr 4) in
  let x = x lor (x lsr 8) in
  let x = x lor (x lsr 16) in
  let x = x lor (x lsr 32) in
  popcnt x

let floor_log2 x =
  let x = x lor (x lsr 1) in
  let x = x lor (x lsr 2) in
  let x = x lor (x lsr 4) in
  let x = x lor (x lsr 8) in
  let x = x lor (x lsr 16) in
  let x = x lor (x lsr 32) in
  popcnt x - 1

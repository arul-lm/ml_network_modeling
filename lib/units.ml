let kilo_b = Int64.of_int 1024
let mega_b = Int64.(mul kilo_b kilo_b)
let giga_b = Int64.(mul kilo_b mega_b)
let tera_b = Int64.(mul kilo_b giga_b)
let peta_b = Int64.(mul kilo_b tera_b)
let milli = Float.pow 10. (-3.)
let micro = milli *. milli
let nano = milli *. micro

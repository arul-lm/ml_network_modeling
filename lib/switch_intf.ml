module type Switch = sig
  val name : string
end

module Rail : Switch = struct
  let name = "rail"
end

type 'a switch_data = { id : int }

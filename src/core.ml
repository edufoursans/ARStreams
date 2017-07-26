module type Streamable = sig
    type t
    val extends : t -> t -> bool
end ;;

module type StreamableTree = sig
  type t
  type element
  val add : element -> t -> t
  val create_from_list : element list -> t
end ;;

module MakeStreamableTree(T : Streamable) : (StreamableTree with type element := T.t) = struct
  type t = (element * t list) list
  let add e tree = match tree with
    |[] -> [Node(e,[])]
    |no::r when no = Node(n,desc) ->
      match T.extends e n, T.extends n e with
      |true,true -> tree
      |true,false -> Node(n,add e desc)::r
      |false,true -> Node(e,[no])::r
      |false,false -> no::(add e r)


end ;;
module type ARStream = sig
  type t
  type element
  type tree
  val create_empty : t
  val create_from_tree : tree -> t
  val or_combine : t -> t -> t
  val and_combine : t -> t -> t
end ;;


module MakeARStream(T : Streamable)
      : (ARStream with type element = T.t and type tree = MakeStreamableTree T)  = struct
    type t =T.t -> (
                    | Endofstream
                    | Timeout of t
                    | Cons of T.t * t
      )

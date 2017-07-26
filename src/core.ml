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
  type element = T.t
  type t = Tree of (element * t) list

  let rec add e tree = match tree with
    | Tree([]) -> Tree([(e,Tree([]))])
    | Tree(no::r) ->
       let n, desc = no in
       begin
       match T.extends e n, T.extends n e with
       | true, true -> tree
       | true, false ->
          let Tree(l) = add e desc in
          (Tree((n,l)::r))
       | false, true -> Tree((e,Tree([no]))::r)
       | false, false -> Tree(no::(add e Tree(r)))
       end

  let rec create_from_list l = match l with
    | [] -> []
    | hd::tl ->
       let tree = create_from_list tl in
       add hd tl

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

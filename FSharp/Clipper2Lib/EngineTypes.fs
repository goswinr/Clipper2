#if USINGZ
namespace Clipper2ZLib
#else
namespace Clipper2Lib
#endif

open System
open System.Collections
open System.Collections.Generic
open System.Runtime.CompilerServices

// Enums
[<Flags>]
type internal VertexFlags =
    | None = 0
    | OpenStart = 1
    | OpenEnd = 2
    | LocalMax = 4
    | LocalMin = 8

type internal JoinWith =
    | None = 0
    | Left = 1
    | Right = 2

type internal HorzPosition =
    | Bottom = 0
    | Middle = 1
    | Top = 2

// PolyPathBase - abstract base declared early so OutRec can reference it
[<AbstractClass; AllowNullLiteral>]
type PolyPathBase(parent: PolyPathBase) =

    internal new() = PolyPathBase(null)

    member val internal _parent: PolyPathBase = parent with get, set
    member val internal _childs: List<PolyPathBase> = List<PolyPathBase>() with get

    member this.IsHole =
        let lvl = this.Level
        lvl <> 0 && (lvl &&& 1) = 0

    member this.Level =
        let mutable result = 0
        let mutable pp = this._parent
        while not (isNull pp) do
            result <- result + 1
            pp <- pp._parent
        result

    member this.Count = this._childs.Count

    abstract AddChild: Path64 -> PolyPathBase

    member this.Clear() = this._childs.Clear()

    member internal this.ToStringInternal(idx: int, level: int) : string =
        let mutable result = ""
        let mutable padding = ""
        let mutable plural = "s"
        if this._childs.Count = 1 then plural <- ""
        padding <- padding.PadLeft(level * 2)
        if (level &&& 1) = 0 then
            result <- result + sprintf "%s+- hole (%d) contains %d nested polygon%s.\n" padding idx this._childs.Count plural
        else
            result <- result + sprintf "%s+- polygon (%d) contains %d hole%s.\n" padding idx this._childs.Count plural
        let mutable i = 0
        while i < this.Count do
            if this._childs.[i].Count > 0 then
                result <- result + this._childs.[i].ToStringInternal(i, level + 1)
            i <- i + 1
        result

    override this.ToString() : string =
        if this.Level > 0 then "" // only accept tree root
        else
            let mutable plural = "s"
            if this._childs.Count = 1 then plural <- ""
            let mutable result = sprintf "Polytree with %d polygon%s.\n" this._childs.Count plural
            let mutable i = 0
            while i < this.Count do
                if this._childs.[i].Count > 0 then
                    result <- result + this._childs.[i].ToStringInternal(i, 1)
                i <- i + 1
            result + "\n"

    interface IEnumerable with
        member this.GetEnumerator() : IEnumerator =
            (new NodeEnumerator(this._childs) :> IEnumerator)

and private NodeEnumerator(nodes: List<PolyPathBase>) =
    let _nodes = List<PolyPathBase>(nodes)
    let mutable _position = -1

    // NodeEnumerator interface members (MoveNext, Reset) can't use inline.
    interface IEnumerator with
        [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
        member _.MoveNext() =
            _position <- _position + 1
            _position < _nodes.Count

        [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
        member _.Reset() =
            _position <- -1

        member _.Current =
            if _position < 0 || _position >= _nodes.Count then
                raise (InvalidOperationException())
            _nodes.[_position] :> obj

// === MUTUAL GROUP: core linked-list node graph ===
[<AllowNullLiteral>]
type internal Vertex(pt: Point64, flags: VertexFlags, prev: Vertex) =
    member val pt = pt with get, set
    member val next: Vertex = null with get, set
    member val prev: Vertex = prev with get, set
    member val flags = flags with get, set

and [<Struct; CustomEquality; NoComparison>] internal LocalMinima =
    val vertex: Vertex
    val polytype: PathType
    val isOpen: bool

    new(vertex: Vertex, polytype: PathType, ?isOpen: bool) =
        { vertex = vertex
          polytype = polytype
          isOpen = defaultArg isOpen false }

    static member op_Equality(lm1: LocalMinima, lm2: LocalMinima) =
        Object.ReferenceEquals(lm1.vertex, lm2.vertex)

    static member op_Inequality(lm1: LocalMinima, lm2: LocalMinima) =
        not (Object.ReferenceEquals(lm1.vertex, lm2.vertex))

    override this.Equals(obj: obj) =
        match obj with
        | :? LocalMinima as lm -> Object.ReferenceEquals(this.vertex, lm.vertex)
        | _ -> false

    override this.GetHashCode() =
        this.vertex.GetHashCode()

and [<AllowNullLiteral>] internal Active() =
    member val bot = Point64() with get, set
    member val top = Point64() with get, set
    member val curX: int64 = 0L with get, set
    member val dx: float = 0.0 with get, set
    member val windDx: int = 0 with get, set
    member val windCount: int = 0 with get, set
    member val windCount2: int = 0 with get, set
    member val outrec: OutRec = null with get, set
    member val prevInAEL: Active = null with get, set
    member val nextInAEL: Active = null with get, set
    member val prevInSEL: Active = null with get, set
    member val nextInSEL: Active = null with get, set
    member val jump: Active = null with get, set
    member val vertexTop: Vertex = null with get, set
    member val localMin = LocalMinima() with get, set
    member val internal isLeftBound: bool = false with get, set
    member val internal joinWith: JoinWith = JoinWith.None with get, set

and [<AllowNullLiteral>] internal OutRec() =
    member val idx: int = 0 with get, set
    member val outPtCount: int = 0 with get, set
    member val owner: OutRec = null with get, set
    member val frontEdge: Active = null with get, set
    member val backEdge: Active = null with get, set
    member val pts: OutPt = null with get, set
    member val polypath: PolyPathBase = null with get, set
    member val bounds = Rect64() with get, set
    member val path: Path64 = Path64() with get, set
    member val isOpen: bool = false with get, set
    member val splits: List<int> = null with get, set
    member val recursiveSplit: OutRec = null with get, set

and [<AllowNullLiteral>] internal OutPt(pt: Point64, outrec: OutRec) =
    member val pt = pt with get, set
    member val next: OutPt = null with get, set
    member val prev: OutPt = null with get, set
    member val outrec: OutRec = outrec with get, set
    member val horz: HorzSegment = null with get, set

and [<AllowNullLiteral>] internal HorzSegment(op: OutPt) =
    member val leftOp: OutPt = op with get, set
    member val rightOp: OutPt = null with get, set
    member val leftToRight: bool = true with get, set

and [<AllowNullLiteral>] internal HorzJoin(ltor: OutPt, rtol: OutPt) =
    member val op1: OutPt = ltor with get, set
    member val op2: OutPt = rtol with get, set

// Standalone struct (depends on Active only)
[<Struct>]
type internal IntersectNode =
    val pt: Point64
    val edge1: Active
    val edge2: Active

    new(pt: Point64, edge1: Active, edge2: Active) =
        { pt = pt; edge1 = edge1; edge2 = edge2 }

[<Struct>]
type internal LocMinSorter =
    interface IComparer<LocalMinima> with
        member _.Compare(locMin1: LocalMinima, locMin2: LocalMinima) : int =
            locMin2.vertex.pt.Y.CompareTo(locMin1.vertex.pt.Y)

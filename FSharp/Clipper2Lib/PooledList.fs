#if USINGZ
namespace Clipper2ZLib
#else
namespace Clipper2Lib
#endif

open System
open System.Collections
open System.Collections.Generic

// Generic PooledList base
[<AbstractClass; AllowNullLiteral>]
type internal PooledList<'T when 'T : not struct and 'T : null>() =

    static let DefaultCapacity = 4

    let mutable _items: 'T[] = Array.zeroCreate<'T> 0
    let mutable _size: int = 0

    member internal _.Items with get() = _items and set v = _items <- v
    member internal _.Size with get() = _size and set v = _size <- v

    member _.Item
        with get(index: int) =
            if uint32 index >= uint32 _size then
                raise (ArgumentOutOfRangeException("index must be greater or equal to zero and less than the size of the collection"))
            _items.[index]

    member _.Count = _size

    member this.Capacity
        with get() = _items.Length
        and set(value: int) =
            if value > _items.Length then
                let rounded = int (PooledList<'T>.RoundUpToPowerOf2(uint32 value))
                let newItems = Array.zeroCreate<'T> rounded
                if _size > 0 then
                    Array.Copy(_items, newItems, _size)
                _items <- newItems

    static member private RoundUpToPowerOf2(value: uint32) : uint32 =
        let mutable v = value - 1u
        v <- v ||| (v >>> 1)
        v <- v ||| (v >>> 2)
        v <- v ||| (v >>> 4)
        v <- v ||| (v >>> 8)
        v <- v ||| (v >>> 16)
        v + 1u

    member this.EnsureCapacity(capacity: int) = this.Capacity <- capacity

    member internal this.TryGrow() =
        let newSize = _size + 1
        if newSize > _items.Length then
            let newCapacity = if _items.Length = 0 then DefaultCapacity else 2 * _items.Length
            this.Capacity <- newCapacity

    abstract Clear: unit -> unit
    default _.Clear() = _size <- 0

    interface IReadOnlyList<'T> with
        member this.Item with get index = this.[index]
        member _.Count = _size
        member this.GetEnumerator() : IEnumerator<'T> =
            new ListEnumerator<'T>(this) :> IEnumerator<'T>
        member this.GetEnumerator() : IEnumerator =
            new ListEnumerator<'T>(this) :> IEnumerator

and [<Struct>] internal ListEnumerator<'T2 when 'T2 : not struct and 'T2 : null> =
    val mutable private _list: PooledList<'T2>
    val mutable private _index: int
    val mutable private _current: 'T2

    new(list: PooledList<'T2>) =
        { _list = list; _index = 0; _current = null }

    interface IEnumerator<'T2> with
        member this.Current = this._current

    interface IEnumerator with
        member this.Current = this._current :> obj

        member this.MoveNext() =
            let count = this._list.Size
            if uint32 this._index < uint32 count then
                this._current <- this._list.[this._index]
                this._index <- this._index + 1
                true
            else
                this._index <- count + 1
                this._current <- null
                false

        member this.Reset() =
            this._index <- 0
            this._current <- null

    interface IDisposable with
        member _.Dispose() = ()

// Concrete pool wrappers
type internal VertexPoolList() =
    inherit PooledList<Vertex>()

    member inline this.Add(point: Point64, flags: VertexFlags, prev: Vertex) : Vertex =
        this.TryGrow()
        let mutable poolVtx = this.Items.[this.Size]
        if isNull poolVtx then
            poolVtx <- Vertex(point, flags, prev)
            this.Items.[this.Size] <- poolVtx
        else
            poolVtx.pt <- point
            poolVtx.flags <- flags
            poolVtx.prev <- prev
            poolVtx.next <- null
        this.Size <- this.Size + 1
        poolVtx

type internal OutPtPoolList() =
    inherit PooledList<OutPt>()

    static member val UseOutPtPool = true with get, set

    member inline this.Add(pt: Point64, outrec: OutRec) : OutPt =
        this.TryGrow()
        let mutable poolPt = this.Items.[this.Size]
        if isNull poolPt then
            poolPt <- OutPt(pt, outrec)
            poolPt.next <- poolPt
            poolPt.prev <- poolPt
            this.Items.[this.Size] <- poolPt
        else
            poolPt.pt <- pt
            poolPt.outrec <- outrec
            poolPt.next <- poolPt
            poolPt.prev <- poolPt
            poolPt.horz <- null
        this.Size <- this.Size + 1
        outrec.outPtCount <- outrec.outPtCount + 1
        poolPt

type internal OutRecPoolList() =
    inherit PooledList<OutRec>()

    static let tombStone = Path64()

    member inline this.Add() : OutRec =
        this.TryGrow()
        let mutable outRec = this.Items.[this.Size]
        if isNull outRec then
            outRec <- OutRec()
            this.Items.[this.Size] <- outRec
        else
            outRec.idx <- 0
            outRec.outPtCount <- 0
            outRec.owner <- null
            outRec.frontEdge <- null
            outRec.backEdge <- null
            outRec.pts <- null
            outRec.bounds <- Rect64()
            outRec.path <- Path64()
            outRec.polypath <- null
            outRec.isOpen <- false
            if not (isNull outRec.splits) then outRec.splits.Clear()
            outRec.recursiveSplit <- null
        this.Size <- this.Size + 1
        outRec

    override this.Clear() =
        base.Clear()
        let items = this.Items
        let mutable i = 0
        while i < items.Length do
            let active = items.[i]
            if isNull active || Object.ReferenceEquals(active.path, tombStone) then
                i <- items.Length // break
            else
                active.path <- tombStone
                active.owner <- null
                active.frontEdge <- null
                active.backEdge <- null
                active.pts <- null
                active.polypath <- null
                active.recursiveSplit <- null
            i <- i + 1

type internal HorzJoinPoolList() =
    inherit PooledList<HorzJoin>()

    member inline this.Add(ltor: OutPt, rtol: OutPt) : HorzJoin =
        this.TryGrow()
        let mutable hJoin = this.Items.[this.Size]
        if isNull hJoin then
            hJoin <- HorzJoin(ltor, rtol)
            this.Items.[this.Size] <- hJoin
        else
            hJoin.op1 <- ltor
            hJoin.op2 <- rtol
        this.Size <- this.Size + 1
        hJoin

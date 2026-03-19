#if USINGZ
namespace Clipper2ZLib
#else
namespace Clipper2Lib
#endif

open System
open System.Collections.Generic

[<AllowNullLiteral>]
type OutPt2(pt: Point64) =
    member val next: OutPt2 = null with get, set
    member val prev: OutPt2 = null with get, set
    member val pt = pt with get, set
    member val ownerIdx: int = 0 with get, set
    member val edge: List<OutPt2> = null with get, set

// Location enum pulled out of RectClip64 since F# doesn't support nested types
type internal RectClipLocation =
    | Left = 0
    | Top = 1
    | Right = 2
    | Bottom = 3
    | Inside = 4

[<AutoOpen>]
module internal RectClipHelpers =

    let inline isHorizontal (pt1: Point64) (pt2: Point64) : bool =
        pt1.Y = pt2.Y

    let inline areOpposites (prev: RectClipLocation) (curr: RectClipLocation) : bool =
        Math.Abs(int prev - int curr) = 2

    let inline headingClockwise (prev: RectClipLocation) (curr: RectClipLocation) : bool =
        ((int prev + 1) % 4) = int curr

    let inline getAdjacentLocation (loc: RectClipLocation) (isClockwise: bool) : RectClipLocation =
        let delta = if isClockwise then 1 else 3
        enum<RectClipLocation> ((int loc + delta) % 4)

    let inline isClockwise (prev: RectClipLocation) (curr: RectClipLocation) (prevPt: Point64) (currPt: Point64) (rectMidPoint: Point64) : bool =
        if areOpposites prev curr then
            InternalClipper.CrossProductSign(prevPt, rectMidPoint, currPt) < 0
        else
            headingClockwise prev curr

    let inline unlinkOp (op: OutPt2) : OutPt2 =
        if obj.ReferenceEquals(op.next, op) then null
        else
            op.prev.next <- op.next
            op.next.prev <- op.prev
            op.next

    let inline unlinkOpBack (op: OutPt2) : OutPt2 =
        if obj.ReferenceEquals(op.next, op) then null
        else
            op.prev.next <- op.next
            op.next.prev <- op.prev
            op.prev

    let inline getEdgesForPt (pt: Point64) (rec': Rect64) : uint32 =
        let mutable result = 0u
        if pt.X = rec'.left then result <- 1u
        elif pt.X = rec'.right then result <- 4u
        if pt.Y = rec'.top then result <- result + 2u
        elif pt.Y = rec'.bottom then result <- result + 8u
        result

    let inline isHeadingClockwise (pt1: Point64) (pt2: Point64) (edgeIdx: int) : bool =
        if edgeIdx = 0 then pt2.Y < pt1.Y
        elif edgeIdx = 1 then pt2.X > pt1.X
        elif edgeIdx = 2 then pt2.Y > pt1.Y
        else pt2.X < pt1.X

    let inline hasHorzOverlap (left1: Point64) (right1: Point64) (left2: Point64) (right2: Point64) : bool =
        (left1.X < right2.X) && (right1.X > left2.X)

    let inline hasVertOverlap (top1: Point64) (bottom1: Point64) (top2: Point64) (bottom2: Point64) : bool =
        (top1.Y < bottom2.Y) && (bottom1.Y > top2.Y)

    let inline addToEdge (edge: List<OutPt2>) (op: OutPt2) : unit =
        if not (isNull op.edge) then ()
        else
            op.edge <- edge
            edge.Add(op)

    let uncoupleEdge (op: OutPt2) : unit =
        if isNull op.edge then ()
        else
            let mutable i = 0
            let mutable break_ = false
            while i < op.edge.Count && not break_ do
                let op2 = op.edge.[i]
                if obj.ReferenceEquals(op2, op) then
                    op.edge.[i] <- null
                    break_ <- true
                else
                    i <- i + 1
            op.edge <- null

    let setNewOwner (op: OutPt2) (newIdx: int) : unit =
        op.ownerIdx <- newIdx
        let mutable op2 = op.next
        while not (obj.ReferenceEquals(op2, op)) do
            op2.ownerIdx <- newIdx
            op2 <- op2.next

    let path1ContainsPath2 (path1: Path64) (path2: Path64) : bool =
        // nb: occasionally, due to rounding, path1 may
        // appear (momentarily) inside or outside path2.
        let mutable ioCount = 0
        let mutable break_ = false
        let mutable idx = 0
        while idx < path2.Count && not break_ do
            let pt = path2.[idx]
            let pip = InternalClipper.PointInPolygon(pt, path1)
            if pip = PointInPolygonResult.IsInside then
                ioCount <- ioCount - 1
            elif pip = PointInPolygonResult.IsOutside then
                ioCount <- ioCount + 1
            if Math.Abs(ioCount) > 1 then break_ <- true
            else idx <- idx + 1
        ioCount <= 0

    let getSegmentIntersection (p1: Point64) (p2: Point64) (p3: Point64) (p4: Point64) (ip: byref<Point64>) : bool =
        let res1 = InternalClipper.CrossProductSign(p1, p3, p4)
        let res2 = InternalClipper.CrossProductSign(p2, p3, p4)
        if res1 = 0 then
            ip <- p1
            if res2 = 0 then false // segments are collinear
            elif p1 = p3 || p1 = p4 then true
            elif isHorizontal p3 p4 then ((p1.X > p3.X) = (p1.X < p4.X))
            else ((p1.Y > p3.Y) = (p1.Y < p4.Y))
        elif res2 = 0 then
            ip <- p2
            if p2 = p3 || p2 = p4 then true
            elif isHorizontal p3 p4 then ((p2.X > p3.X) = (p2.X < p4.X))
            else ((p2.Y > p3.Y) = (p2.Y < p4.Y))
        elif (res1 > 0) = (res2 > 0) then
            ip <- Point64(0L, 0L)
            false
        else
            let res3 = InternalClipper.CrossProductSign(p3, p1, p2)
            let res4 = InternalClipper.CrossProductSign(p4, p1, p2)
            if res3 = 0 then
                ip <- p3
                if p3 = p1 || p3 = p2 then true
                elif isHorizontal p1 p2 then ((p3.X > p1.X) = (p3.X < p2.X))
                else ((p3.Y > p1.Y) = (p3.Y < p2.Y))
            elif res4 = 0 then
                ip <- p4
                if p4 = p1 || p4 = p2 then true
                elif isHorizontal p1 p2 then ((p4.X > p1.X) = (p4.X < p2.X))
                else ((p4.Y > p1.Y) = (p4.Y < p2.Y))
            elif (res3 > 0) = (res4 > 0) then
                ip <- Point64(0L, 0L)
                false
            else
                // segments must intersect to get here
                InternalClipper.GetLineIntersectPt(p1, p2, p3, p4, &ip)

    let startLocsAreClockwise (startLocs: List<RectClipLocation>) : bool =
        let mutable result = 0
        for i = 1 to startLocs.Count - 1 do
            let d = int startLocs.[i] - int startLocs.[i - 1]
            if d = -1 then result <- result - 1
            elif d = 1 then result <- result + 1
            elif d = -3 then result <- result + 1
            elif d = 3 then result <- result - 1
        result > 0

type RectClip64 internal (rect: Rect64) =

    let rect_ = rect
    let mp_ = rect.MidPoint()
    let rectPath_ = rect.AsPath()
    let mutable pathBounds_ = Rect64()
    let results_ = List<OutPt2>()
    let edges_ = Array.init 8 (fun _ -> List<OutPt2>())
    let mutable currIdx_ = -1

    member internal _.Rect = rect_
    member internal _.Mp = mp_
    member internal _.RectPath = rectPath_
    member internal _.PathBounds with get() = pathBounds_ and set v = pathBounds_ <- v
    member internal _.Results = results_
    member internal _.Edges = edges_
    member internal _.CurrIdx with get() = currIdx_ and set v = currIdx_ <- v

    member internal this.Add(pt: Point64, ?startingNewPath: bool) : OutPt2 =
        let startingNewPath = defaultArg startingNewPath false
        // this method is only called by InternalExecute.
        // Later splitting and rejoining won't create additional op's,
        // though they will change the (non-storage) fResults count.
        let currIdx = results_.Count
        let mutable result : OutPt2 = null
        if (currIdx = 0) || startingNewPath then
            result <- OutPt2(pt)
            results_.Add(result)
            result.ownerIdx <- currIdx
            result.prev <- result
            result.next <- result
        else
            let currIdx2 = currIdx - 1
            let prevOp = results_.[currIdx2]
            if prevOp.pt = pt then result <- prevOp
            else
                result <- OutPt2(pt)
                result.ownerIdx <- currIdx2
                result.next <- prevOp.next
                prevOp.next.prev <- result
                prevOp.next <- result
                result.prev <- prevOp
                results_.[currIdx2] <- result
        result

    static member internal GetLocation(r: Rect64, pt: Point64, loc: byref<RectClipLocation>) : bool =
        if pt.X = r.left && pt.Y >= r.top && pt.Y <= r.bottom then
            loc <- RectClipLocation.Left; false // pt on rec
        elif pt.X = r.right && pt.Y >= r.top && pt.Y <= r.bottom then
            loc <- RectClipLocation.Right; false // pt on rec
        elif pt.Y = r.top && pt.X >= r.left && pt.X <= r.right then
            loc <- RectClipLocation.Top; false // pt on rec
        elif pt.Y = r.bottom && pt.X >= r.left && pt.X <= r.right then
            loc <- RectClipLocation.Bottom; false // pt on rec
        else
            if pt.X < r.left then loc <- RectClipLocation.Left
            elif pt.X > r.right then loc <- RectClipLocation.Right
            elif pt.Y < r.top then loc <- RectClipLocation.Top
            elif pt.Y > r.bottom then loc <- RectClipLocation.Bottom
            else loc <- RectClipLocation.Inside
            true

    static member internal GetIntersection(rectPath: Path64, p: Point64, p2: Point64, loc: byref<RectClipLocation>, ip: byref<Point64>) : bool =
        // gets the pt of intersection between rectPath and segment(p, p2) that's closest to 'p'
        // when result == false, loc will remain unchanged
        ip <- Point64()
        if loc = RectClipLocation.Left then
            if getSegmentIntersection p p2 rectPath.[0] rectPath.[3] &ip then true
            elif p.Y < rectPath.[0].Y && getSegmentIntersection p p2 rectPath.[0] rectPath.[1] &ip then
                loc <- RectClipLocation.Top; true
            elif getSegmentIntersection p p2 rectPath.[2] rectPath.[3] &ip then
                loc <- RectClipLocation.Bottom; true
            else false
        elif loc = RectClipLocation.Right then
            if getSegmentIntersection p p2 rectPath.[1] rectPath.[2] &ip then true
            elif p.Y < rectPath.[0].Y && getSegmentIntersection p p2 rectPath.[0] rectPath.[1] &ip then
                loc <- RectClipLocation.Top; true
            elif getSegmentIntersection p p2 rectPath.[2] rectPath.[3] &ip then
                loc <- RectClipLocation.Bottom; true
            else false
        elif loc = RectClipLocation.Top then
            if getSegmentIntersection p p2 rectPath.[0] rectPath.[1] &ip then true
            elif p.X < rectPath.[0].X && getSegmentIntersection p p2 rectPath.[0] rectPath.[3] &ip then
                loc <- RectClipLocation.Left; true
            elif p.X > rectPath.[1].X && getSegmentIntersection p p2 rectPath.[1] rectPath.[2] &ip then
                loc <- RectClipLocation.Right; true
            else false
        elif loc = RectClipLocation.Bottom then
            if getSegmentIntersection p p2 rectPath.[2] rectPath.[3] &ip then true
            elif p.X < rectPath.[3].X && getSegmentIntersection p p2 rectPath.[0] rectPath.[3] &ip then
                loc <- RectClipLocation.Left; true
            elif p.X > rectPath.[2].X && getSegmentIntersection p p2 rectPath.[1] rectPath.[2] &ip then
                loc <- RectClipLocation.Right; true
            else false
        else // Inside
            if getSegmentIntersection p p2 rectPath.[0] rectPath.[3] &ip then
                loc <- RectClipLocation.Left; true
            elif getSegmentIntersection p p2 rectPath.[0] rectPath.[1] &ip then
                loc <- RectClipLocation.Top; true
            elif getSegmentIntersection p p2 rectPath.[1] rectPath.[2] &ip then
                loc <- RectClipLocation.Right; true
            elif getSegmentIntersection p p2 rectPath.[2] rectPath.[3] &ip then
                loc <- RectClipLocation.Bottom; true
            else false

    member internal this.GetNextLocation(path: Path64, loc: byref<RectClipLocation>, i: byref<int>, highI: int) : unit =
        if loc = RectClipLocation.Left then
            while i <= highI && path.[i].X <= rect_.left do i <- i + 1
            if i <= highI then
                if path.[i].X >= rect_.right then loc <- RectClipLocation.Right
                elif path.[i].Y <= rect_.top then loc <- RectClipLocation.Top
                elif path.[i].Y >= rect_.bottom then loc <- RectClipLocation.Bottom
                else loc <- RectClipLocation.Inside
        elif loc = RectClipLocation.Top then
            while i <= highI && path.[i].Y <= rect_.top do i <- i + 1
            if i <= highI then
                if path.[i].Y >= rect_.bottom then loc <- RectClipLocation.Bottom
                elif path.[i].X <= rect_.left then loc <- RectClipLocation.Left
                elif path.[i].X >= rect_.right then loc <- RectClipLocation.Right
                else loc <- RectClipLocation.Inside
        elif loc = RectClipLocation.Right then
            while i <= highI && path.[i].X >= rect_.right do i <- i + 1
            if i <= highI then
                if path.[i].X <= rect_.left then loc <- RectClipLocation.Left
                elif path.[i].Y <= rect_.top then loc <- RectClipLocation.Top
                elif path.[i].Y >= rect_.bottom then loc <- RectClipLocation.Bottom
                else loc <- RectClipLocation.Inside
        elif loc = RectClipLocation.Bottom then
            while i <= highI && path.[i].Y >= rect_.bottom do i <- i + 1
            if i <= highI then
                if path.[i].Y <= rect_.top then loc <- RectClipLocation.Top
                elif path.[i].X <= rect_.left then loc <- RectClipLocation.Left
                elif path.[i].X >= rect_.right then loc <- RectClipLocation.Right
                else loc <- RectClipLocation.Inside
        else // Inside
            let mutable continue_ = true
            while i <= highI && continue_ do
                if path.[i].X < rect_.left then loc <- RectClipLocation.Left; continue_ <- false
                elif path.[i].X > rect_.right then loc <- RectClipLocation.Right; continue_ <- false
                elif path.[i].Y > rect_.bottom then loc <- RectClipLocation.Bottom; continue_ <- false
                elif path.[i].Y < rect_.top then loc <- RectClipLocation.Top; continue_ <- false
                else
                    this.Add(path.[i]) |> ignore
                    i <- i + 1

    member private this.AddCorner(prev: RectClipLocation, curr: RectClipLocation) : unit =
        if headingClockwise prev curr then
            this.Add(rectPath_.[int prev]) |> ignore
        else
            this.Add(rectPath_.[int curr]) |> ignore

    member private this.AddCornerByRef(loc: byref<RectClipLocation>, isClockwise: bool) : unit =
        if isClockwise then
            this.Add(rectPath_.[int loc]) |> ignore
            loc <- getAdjacentLocation loc true
        else
            loc <- getAdjacentLocation loc false
            this.Add(rectPath_.[int loc]) |> ignore

    member private this.ExecuteInternal(path: Path64) : unit =
        if path.Count < 3 || rect_.IsEmpty() then ()
        else
            let startLocs = List<RectClipLocation>()

            let mutable firstCross = RectClipLocation.Inside
            let mutable crossingLoc = firstCross
            let mutable prev = firstCross

            let highI = path.Count - 1
            let mutable loc = RectClipLocation.Inside
            let mutable i = 0
            if not (RectClip64.GetLocation(rect_, path.[highI], &loc)) then
                i <- highI - 1
                while i >= 0 && not (RectClip64.GetLocation(rect_, path.[i], &prev)) do
                    i <- i - 1
                if i < 0 then
                    for pt in path do this.Add(pt) |> ignore
                else
                    if prev = RectClipLocation.Inside then loc <- RectClipLocation.Inside

            if i >= 0 then
                let startingLoc = loc

                ///////////////////////////////////////////////////
                i <- 0
                while i <= highI do
                    prev <- loc
                    let prevCrossLoc = crossingLoc
                    this.GetNextLocation(path, &loc, &i, highI)
                    if i <= highI then
                        let prevPt = if i = 0 then path.[highI] else path.[i - 1]
                        crossingLoc <- loc
                        let mutable ip = Point64()

                        if not (RectClip64.GetIntersection(rectPath_, path.[i], prevPt, &crossingLoc, &ip)) then
                            // ie remaining outside
                            if prevCrossLoc = RectClipLocation.Inside then
                                let isClockw = isClockwise prev loc prevPt path.[i] mp_
                                let mutable p = prev
                                while p <> loc do
                                    startLocs.Add(p)
                                    p <- getAdjacentLocation p isClockw
                                crossingLoc <- prevCrossLoc // still not crossed

                            elif prev <> RectClipLocation.Inside && prev <> loc then
                                let isClockw = isClockwise prev loc prevPt path.[i] mp_
                                let mutable p = prev
                                while p <> loc do
                                    this.AddCornerByRef(&p, isClockw)

                            i <- i + 1
                        else
                            ////////////////////////////////////////////////////
                            // we must be crossing the rect boundary to get here
                            ////////////////////////////////////////////////////
                            if loc = RectClipLocation.Inside then
                                // path must be entering rect
                                if firstCross = RectClipLocation.Inside then
                                    firstCross <- crossingLoc
                                    startLocs.Add(prev)
                                elif prev <> crossingLoc then
                                    let isClockw = isClockwise prev crossingLoc prevPt path.[i] mp_
                                    let mutable p = prev
                                    while p <> crossingLoc do
                                        this.AddCornerByRef(&p, isClockw)

                            elif prev <> RectClipLocation.Inside then
                                // passing right through rect. ip here is the second
                                // intersect pt, we'll also need first intersect pt (ip2)
                                loc <- prev
                                let mutable ip2 = Point64()
                                RectClip64.GetIntersection(rectPath_, prevPt, path.[i], &loc, &ip2) |> ignore

                                if prevCrossLoc <> RectClipLocation.Inside && prevCrossLoc <> loc then
                                    this.AddCorner(prevCrossLoc, loc)

                                if firstCross = RectClipLocation.Inside then
                                    firstCross <- loc
                                    startLocs.Add(prev)

                                loc <- crossingLoc
                                this.Add(ip2) |> ignore

                                if ip = ip2 then
                                    // it's very likely that path[i] is on rect
                                    RectClip64.GetLocation(rect_, path.[i], &loc) |> ignore
                                    this.AddCorner(crossingLoc, loc)
                                    crossingLoc <- loc
                                else
                                    this.Add(ip) |> ignore

                            else
                                // path must be exiting rect
                                loc <- crossingLoc
                                if firstCross = RectClipLocation.Inside then
                                    firstCross <- crossingLoc
                                this.Add(ip) |> ignore

                ///////////////////////////////////////////////////

                if firstCross = RectClipLocation.Inside then
                    // path never intersects
                    if startingLoc = RectClipLocation.Inside then ()
                    elif (not (pathBounds_.Contains(rect_))) || (not (path1ContainsPath2 path rectPath_)) then ()
                    else
                        let startLocsClockwise = startLocsAreClockwise(startLocs)
                        for j = 0 to 3 do
                            let k = if startLocsClockwise then j else 3 - j
                            this.Add(rectPath_.[k]) |> ignore
                            addToEdge edges_.[k * 2] results_.[0]
                elif loc <> RectClipLocation.Inside && (loc <> firstCross || startLocs.Count > 2) then
                    if startLocs.Count > 0 then
                        prev <- loc
                        for loc2 in startLocs do
                            if prev <> loc2 then
                                this.AddCornerByRef(&prev, headingClockwise prev loc2)
                                prev <- loc2
                        loc <- prev
                    if loc <> firstCross then
                        this.AddCornerByRef(&loc, headingClockwise loc firstCross)

    member private this.CheckEdges() : unit =
        for i = 0 to results_.Count - 1 do
            let mutable op = results_.[i]
            let mutable op2 = op
            if not (isNull op) then
                let mutable continueLoop = true
                while continueLoop do
                    if InternalClipper.IsCollinear(op2.prev.pt, op2.pt, op2.next.pt) then
                        if obj.ReferenceEquals(op2, op) then
                            op2 <- unlinkOpBack op2
                            if isNull op2 then continueLoop <- false
                            else op <- op2.prev
                        else
                            op2 <- unlinkOpBack op2
                            if isNull op2 then continueLoop <- false
                    else
                        op2 <- op2.next
                    if continueLoop && obj.ReferenceEquals(op2, op) then
                        continueLoop <- false

                if isNull op2 then
                    results_.[i] <- null
                else
                    results_.[i] <- op2 // safety first

                    let mutable edgeSet1 = getEdgesForPt op.prev.pt rect_
                    op2 <- op
                    let mutable continueLoop2 = true
                    while continueLoop2 do
                        let edgeSet2 = getEdgesForPt op2.pt rect_
                        if edgeSet2 <> 0u && isNull op2.edge then
                            let combinedSet = edgeSet1 &&& edgeSet2
                            for j = 0 to 3 do
                                if (combinedSet &&& (1u <<< j)) <> 0u then
                                    if isHeadingClockwise op2.prev.pt op2.pt j then
                                        addToEdge edges_.[j * 2] op2
                                    else
                                        addToEdge edges_.[j * 2 + 1] op2
                        edgeSet1 <- edgeSet2
                        op2 <- op2.next
                        if obj.ReferenceEquals(op2, op) then continueLoop2 <- false

    member private this.TidyEdgePair(idx: int, cw: List<OutPt2>, ccw: List<OutPt2>) : unit =
        if ccw.Count = 0 then ()
        else
            let isHorz = (idx = 1) || (idx = 3)
            let cwIsTowardLarger = (idx = 1) || (idx = 2)
            let mutable i = 0
            let mutable j = 0

            while i < cw.Count do
                let p1Check = cw.[i]
                if isNull p1Check || obj.ReferenceEquals(p1Check.next, p1Check.prev) then
                    cw.[i] <- null
                    i <- i + 1
                    j <- 0
                else
                    let jLim = ccw.Count
                    while j < jLim && (isNull ccw.[j] || obj.ReferenceEquals(ccw.[j].next, ccw.[j].prev)) do
                        j <- j + 1

                    if j = jLim then
                        i <- i + 1
                        j <- 0
                    else
                        let mutable p1 : OutPt2 = null
                        let mutable p1a : OutPt2 = null
                        let mutable p2 : OutPt2 = null
                        let mutable p2a : OutPt2 = null
                        if cwIsTowardLarger then
                            // p1 >>>> p1a;
                            // p2 <<<< p2a;
                            p1 <- cw.[i].prev
                            p1a <- cw.[i]
                            p2 <- ccw.[j]
                            p2a <- ccw.[j].prev
                        else
                            // p1 <<<< p1a;
                            // p2 >>>> p2a;
                            p1 <- cw.[i]
                            p1a <- cw.[i].prev
                            p2 <- ccw.[j].prev
                            p2a <- ccw.[j]

                        if (isHorz && not (hasHorzOverlap p1.pt p1a.pt p2.pt p2a.pt)) ||
                           (not isHorz && not (hasVertOverlap p1.pt p1a.pt p2.pt p2a.pt)) then
                            j <- j + 1
                        else
                            // to get here we're either splitting or rejoining
                            let isRejoining = cw.[i].ownerIdx <> ccw.[j].ownerIdx

                            if isRejoining then
                                results_.[p2.ownerIdx] <- null
                                setNewOwner p2 p1.ownerIdx

                            // do the split or re-join
                            if cwIsTowardLarger then
                                // p1 >> | >> p1a;
                                // p2 << | << p2a;
                                p1.next <- p2
                                p2.prev <- p1
                                p1a.prev <- p2a
                                p2a.next <- p1a
                            else
                                // p1 << | << p1a;
                                // p2 >> | >> p2a;
                                p1.prev <- p2
                                p2.next <- p1
                                p1a.next <- p2a
                                p2a.prev <- p1a

                            if not isRejoining then
                                let new_idx = results_.Count
                                results_.Add(p1a)
                                setNewOwner p1a new_idx

                            let mutable op : OutPt2 = null
                            let mutable op2 : OutPt2 = null
                            if cwIsTowardLarger then
                                op <- p2
                                op2 <- p1a
                            else
                                op <- p1
                                op2 <- p2a
                            results_.[op.ownerIdx] <- op
                            results_.[op2.ownerIdx] <- op2

                            // and now lots of work to get ready for the next loop
                            let mutable opIsLarger = false
                            let mutable op2IsLarger = false
                            if isHorz then // X
                                opIsLarger <- op.pt.X > op.prev.pt.X
                                op2IsLarger <- op2.pt.X > op2.prev.pt.X
                            else       // Y
                                opIsLarger <- op.pt.Y > op.prev.pt.Y
                                op2IsLarger <- op2.pt.Y > op2.prev.pt.Y

                            if (obj.ReferenceEquals(op.next, op.prev)) ||
                               (op.pt = op.prev.pt) then
                                if op2IsLarger = cwIsTowardLarger then
                                    cw.[i] <- op2
                                    ccw.[j] <- null
                                    j <- j + 1
                                else
                                    ccw.[j] <- op2
                                    cw.[i] <- null
                                    i <- i + 1
                            elif (obj.ReferenceEquals(op2.next, op2.prev)) ||
                                 (op2.pt = op2.prev.pt) then
                                if opIsLarger = cwIsTowardLarger then
                                    cw.[i] <- op
                                    ccw.[j] <- null
                                    j <- j + 1
                                else
                                    ccw.[j] <- op
                                    cw.[i] <- null
                                    i <- i + 1
                            elif opIsLarger = op2IsLarger then
                                if opIsLarger = cwIsTowardLarger then
                                    cw.[i] <- op
                                    uncoupleEdge op2
                                    addToEdge cw op2
                                    ccw.[j] <- null
                                    j <- j + 1
                                else
                                    cw.[i] <- null
                                    i <- i + 1
                                    ccw.[j] <- op2
                                    uncoupleEdge op
                                    addToEdge ccw op
                                    j <- 0
                            else
                                if opIsLarger = cwIsTowardLarger then
                                    cw.[i] <- op
                                else
                                    ccw.[j] <- op
                                if op2IsLarger = cwIsTowardLarger then
                                    cw.[i] <- op2
                                else
                                    ccw.[j] <- op2

    static member private GetPath(op: OutPt2) : Path64 =
        let result = Path64()
        if isNull op || obj.ReferenceEquals(op.prev, op.next) then result
        else
            let mutable op2 = op.next
            let mutable opRef = op
            let mutable continueLoop = true
            while not (isNull op2) && not (obj.ReferenceEquals(op2, opRef)) && continueLoop do
                if InternalClipper.IsCollinear(op2.prev.pt, op2.pt, op2.next.pt) then
                    opRef <- op2.prev
                    op2 <- unlinkOp op2
                else
                    op2 <- op2.next
                if isNull op2 then continueLoop <- false
            if isNull op2 then Path64()
            else
                result.Add(opRef.pt)
                op2 <- opRef.next
                while not (obj.ReferenceEquals(op2, opRef)) do
                    result.Add(op2.pt)
                    op2 <- op2.next
                result

    member this.Execute(paths: Paths64) : Paths64 =
        let result = Paths64()
        if rect_.IsEmpty() then result
        else
            for path in paths do
                if path.Count >= 3 then
                    pathBounds_ <- InternalClipper.GetBounds(path)
                    if rect_.Intersects(pathBounds_) then
                        if rect_.Contains(pathBounds_) then
                            // the path must be completely inside rect_
                            result.Add(path)
                        else
                            this.ExecuteInternal(path)
                            this.CheckEdges()
                            for i = 0 to 3 do
                                this.TidyEdgePair(i, edges_.[i * 2], edges_.[i * 2 + 1])

                            for op in results_ do
                                let tmp = RectClip64.GetPath(op)
                                if tmp.Count > 0 then result.Add(tmp)

                            //clean up after every loop
                            results_.Clear()
                            for i = 0 to 7 do
                                edges_.[i].Clear()
            result

type RectClipLines64 internal (rect: Rect64) =
    inherit RectClip64(rect)

    static member private GetPath(op: OutPt2) : Path64 =
        let result = Path64()
        if isNull op || obj.ReferenceEquals(op, op.next) then result
        else
            let op = op.next // starting at path beginning
            result.Add(op.pt)
            let mutable op2 = op.next
            while not (obj.ReferenceEquals(op2, op)) do
                result.Add(op2.pt)
                op2 <- op2.next
            result

    member private this.ExecuteInternal(path: Path64) : unit =
        let results_ = this.Results
        let rect_ = this.Rect
        let rectPath_ = this.RectPath
        results_.Clear()
        if path.Count < 2 || rect_.IsEmpty() then ()
        else
            let mutable prev = RectClipLocation.Inside
            let highI = path.Count - 1
            let mutable loc = RectClipLocation.Inside
            let mutable i = 1

            let mutable earlyReturn = false
            if not (RectClip64.GetLocation(rect_, path.[0], &loc)) then
                while i <= highI && not (RectClip64.GetLocation(rect_, path.[i], &prev)) do
                    i <- i + 1
                if i > highI then
                    for pt in path do this.Add(pt) |> ignore
                    earlyReturn <- true
                else
                    if prev = RectClipLocation.Inside then loc <- RectClipLocation.Inside
                    i <- 1

            if not earlyReturn then
                if loc = RectClipLocation.Inside then this.Add(path.[0]) |> ignore

                ///////////////////////////////////////////////////
                while i <= highI do
                    prev <- loc
                    this.GetNextLocation(path, &loc, &i, highI)
                    if i <= highI then
                        let prevPt = path.[i - 1]

                        let mutable crossingLoc = loc
                        let mutable ip = Point64()
                        if not (RectClip64.GetIntersection(rectPath_, path.[i], prevPt, &crossingLoc, &ip)) then
                            // ie remaining outside (& crossingLoc still == loc)
                            i <- i + 1
                        else
                            ////////////////////////////////////////////////////
                            // we must be crossing the rect boundary to get here
                            ////////////////////////////////////////////////////
                            if loc = RectClipLocation.Inside then // path must be entering rect
                                this.Add(ip, true) |> ignore
                            elif prev <> RectClipLocation.Inside then
                                // passing right through rect. 'ip' here will be the second
                                // intersect pt but we'll also need the first intersect pt (ip2)
                                let mutable crossingLoc2 = prev
                                let mutable ip2 = Point64()
                                RectClip64.GetIntersection(rectPath_, prevPt, path.[i], &crossingLoc2, &ip2) |> ignore
                                this.Add(ip2, true) |> ignore
                                this.Add(ip) |> ignore
                            else // path must be exiting rect
                                this.Add(ip) |> ignore
                    // end while body (i > highI case just falls through)
                ///////////////////////////////////////////////////

    member this.Execute(paths: Paths64) : Paths64 =
        let result = Paths64()
        let rect_ = this.Rect
        let edges_ = this.Edges
        let results_ = this.Results
        if rect_.IsEmpty() then result
        else
            for path in paths do
                if path.Count >= 2 then
                    this.PathBounds <- InternalClipper.GetBounds(path)
                    if rect_.Intersects(this.PathBounds) then
                        // Apart from that, we can't be sure whether the path
                        // is completely outside or completed inside or intersects
                        // fRect, simply by comparing path bounds with fRect.
                        this.ExecuteInternal(path)

                        for op in results_ do
                            let tmp = RectClipLines64.GetPath(op)
                            if tmp.Count > 0 then result.Add(tmp)

                        //clean up after every loop
                        results_.Clear()
                        for i = 0 to 7 do
                            edges_.[i].Clear()
            result

#if USINGZ
namespace Clipper2ZLib
#else
namespace Clipper2Lib
#endif

open System
open System.Collections.Generic

// Enums
type TriangulateResult =
    | Success = 0
    | Fail = 1
    | NoPolygons = 2
    | PathsIntersect = 3

type internal EdgeKind =
    | Loose = 0
    | Ascend = 1
    | Descend = 2

type internal IntersectKind =
    | None = 0
    | Collinear = 1
    | Intersect = 2

type internal EdgeContainsResult =
    | Neither = 0
    | Left = 1
    | Right = 2

// Mutual group: Vertex2, Edge, Triangle
[<AllowNullLiteral>]
type internal Vertex2(p64: Point64) =
    member val pt = p64 with get, set
    member val edges = List<Edge>(2) with get
    member val innerLM = false with get, set

and [<AllowNullLiteral>] internal Edge() =
    member val vL: Vertex2 = null with get, set
    member val vR: Vertex2 = null with get, set
    member val vB: Vertex2 = null with get, set
    member val vT: Vertex2 = null with get, set
    member val kind: EdgeKind = EdgeKind.Loose with get, set
    member val triA: Triangle = null with get, set
    member val triB: Triangle = null with get, set
    member val isActive: bool = false with get, set
    member val nextE: Edge = null with get, set
    member val prevE: Edge = null with get, set

and [<AllowNullLiteral>] internal Triangle(e1: Edge, e2: Edge, e3: Edge) =
    member val edges = [| e1; e2; e3 |] with get

// Delaunay engine
[<AllowNullLiteral>]
type internal Delaunay(?delaunay: bool) =
    let useDelaunay = defaultArg delaunay true
    let allVertices = List<Vertex2>()
    let allEdges = List<Edge>()
    let allTriangles = List<Triangle>()
    let pendingDelaunayStack = Stack<Edge>()
    let horzEdgeStack = Stack<Edge>()
    let locMinStack = Stack<Vertex2>()
    let mutable firstActive: Edge = null
    let mutable lowermostVertex: Vertex2 = null

    member private this.AddPath(path: Path64) : unit =
        let mutable len = path.Count
        if len = 0 then ()
        else
            let mutable i0 = 0
            if not (Delaunay.FindLocMinIdx(path, len, &i0)) then ()
            else
                let mutable iPrev = Delaunay.Prev(i0, len)
                while path.[iPrev].Equals(path.[i0]) do
                    iPrev <- Delaunay.Prev(iPrev, len)

                let mutable iNext = Delaunay.Next(i0, len)
                let mutable i = i0
                let mutable isCollinearPath = false

                while InternalClipper.CrossProductSign(path.[iPrev], path.[i], path.[iNext]) = 0 && not isCollinearPath do
                    Delaunay.FindLocMinIdx(path, len, &i) |> ignore
                    if i = i0 then
                        isCollinearPath <- true
                    else
                        iPrev <- Delaunay.Prev(i, len)
                        while path.[iPrev].Equals(path.[i]) do
                            iPrev <- Delaunay.Prev(iPrev, len)
                        iNext <- Delaunay.Next(i, len)

                if not isCollinearPath then
                    let vertCnt = allVertices.Count
                    let v0 = Vertex2(path.[i])
                    allVertices.Add(v0)

                    if Delaunay.LeftTurning(path.[iPrev], path.[i], path.[iNext]) then
                        v0.innerLM <- true

                    let mutable vPrev = v0
                    i <- iNext

                    let mutable done_ = false
                    while not done_ do
                        locMinStack.Push(vPrev)

                        if isNull lowermostVertex ||
                           vPrev.pt.Y > lowermostVertex.pt.Y ||
                           (vPrev.pt.Y = lowermostVertex.pt.Y && vPrev.pt.X < lowermostVertex.pt.X) then
                            lowermostVertex <- vPrev

                        iNext <- Delaunay.Next(i, len)
                        if InternalClipper.CrossProductSign(vPrev.pt, path.[i], path.[iNext]) = 0 then
                            i <- iNext
                        else
                            while path.[i].Y <= vPrev.pt.Y do
                                let v = Vertex2(path.[i])
                                allVertices.Add(v)
                                this.CreateEdge(vPrev, v, EdgeKind.Ascend) |> ignore
                                vPrev <- v
                                i <- iNext
                                iNext <- Delaunay.Next(i, len)

                                while InternalClipper.CrossProductSign(vPrev.pt, path.[i], path.[iNext]) = 0 do
                                    i <- iNext
                                    iNext <- Delaunay.Next(i, len)

                            let mutable vPrevPrev = vPrev
                            while i <> i0 && path.[i].Y >= vPrev.pt.Y do
                                let v = Vertex2(path.[i])
                                allVertices.Add(v)
                                this.CreateEdge(v, vPrev, EdgeKind.Descend) |> ignore
                                vPrevPrev <- vPrev
                                vPrev <- v
                                i <- iNext
                                iNext <- Delaunay.Next(i, len)

                                while InternalClipper.CrossProductSign(vPrev.pt, path.[i], path.[iNext]) = 0 do
                                    i <- iNext
                                    iNext <- Delaunay.Next(i, len)

                            if i = i0 then
                                done_ <- true
                            elif Delaunay.LeftTurning(vPrevPrev.pt, vPrev.pt, path.[i]) then
                                vPrev.innerLM <- true

                    this.CreateEdge(v0, vPrev, EdgeKind.Descend) |> ignore

                    len <- allVertices.Count - vertCnt
                    let idx = vertCnt
                    if len < 3 ||
                       (len = 3 &&
                        (Delaunay.DistSqr(allVertices.[idx].pt, allVertices.[idx + 1].pt) <= 1.0 ||
                         Delaunay.DistSqr(allVertices.[idx + 1].pt, allVertices.[idx + 2].pt) <= 1.0 ||
                         Delaunay.DistSqr(allVertices.[idx + 2].pt, allVertices.[idx].pt) <= 1.0)) then
                        for j = vertCnt to allVertices.Count - 1 do
                            allVertices.[j].edges.Clear()

    member private this.AddPaths(paths: Paths64) : bool =
        let mutable totalVertexCount = 0
        for path in paths do
            totalVertexCount <- totalVertexCount + path.Count
        if totalVertexCount = 0 then false
        else
            allVertices.Capacity <- allVertices.Count + totalVertexCount
            allEdges.Capacity <- allEdges.Count + totalVertexCount
            for path in paths do
                this.AddPath(path)
            allVertices.Count > 2
    member private this.CleanUp() : unit =
        allVertices.Clear()
        allEdges.Clear()
        allTriangles.Clear()
        pendingDelaunayStack.Clear()
        horzEdgeStack.Clear()
        locMinStack.Clear()
        firstActive <- null
        lowermostVertex <- null
    member private this.FixupEdgeIntersects() : bool =
        let mutable i1 = 0
        let mutable result = true
        while i1 < allEdges.Count && result do
            let e1 = allEdges.[i1]
            let mutable i2 = i1 + 1
            while i2 < allEdges.Count && result do
                let e2 = allEdges.[i2]
                if e2.vL.pt.X >= e1.vR.pt.X then
                    i2 <- allEdges.Count
                else
                    if e2.vT.pt.Y < e1.vB.pt.Y &&
                       e2.vB.pt.Y > e1.vT.pt.Y &&
                       Delaunay.SegsIntersect(e2.vL.pt, e2.vR.pt, e1.vL.pt, e1.vR.pt) = IntersectKind.Intersect &&
                       not (this.RemoveIntersection(e2, e1)) then
                        result <- false
                    i2 <- i2 + 1
            i1 <- i1 + 1
        result

    member private this.MergeDupOrCollinearVertices() : unit =
        if allVertices.Count < 2 then ()
        else
            let mutable v1Index = 0
            for v2Index = 1 to allVertices.Count - 1 do
                let v1 = allVertices.[v1Index]
                let v2 = allVertices.[v2Index]

                if not (v1.pt.Equals(v2.pt)) then
                    v1Index <- v2Index
                else
                    if not v1.innerLM || not v2.innerLM then
                        v1.innerLM <- false

                    for e in v2.edges do
                        if obj.ReferenceEquals(e.vB, v2) then e.vB <- v1 else e.vT <- v1
                        if obj.ReferenceEquals(e.vL, v2) then e.vL <- v1 else e.vR <- v1

                    v1.edges.AddRange(v2.edges)
                    v2.edges.Clear()

                    let mutable iE = 0
                    let mutable breakOuter = false
                    while iE < v1.edges.Count && not breakOuter do
                        let e1 = v1.edges.[iE]
                        if Delaunay.IsHorizontal(e1) || not (obj.ReferenceEquals(e1.vB, v1)) then
                            iE <- iE + 1
                        else
                            let mutable iE2 = iE + 1
                            let mutable breakInner = false
                            while iE2 < v1.edges.Count && not breakInner do
                                let e2 = v1.edges.[iE2]
                                if obj.ReferenceEquals(e2.vB, v1) &&
                                   e1.vT.pt.Y <> e2.vT.pt.Y &&
                                   InternalClipper.CrossProductSign(e1.vT.pt, v1.pt, e2.vT.pt) = 0 then
                                    if e1.vT.pt.Y < e2.vT.pt.Y then
                                        this.SplitEdge(e1, e2)
                                    else
                                        this.SplitEdge(e2, e1)
                                    breakInner <- true
                                    breakOuter <- true
                                else
                                    iE2 <- iE2 + 1
                            iE <- iE + 1

    member private this.SplitEdge(longE: Edge, shortE: Edge) : unit =
        let oldT = longE.vT
        let newT = shortE.vT

        Delaunay.RemoveEdgeFromVertex(oldT, longE)

        longE.vT <- newT
        if obj.ReferenceEquals(longE.vL, oldT) then longE.vL <- newT else longE.vR <- newT

        newT.edges.Add(longE)
        this.CreateEdge(newT, oldT, longE.kind) |> ignore

    member private this.RemoveIntersection(e1: Edge, e2: Edge) : bool =
        let mutable v = e1.vL
        let mutable tmpE = e2

        let mutable d = Delaunay.ShortestDistFromSegment(e1.vL.pt, e2.vL.pt, e2.vR.pt)
        let mutable d2 = Delaunay.ShortestDistFromSegment(e1.vR.pt, e2.vL.pt, e2.vR.pt)
        if d2 < d then
            d <- d2
            v <- e1.vR

        d2 <- Delaunay.ShortestDistFromSegment(e2.vL.pt, e1.vL.pt, e1.vR.pt)
        if d2 < d then
            d <- d2
            tmpE <- e1
            v <- e2.vL

        d2 <- Delaunay.ShortestDistFromSegment(e2.vR.pt, e1.vL.pt, e1.vR.pt)
        if d2 < d then
            d <- d2
            tmpE <- e1
            v <- e2.vR

        if d > 1.0 then false
        else
            let v2 = tmpE.vT
            Delaunay.RemoveEdgeFromVertex(v2, tmpE)

            if obj.ReferenceEquals(tmpE.vL, v2) then tmpE.vL <- v else tmpE.vR <- v
            tmpE.vT <- v
            v.edges.Add(tmpE)
            v.innerLM <- false

            if tmpE.vB.innerLM && Delaunay.GetLocMinAngle(tmpE.vB) <= 0.0 then
                tmpE.vB.innerLM <- false

            this.CreateEdge(v, v2, tmpE.kind) |> ignore
            true

    member private this.CreateEdge(v1: Vertex2, v2: Vertex2, k: EdgeKind) : Edge =
        let res = Edge()
        allEdges.Add(res)

        if v1.pt.Y = v2.pt.Y then
            res.vB <- v1
            res.vT <- v2
        elif v1.pt.Y < v2.pt.Y then
            res.vB <- v2
            res.vT <- v1
        else
            res.vB <- v1
            res.vT <- v2

        if v1.pt.X <= v2.pt.X then
            res.vL <- v1
            res.vR <- v2
        else
            res.vL <- v2
            res.vR <- v1

        res.kind <- k
        v1.edges.Add(res)
        v2.edges.Add(res)

        if k = EdgeKind.Loose then
            pendingDelaunayStack.Push(res)
            this.AddEdgeToActives(res)

        res

    member private this.CreateTriangle(e1: Edge, e2: Edge, e3: Edge) : Triangle =
        let tri = Triangle(e1, e2, e3)
        allTriangles.Add(tri)

        for i = 0 to 2 do
            let e = tri.edges.[i]
            if not (isNull e.triA) then
                e.triB <- tri
                this.RemoveEdgeFromActives(e)
            else
                e.triA <- tri
                if not (Delaunay.IsLooseEdge(e)) then
                    this.RemoveEdgeFromActives(e)
        tri

    member private this.ForceLegal(edge: Edge) : unit =
        if isNull edge.triA || isNull edge.triB then ()
        else
            let mutable vertA: Vertex2 = null
            let mutable vertB: Vertex2 = null

            let edgesA = Array.zeroCreate<Edge> 3
            let edgesB = Array.zeroCreate<Edge> 3

            for i = 0 to 2 do
                if not (obj.ReferenceEquals(edge.triA.edges.[i], edge)) then
                    let e = edge.triA.edges.[i]
                    match Delaunay.EdgeContains(e, edge.vL) with
                    | EdgeContainsResult.Left ->
                        edgesA.[1] <- e
                        vertA <- e.vR
                    | EdgeContainsResult.Right ->
                        edgesA.[1] <- e
                        vertA <- e.vL
                    | _ ->
                        edgesB.[1] <- e

            for i = 0 to 2 do
                if not (obj.ReferenceEquals(edge.triB.edges.[i], edge)) then
                    let e = edge.triB.edges.[i]
                    match Delaunay.EdgeContains(e, edge.vL) with
                    | EdgeContainsResult.Left ->
                        edgesA.[2] <- e
                        vertB <- e.vR
                    | EdgeContainsResult.Right ->
                        edgesA.[2] <- e
                        vertB <- e.vL
                    | _ ->
                        edgesB.[2] <- e

            if isNull vertA || isNull vertB then ()
            elif InternalClipper.CrossProductSign(vertA.pt, edge.vL.pt, edge.vR.pt) = 0 then ()
            else
                let ictResult = Delaunay.InCircleTest(vertA.pt, edge.vL.pt, edge.vR.pt, vertB.pt)
                if ictResult = 0.0 ||
                   (Delaunay.RightTurning(vertA.pt, edge.vL.pt, edge.vR.pt) = (ictResult < 0.0)) then ()
                else
                    edge.vL <- vertA
                    edge.vR <- vertB

                    edge.triA.edges.[0] <- edge
                    for i = 1 to 2 do
                        let eAi = edgesA.[i]
                        edge.triA.edges.[i] <- eAi
                        if Delaunay.IsLooseEdge(eAi) then
                            pendingDelaunayStack.Push(eAi)

                        if obj.ReferenceEquals(eAi.triA, edge.triA) || obj.ReferenceEquals(eAi.triB, edge.triA) then ()
                        elif obj.ReferenceEquals(eAi.triA, edge.triB) then
                            eAi.triA <- edge.triA
                        elif obj.ReferenceEquals(eAi.triB, edge.triB) then
                            eAi.triB <- edge.triA
                        else
                            raise (InvalidOperationException("oops"))

                    edge.triB.edges.[0] <- edge
                    for i = 1 to 2 do
                        let eBi = edgesB.[i]
                        edge.triB.edges.[i] <- eBi
                        if Delaunay.IsLooseEdge(eBi) then
                            pendingDelaunayStack.Push(eBi)

                        if obj.ReferenceEquals(eBi.triA, edge.triB) || obj.ReferenceEquals(eBi.triB, edge.triB) then ()
                        elif obj.ReferenceEquals(eBi.triA, edge.triA) then
                            eBi.triA <- edge.triB
                        elif obj.ReferenceEquals(eBi.triB, edge.triA) then
                            eBi.triB <- edge.triB
                        else
                            raise (InvalidOperationException("oops"))

    member private this.CreateInnerLocMinLooseEdge(vAbove: Vertex2) : Edge =
        if isNull firstActive then null
        else
            let xAbove = vAbove.pt.X
            let yAbove = vAbove.pt.Y

            let mutable e = firstActive
            let mutable eBelow: Edge = null
            let mutable bestD = -1.0

            while not (isNull e) do
                if e.vL.pt.X <= xAbove &&
                   e.vR.pt.X >= xAbove &&
                   e.vB.pt.Y >= yAbove &&
                   not (obj.ReferenceEquals(e.vB, vAbove)) &&
                   not (obj.ReferenceEquals(e.vT, vAbove)) &&
                   not (Delaunay.LeftTurning(e.vL.pt, vAbove.pt, e.vR.pt)) then
                    let d = Delaunay.ShortestDistFromSegment(vAbove.pt, e.vL.pt, e.vR.pt)
                    if isNull eBelow || d < bestD then
                        eBelow <- e
                        bestD <- d
                e <- e.nextE

            if isNull eBelow then null
            else
                let mutable vBest = if eBelow.vT.pt.Y <= yAbove then eBelow.vB else eBelow.vT
                let mutable xBest = vBest.pt.X
                let mutable yBest = vBest.pt.Y

                e <- firstActive
                if xBest < xAbove then
                    while not (isNull e) do
                        if e.vR.pt.X > xBest &&
                           e.vL.pt.X < xAbove &&
                           e.vB.pt.Y > yAbove &&
                           e.vT.pt.Y < yBest &&
                           Delaunay.SegsIntersect(e.vB.pt, e.vT.pt, vBest.pt, vAbove.pt) = IntersectKind.Intersect then
                            vBest <- if e.vT.pt.Y > yAbove then e.vT else e.vB
                            xBest <- vBest.pt.X
                            yBest <- vBest.pt.Y
                        e <- e.nextE
                else
                    while not (isNull e) do
                        if e.vR.pt.X < xBest &&
                           e.vL.pt.X > xAbove &&
                           e.vB.pt.Y > yAbove &&
                           e.vT.pt.Y < yBest &&
                           Delaunay.SegsIntersect(e.vB.pt, e.vT.pt, vBest.pt, vAbove.pt) = IntersectKind.Intersect then
                            vBest <- if e.vT.pt.Y > yAbove then e.vT else e.vB
                            xBest <- vBest.pt.X
                            yBest <- vBest.pt.Y
                        e <- e.nextE

                this.CreateEdge(vBest, vAbove, EdgeKind.Loose)

    member private this.HorizontalBetween(v1: Vertex2, v2: Vertex2) : Edge =
        let y = v1.pt.Y
        let mutable l = 0L
        let mutable r = 0L

        if v1.pt.X > v2.pt.X then
            l <- v2.pt.X
            r <- v1.pt.X
        else
            l <- v1.pt.X
            r <- v2.pt.X

        let mutable res = firstActive
        let mutable found = false
        while not (isNull res) && not found do
            if res.vL.pt.Y = y &&
               res.vR.pt.Y = y &&
               res.vL.pt.X >= l &&
               res.vR.pt.X <= r &&
               (res.vL.pt.X <> l || res.vL.pt.X <> r) then
                found <- true
            else
                res <- res.nextE
        res

    member private this.DoTriangulateLeft(edge: Edge, pivot: Vertex2, minY: int64) : unit =
        let mutable vAlt: Vertex2 = null
        let mutable eAlt: Edge = null

        let v = if obj.ReferenceEquals(edge.vB, pivot) then edge.vT else edge.vB

        for e in pivot.edges do
            if not (obj.ReferenceEquals(e, edge)) && e.isActive then
                let vX = if obj.ReferenceEquals(e.vT, pivot) then e.vB else e.vT
                if not (obj.ReferenceEquals(vX, v)) then
                    let cps = InternalClipper.CrossProductSign(v.pt, pivot.pt, vX.pt)
                    if cps = 0 then
                        if not ((v.pt.X > pivot.pt.X) = (pivot.pt.X > vX.pt.X)) then
                            vAlt <- vX
                            eAlt <- e
                    elif cps < 0 && (isNull vAlt || Delaunay.LeftTurning(vX.pt, pivot.pt, vAlt.pt)) then
                        vAlt <- vX
                        eAlt <- e

        if isNull vAlt || vAlt.pt.Y < minY || isNull eAlt then ()
        else
            if vAlt.pt.Y < pivot.pt.Y then
                if Delaunay.IsLeftEdge(eAlt) then ()
                else
                    let mutable eX = Delaunay.FindLinkingEdge(vAlt, v, vAlt.pt.Y < v.pt.Y)
                    if isNull eX then
                        if vAlt.pt.Y = v.pt.Y && v.pt.Y = minY && not (isNull (this.HorizontalBetween(vAlt, v))) then ()
                        else
                            eX <- this.CreateEdge(vAlt, v, EdgeKind.Loose)

                    if not (isNull eX) then
                        this.CreateTriangle(edge, eAlt, eX) |> ignore
                        if not (Delaunay.EdgeCompleted(eX)) then
                            this.DoTriangulateLeft(eX, vAlt, minY)
            elif vAlt.pt.Y > pivot.pt.Y then
                if Delaunay.IsRightEdge(eAlt) then ()
                else
                    let mutable eX = Delaunay.FindLinkingEdge(vAlt, v, vAlt.pt.Y < v.pt.Y)
                    if isNull eX then
                        if vAlt.pt.Y = v.pt.Y && v.pt.Y = minY && not (isNull (this.HorizontalBetween(vAlt, v))) then ()
                        else
                            eX <- this.CreateEdge(vAlt, v, EdgeKind.Loose)

                    if not (isNull eX) then
                        this.CreateTriangle(edge, eAlt, eX) |> ignore
                        if not (Delaunay.EdgeCompleted(eX)) then
                            this.DoTriangulateLeft(eX, vAlt, minY)
            else
                let mutable eX = Delaunay.FindLinkingEdge(vAlt, v, vAlt.pt.Y < v.pt.Y)
                if isNull eX then
                    if vAlt.pt.Y = v.pt.Y && v.pt.Y = minY && not (isNull (this.HorizontalBetween(vAlt, v))) then ()
                    else
                        eX <- this.CreateEdge(vAlt, v, EdgeKind.Loose)

                if not (isNull eX) then
                    this.CreateTriangle(edge, eAlt, eX) |> ignore
                    if not (Delaunay.EdgeCompleted(eX)) then
                        this.DoTriangulateLeft(eX, vAlt, minY)

    member private this.DoTriangulateRight(edge: Edge, pivot: Vertex2, minY: int64) : unit =
        let mutable vAlt: Vertex2 = null
        let mutable eAlt: Edge = null

        let v = if obj.ReferenceEquals(edge.vB, pivot) then edge.vT else edge.vB

        for e in pivot.edges do
            if not (obj.ReferenceEquals(e, edge)) && e.isActive then
                let vX = if obj.ReferenceEquals(e.vT, pivot) then e.vB else e.vT
                if not (obj.ReferenceEquals(vX, v)) then
                    let cps = InternalClipper.CrossProductSign(v.pt, pivot.pt, vX.pt)
                    if cps = 0 then
                        if not ((v.pt.X > pivot.pt.X) = (pivot.pt.X > vX.pt.X)) then
                            vAlt <- vX
                            eAlt <- e
                    elif cps > 0 && (isNull vAlt || Delaunay.RightTurning(vX.pt, pivot.pt, vAlt.pt)) then
                        vAlt <- vX
                        eAlt <- e

        if isNull vAlt || vAlt.pt.Y < minY || isNull eAlt then ()
        else
            if vAlt.pt.Y < pivot.pt.Y then
                if Delaunay.IsRightEdge(eAlt) then ()
                else
                    let mutable eX = Delaunay.FindLinkingEdge(vAlt, v, vAlt.pt.Y > v.pt.Y)
                    if isNull eX then
                        if vAlt.pt.Y = v.pt.Y && v.pt.Y = minY && not (isNull (this.HorizontalBetween(vAlt, v))) then ()
                        else
                            eX <- this.CreateEdge(vAlt, v, EdgeKind.Loose)

                    if not (isNull eX) then
                        this.CreateTriangle(edge, eX, eAlt) |> ignore
                        if not (Delaunay.EdgeCompleted(eX)) then
                            this.DoTriangulateRight(eX, vAlt, minY)
            elif vAlt.pt.Y > pivot.pt.Y then
                if Delaunay.IsLeftEdge(eAlt) then ()
                else
                    let mutable eX = Delaunay.FindLinkingEdge(vAlt, v, vAlt.pt.Y > v.pt.Y)
                    if isNull eX then
                        if vAlt.pt.Y = v.pt.Y && v.pt.Y = minY && not (isNull (this.HorizontalBetween(vAlt, v))) then ()
                        else
                            eX <- this.CreateEdge(vAlt, v, EdgeKind.Loose)

                    if not (isNull eX) then
                        this.CreateTriangle(edge, eX, eAlt) |> ignore
                        if not (Delaunay.EdgeCompleted(eX)) then
                            this.DoTriangulateRight(eX, vAlt, minY)
            else
                let mutable eX = Delaunay.FindLinkingEdge(vAlt, v, vAlt.pt.Y > v.pt.Y)
                if isNull eX then
                    if vAlt.pt.Y = v.pt.Y && v.pt.Y = minY && not (isNull (this.HorizontalBetween(vAlt, v))) then ()
                    else
                        eX <- this.CreateEdge(vAlt, v, EdgeKind.Loose)

                if not (isNull eX) then
                    this.CreateTriangle(edge, eX, eAlt) |> ignore
                    if not (Delaunay.EdgeCompleted(eX)) then
                        this.DoTriangulateRight(eX, vAlt, minY)
    member private this.AddEdgeToActives(edge: Edge) : unit =
        if not edge.isActive then
            edge.prevE <- null
            edge.nextE <- firstActive
            edge.isActive <- true
            if not (isNull firstActive) then
                firstActive.prevE <- edge
            firstActive <- edge
    member private this.RemoveEdgeFromActives(edge: Edge) : unit =
        Delaunay.RemoveEdgeFromVertex(edge.vB, edge)
        Delaunay.RemoveEdgeFromVertex(edge.vT, edge)
        let prev = edge.prevE
        let next = edge.nextE
        if not (isNull next) then next.prevE <- prev
        if not (isNull prev) then prev.nextE <- next
        edge.isActive <- false
        if firstActive = edge then firstActive <- next

    member internal this.Execute(paths: Paths64, sol: byref<Paths64>) : TriangulateResult =
        sol <- Paths64()

        if not (this.AddPaths(paths)) then
            TriangulateResult.NoPolygons
        else
            if lowermostVertex.innerLM then
                while locMinStack.Count > 0 do
                    let lm = locMinStack.Pop()
                    lm.innerLM <- not lm.innerLM

                for e in allEdges do
                    if e.kind = EdgeKind.Ascend then
                        e.kind <- EdgeKind.Descend
                    else
                        e.kind <- EdgeKind.Ascend
            else
                while locMinStack.Count > 0 do
                    locMinStack.Pop() |> ignore

            allEdges.Sort(fun a b -> a.vL.pt.X.CompareTo(b.vL.pt.X))

            if not (this.FixupEdgeIntersects()) then
                this.CleanUp()
                TriangulateResult.PathsIntersect
            else
                allVertices.Sort(fun a b ->
                    if a.pt.Y = b.pt.Y then a.pt.X.CompareTo(b.pt.X)
                    else b.pt.Y.CompareTo(a.pt.Y))

                this.MergeDupOrCollinearVertices()

                let mutable currY = allVertices.[0].pt.Y
                let mutable failed = false

                for v in allVertices do
                    if not failed && v.edges.Count > 0 then
                        if v.pt.Y <> currY then
                            while locMinStack.Count > 0 && not failed do
                                let lm = locMinStack.Pop()
                                let e = this.CreateInnerLocMinLooseEdge(lm)
                                if isNull e then
                                    failed <- true
                                else
                                    if Delaunay.IsHorizontal(e) then
                                        if obj.ReferenceEquals(e.vL, e.vB) then
                                            this.DoTriangulateLeft(e, e.vB, currY)
                                        else
                                            this.DoTriangulateRight(e, e.vB, currY)
                                    else
                                        this.DoTriangulateLeft(e, e.vB, currY)
                                        if not (Delaunay.EdgeCompleted(e)) then
                                            this.DoTriangulateRight(e, e.vB, currY)

                                    this.AddEdgeToActives(lm.edges.[0])
                                    this.AddEdgeToActives(lm.edges.[1])

                            while horzEdgeStack.Count > 0 && not failed do
                                let e = horzEdgeStack.Pop()
                                if not (Delaunay.EdgeCompleted(e)) then
                                    if obj.ReferenceEquals(e.vB, e.vL) then
                                        if Delaunay.IsLeftEdge(e) then
                                            this.DoTriangulateLeft(e, e.vB, currY)
                                    else
                                        if Delaunay.IsRightEdge(e) then
                                            this.DoTriangulateRight(e, e.vB, currY)

                            currY <- v.pt.Y

                        if not failed then
                            for i = v.edges.Count - 1 downto 0 do
                                if i < v.edges.Count then
                                    let e = v.edges.[i]
                                    if not (Delaunay.EdgeCompleted(e)) && not (Delaunay.IsLooseEdge(e)) then
                                        if obj.ReferenceEquals(v, e.vB) then
                                            if Delaunay.IsHorizontal(e) then
                                                horzEdgeStack.Push(e)
                                            if not v.innerLM then
                                                this.AddEdgeToActives(e)
                                        else
                                            if Delaunay.IsHorizontal(e) then
                                                horzEdgeStack.Push(e)
                                            elif Delaunay.IsLeftEdge(e) then
                                                this.DoTriangulateLeft(e, e.vB, v.pt.Y)
                                            else
                                                this.DoTriangulateRight(e, e.vB, v.pt.Y)

                            if v.innerLM then
                                locMinStack.Push(v)

                if failed then
                    this.CleanUp()
                    TriangulateResult.Fail
                else
                    while horzEdgeStack.Count > 0 do
                        let e = horzEdgeStack.Pop()
                        if not (Delaunay.EdgeCompleted(e)) && obj.ReferenceEquals(e.vB, e.vL) then
                            this.DoTriangulateLeft(e, e.vB, currY)

                    if useDelaunay then
                        while pendingDelaunayStack.Count > 0 do
                            let e = pendingDelaunayStack.Pop()
                            this.ForceLegal(e)

                    sol <- Paths64(allTriangles.Count)
                    for tri in allTriangles do
                        let p = Delaunay.PathFromTriangle(tri)
                        let cps = InternalClipper.CrossProductSign(p.[0], p.[1], p.[2])
                        if cps <> 0 then
                            if cps < 0 then p.Reverse()
                            sol.Add(p)

                    this.CleanUp()
                    TriangulateResult.Success

    // Static helpers
    static member private IsLooseEdge(e: Edge) : bool = e.kind = EdgeKind.Loose
    static member private IsLeftEdge(e: Edge) : bool = e.kind = EdgeKind.Ascend
    static member private IsRightEdge(e: Edge) : bool = e.kind = EdgeKind.Descend
    static member private IsHorizontal(e: Edge) : bool = e.vB.pt.Y = e.vT.pt.Y

    static member private LeftTurning(p1: Point64, p2: Point64, p3: Point64) : bool =
        InternalClipper.CrossProductSign(p1, p2, p3) < 0

    static member private RightTurning(p1: Point64, p2: Point64, p3: Point64) : bool =
        InternalClipper.CrossProductSign(p1, p2, p3) > 0

    static member private EdgeCompleted(edge: Edge) : bool =
        if isNull edge.triA then false
        elif not (isNull edge.triB) then true
        else edge.kind <> EdgeKind.Loose

    static member private EdgeContains(edge: Edge, v: Vertex2) : EdgeContainsResult =
        if edge.vL = v then EdgeContainsResult.Left
        elif edge.vR = v then EdgeContainsResult.Right
        else EdgeContainsResult.Neither

    static member private GetAngle(a: Point64, b: Point64, c: Point64) : float =
        let abx = float (b.X - a.X)
        let aby = float (b.Y - a.Y)
        let bcx = float (b.X - c.X)
        let bcy = float (b.Y - c.Y)
        let dp = abx * bcx + aby * bcy
        let cp = abx * bcy - aby * bcx
        Math.Atan2(cp, dp)

    static member private GetLocMinAngle(v: Vertex2) : float =
        let mutable asc = 0
        let mutable des = 0
        if v.edges.[0].kind = EdgeKind.Ascend then
            asc <- 0
            des <- 1
        else
            des <- 0
            asc <- 1
        Delaunay.GetAngle(v.edges.[des].vT.pt, v.pt, v.edges.[asc].vT.pt)

    static member private RemoveEdgeFromVertex(vert: Vertex2, edge: Edge) : unit =
        let idx = vert.edges.IndexOf(edge)
        if idx < 0 then raise (InvalidOperationException("oops!"))
        vert.edges.RemoveAt(idx)

    static member private FindLocMinIdx(path: Path64, len: int, idx: byref<int>) : bool =
        if len < 3 then false
        else
            let i0 = idx
            let mutable n = (idx + 1) % len
            let mutable cont = true
            let mutable result = true
            while cont && path.[n].Y <= path.[idx].Y do
                idx <- n
                n <- (n + 1) % len
                if idx = i0 then
                    cont <- false
                    result <- false
            if result then
                while path.[n].Y >= path.[idx].Y do
                    idx <- n
                    n <- (n + 1) % len
            result

    static member private Prev(idx: int, len: int) : int =
        if idx = 0 then len - 1 else idx - 1

    static member private Next(idx: int, len: int) : int =
        (idx + 1) % len

    static member private FindLinkingEdge(vert1: Vertex2, vert2: Vertex2, preferAscending: bool) : Edge =
        let mutable res: Edge = null
        let mutable i = 0
        let mutable found = false
        while i < vert1.edges.Count && not found do
            let e = vert1.edges.[i]
            if e.vL = vert2 || e.vR = vert2 then
                if e.kind = EdgeKind.Loose || ((e.kind = EdgeKind.Ascend) = preferAscending) then
                    res <- e
                    found <- true
                else
                    res <- e
            i <- i + 1
        res

    static member private PathFromTriangle(tri: Triangle) : Path64 =
        let res = Path64(3)
        res.Add(tri.edges.[0].vL.pt)
        res.Add(tri.edges.[0].vR.pt)
        let e = tri.edges.[1]
        if e.vL.pt.Equals(res.[0]) || e.vL.pt.Equals(res.[1]) then
            res.Add(e.vR.pt)
        else
            res.Add(e.vL.pt)
        res

    static member private InCircleTest(ptA: Point64, ptB: Point64, ptC: Point64, ptD: Point64) : float =
        let m00 = float (ptA.X - ptD.X)
        let m01 = float (ptA.Y - ptD.Y)
        let m02 = Delaunay.Sqr(m00) + Delaunay.Sqr(m01)
        let m10 = float (ptB.X - ptD.X)
        let m11 = float (ptB.Y - ptD.Y)
        let m12 = Delaunay.Sqr(m10) + Delaunay.Sqr(m11)
        let m20 = float (ptC.X - ptD.X)
        let m21 = float (ptC.Y - ptD.Y)
        let m22 = Delaunay.Sqr(m20) + Delaunay.Sqr(m21)
        m00 * (m11 * m22 - m21 * m12) -
        m10 * (m01 * m22 - m21 * m02) +
        m20 * (m01 * m12 - m11 * m02)

    static member private ShortestDistFromSegment(pt: Point64, segPt1: Point64, segPt2: Point64) : float =
        let dx = float (segPt2.X - segPt1.X)
        let dy = float (segPt2.Y - segPt1.Y)
        let ax = float (pt.X - segPt1.X)
        let ay = float (pt.Y - segPt1.Y)
        let qNum = ax * dx + ay * dy
        let denom = Delaunay.Sqr(dx) + Delaunay.Sqr(dy)
        if qNum < 0.0 then
            Delaunay.DistanceSqr(pt, segPt1)
        elif qNum > denom then
            Delaunay.DistanceSqr(pt, segPt2)
        else
            Delaunay.Sqr(ax * dy - dx * ay) / denom

    static member private SegsIntersect(s1a: Point64, s1b: Point64, s2a: Point64, s2b: Point64) : IntersectKind =
        if s1a = s2a || s1a = s2b || s1b = s2b then IntersectKind.None
        else
            let dy1 = float (s1b.Y - s1a.Y)
            let dx1 = float (s1b.X - s1a.X)
            let dy2 = float (s2b.Y - s2a.Y)
            let dx2 = float (s2b.X - s2a.X)
            let cp = dy1 * dx2 - dy2 * dx1
            if cp = 0.0 then IntersectKind.Collinear
            else
                let mutable t = (float (s1a.X - s2a.X) * dy2 -
                                 float (s1a.Y - s2a.Y) * dx2)
                let mutable result = IntersectKind.None
                let mutable checkSecond = true
                if t >= 0.0 then
                    if cp < 0.0 || t >= cp then
                        checkSecond <- false
                        result <- IntersectKind.None
                else
                    if cp > 0.0 || t <= cp then
                        checkSecond <- false
                        result <- IntersectKind.None
                if checkSecond then
                    t <- (float (s1a.X - s2a.X) * dy1 -
                          float (s1a.Y - s2a.Y) * dx1)
                    if t >= 0.0 then
                        if cp > 0.0 && t < cp then result <- IntersectKind.Intersect
                        else result <- IntersectKind.None
                    else
                        if cp < 0.0 && t > cp then result <- IntersectKind.Intersect
                        else result <- IntersectKind.None
                result

    static member private DistSqr(pt1: Point64, pt2: Point64) : float =
        Delaunay.Sqr(float (pt1.X - pt2.X)) + Delaunay.Sqr(float (pt1.Y - pt2.Y))

    static member private Sqr(v: float) : float = v * v

    static member private DistanceSqr(a: Point64, b: Point64) : float =
        let dx = float (a.X - b.X)
        let dy = float (a.Y - b.Y)
        dx * dx + dy * dy

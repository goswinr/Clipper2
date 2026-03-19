#if USINGZ
namespace Clipper2ZLib
#else
namespace Clipper2Lib
#endif

open System
open System.Collections.Generic
open System.Runtime.InteropServices

// ClipperEngine static helpers
[<AbstractClass; Sealed>]
type internal ClipperEngine private () =

    static member internal AddLocMin(vert: Vertex, polytype: PathType, isOpen: bool, minimaList: List<LocalMinima>) : unit =
        // make sure the vertex is added only once ...
        if (vert.flags &&& VertexFlags.LocalMin) <> VertexFlags.None then ()
        else
            vert.flags <- vert.flags ||| VertexFlags.LocalMin
            let lm = LocalMinima(vert, polytype, isOpen)
            minimaList.Add(lm)

    static member inline internal EnsureCapacity(list: List<'T>, minCapacity: int) : unit =
        if list.Capacity < minCapacity then
            list.Capacity <- minCapacity

    static member internal AddPathsToVertexList(paths: Paths64, polytype: PathType, isOpen: bool,
                                                 minimaList: List<LocalMinima>, vertexList: VertexPoolList) : unit =
        let mutable totalVertCnt = 0
        for path in paths do
            totalVertCnt <- totalVertCnt + path.Count
        vertexList.EnsureCapacity(vertexList.Count + totalVertCnt)

        for path in paths do
            let mutable v0: Vertex = null
            let mutable prev_v: Vertex = null
            for pt in path do
                if isNull v0 then
                    v0 <- vertexList.Add(pt, VertexFlags.None, null)
                    prev_v <- v0
                elif prev_v.pt <> pt then // ie skips duplicates
                    let curr_v = vertexList.Add(pt, VertexFlags.None, prev_v)
                    prev_v.next <- curr_v
                    prev_v <- curr_v

            let mutable skipPath = false
            if isNull prev_v || isNull prev_v.prev then
                skipPath <- true
            if not skipPath then
                if not isOpen && prev_v.pt = v0.pt then prev_v <- prev_v.prev
                prev_v.next <- v0
                v0.prev <- prev_v
                if not isOpen && Object.ReferenceEquals(prev_v.next, prev_v) then
                    skipPath <- true

            if not skipPath then
                // OK, we have a valid path
                let mutable going_up = false
                if isOpen then
                    let mutable curr_v = v0.next
                    while not (Object.ReferenceEquals(curr_v, v0)) && curr_v.pt.Y = v0.pt.Y do
                        curr_v <- curr_v.next
                    going_up <- curr_v.pt.Y <= v0.pt.Y
                    if going_up then
                        v0.flags <- VertexFlags.OpenStart
                        ClipperEngine.AddLocMin(v0, polytype, true, minimaList)
                    else
                        v0.flags <- VertexFlags.OpenStart ||| VertexFlags.LocalMax
                else // closed path
                    prev_v <- v0.prev
                    while not (Object.ReferenceEquals(prev_v, v0)) && prev_v.pt.Y = v0.pt.Y do
                        prev_v <- prev_v.prev
                    if Object.ReferenceEquals(prev_v, v0) then
                        skipPath <- true // only open paths can be completely flat
                    else
                        going_up <- prev_v.pt.Y > v0.pt.Y

                if not skipPath then
                    let going_up0 = going_up
                    let mutable going_up = going_up
                    prev_v <- v0
                    let mutable curr_v = v0.next
                    while not (Object.ReferenceEquals(curr_v, v0)) do
                        if curr_v.pt.Y > prev_v.pt.Y && going_up then
                            prev_v.flags <- prev_v.flags ||| VertexFlags.LocalMax
                            going_up <- false
                        elif curr_v.pt.Y < prev_v.pt.Y && not going_up then
                            going_up <- true
                            ClipperEngine.AddLocMin(prev_v, polytype, isOpen, minimaList)
                        prev_v <- curr_v
                        curr_v <- curr_v.next

                    if isOpen then
                        prev_v.flags <- prev_v.flags ||| VertexFlags.OpenEnd
                        if going_up then
                            prev_v.flags <- prev_v.flags ||| VertexFlags.LocalMax
                        else
                            ClipperEngine.AddLocMin(prev_v, polytype, isOpen, minimaList)
                    elif going_up <> going_up0 then
                        if going_up0 then ClipperEngine.AddLocMin(prev_v, polytype, false, minimaList)
                        else prev_v.flags <- prev_v.flags ||| VertexFlags.LocalMax

// ReuseableDataContainer64
type ReuseableDataContainer64() =
    let _minimaList = List<LocalMinima>()
    let _vertexList = VertexPoolList()

    member internal _.MinimaList = _minimaList
    member internal _.VertexList = _vertexList

    member _.Clear() =
        _minimaList.Clear()
        _vertexList.Clear()

    member _.AddPaths(paths: Paths64, pt: PathType, isOpen: bool) =
        ClipperEngine.AddPathsToVertexList(paths, pt, isOpen, _minimaList, _vertexList)

#if USINGZ
type ZCallback64 = delegate of Point64 * Point64 * Point64 * Point64 * byref<Point64> -> unit
#endif

// ClipperBase
[<AllowNullLiteral>]
type ClipperBase() =
    let mutable _cliptype = ClipType.NoClip
    let mutable _fillrule = FillRule.EvenOdd
    let mutable _actives: Active = null
    let mutable _sel: Active = null
    let _freeActives = Stack<Active>()
    let _minimaList = List<LocalMinima>()
    let _intersectList = List<IntersectNode>()
    let _vertexList = VertexPoolList()
    let _outrecList = OutRecPoolList()
    let _scanlineList = List<int64>()
    let _horzSegList = List<HorzSegment>()
    let _horzJoinList = HorzJoinPoolList()
    let _outPtPool = OutPtPoolList()
    let mutable _currentLocMin = 0
    let mutable _currentBotY = 0L
    let mutable _isSortedMinimaList = false
    let mutable _hasOpenPaths = false

    member val internal _using_polytree = false with get, set
    member val internal _succeeded = false with get, set
    member val PreserveCollinear = true with get, set
    member val ReverseSolution = false with get, set

#if USINGZ
    member val DefaultZ = 0L with get, set
    member val internal _zCallback: ZCallback64 = null with get, set

    member private this.XYCoordsEqual(pt1: Point64, pt2: Point64) : bool =
        pt1.X = pt2.X && pt1.Y = pt2.Y

    member private this.SetZ(e1: Active, e2: Active, intersectPt: byref<Point64>) : unit =
        if isNull this._zCallback then ()
        else
            if e1.localMin.polytype = PathType.Subject then
                if this.XYCoordsEqual(intersectPt, e1.bot) then
                    intersectPt <- Point64(intersectPt.X, intersectPt.Y, e1.bot.Z)
                elif this.XYCoordsEqual(intersectPt, e1.top) then
                    intersectPt <- Point64(intersectPt.X, intersectPt.Y, e1.top.Z)
                elif this.XYCoordsEqual(intersectPt, e2.bot) then
                    intersectPt <- Point64(intersectPt.X, intersectPt.Y, e2.bot.Z)
                elif this.XYCoordsEqual(intersectPt, e2.top) then
                    intersectPt <- Point64(intersectPt.X, intersectPt.Y, e2.top.Z)
                else
                    intersectPt <- Point64(intersectPt.X, intersectPt.Y, this.DefaultZ)
                this._zCallback.Invoke(e1.bot, e1.top, e2.bot, e2.top, &intersectPt)
            else
                if this.XYCoordsEqual(intersectPt, e2.bot) then
                    intersectPt <- Point64(intersectPt.X, intersectPt.Y, e2.bot.Z)
                elif this.XYCoordsEqual(intersectPt, e2.top) then
                    intersectPt <- Point64(intersectPt.X, intersectPt.Y, e2.top.Z)
                elif this.XYCoordsEqual(intersectPt, e1.bot) then
                    intersectPt <- Point64(intersectPt.X, intersectPt.Y, e1.bot.Z)
                elif this.XYCoordsEqual(intersectPt, e1.top) then
                    intersectPt <- Point64(intersectPt.X, intersectPt.Y, e1.top.Z)
                else
                    intersectPt <- Point64(intersectPt.X, intersectPt.Y, this.DefaultZ)
                this._zCallback.Invoke(e2.bot, e2.top, e1.bot, e1.top, &intersectPt)
#endif

    member this.ClearSolutionOnly() : unit =
        while not (isNull _actives) do
            this.DeleteFromAEL(_actives)
        _scanlineList.Clear()
        this.DisposeIntersectNodes()
        _outrecList.Clear()
        _horzSegList.Clear()
        _horzJoinList.Clear()
        _outPtPool.Clear()
        _freeActives.Clear()

    member this.Clear() : unit =
        this.ClearSolutionOnly()
        _minimaList.Clear()
        _vertexList.Clear()
        _currentLocMin <- 0
        _isSortedMinimaList <- false
        _hasOpenPaths <- false

    member internal this.Reset() : unit =
        if not _isSortedMinimaList then
            _minimaList.Sort(LocMinSorter())
            _isSortedMinimaList <- true

        ClipperEngine.EnsureCapacity(_scanlineList, _minimaList.Count)
        for i = _minimaList.Count - 1 downto 0 do
            _scanlineList.Add(_minimaList.[i].vertex.pt.Y)

        _currentBotY <- 0L
        _currentLocMin <- 0
        _actives <- null
        _sel <- null
        this._succeeded <- true

    member private this.InsertScanline(y: int64) : unit =
        let index = _scanlineList.BinarySearch(y)
        if index >= 0 then ()
        else
            _scanlineList.Insert(~~~index, y)

    member private this.PopScanline(y: byref<int64>) : bool =
        let mutable cnt = _scanlineList.Count - 1
        if cnt < 0 then
            y <- 0L
            false
        else
            y <- _scanlineList.[cnt]
            _scanlineList.RemoveAt(cnt)
            cnt <- cnt - 1
            while cnt >= 0 && y = _scanlineList.[cnt] do
                _scanlineList.RemoveAt(cnt)
                cnt <- cnt - 1
            true

    member private this.HasLocMinAtY(y: int64) : bool =
        _currentLocMin < _minimaList.Count && _minimaList.[_currentLocMin].vertex.pt.Y = y

    member private this.PopLocalMinima() : LocalMinima =
        let result = _minimaList.[_currentLocMin]
        _currentLocMin <- _currentLocMin + 1
        result

    member this.AddSubject(path: Path64) : unit = this.AddPath(path, PathType.Subject)
    member this.AddOpenSubject(path: Path64) : unit = this.AddPath(path, PathType.Subject, true)
    member this.AddClip(path: Path64) : unit = this.AddPath(path, PathType.Clip)

    member internal this.AddPath(path: Path64, polytype: PathType,
                                 [<Optional; DefaultParameterValue(false)>] isOpen: bool) : unit =
        let tmp = Paths64(1)
        tmp.Add(path)
        this.AddPaths(tmp, polytype, isOpen)

    member internal this.AddPaths(paths: Paths64, polytype: PathType,
                                  [<Optional; DefaultParameterValue(false)>] isOpen: bool) : unit =
        if isOpen then _hasOpenPaths <- true
        _isSortedMinimaList <- false
        ClipperEngine.AddPathsToVertexList(paths, polytype, isOpen, _minimaList, _vertexList)

    member internal this.AddReuseableData(reuseableData: ReuseableDataContainer64) : unit =
        if reuseableData.MinimaList.Count = 0 then ()
        else
            _isSortedMinimaList <- false
            for lm in reuseableData.MinimaList do
                _minimaList.Add(LocalMinima(lm.vertex, lm.polytype, lm.isOpen))
                if lm.isOpen then _hasOpenPaths <- true

    member internal this.ExecuteInternal(ct: ClipType, fillRule: FillRule) : unit =
        if ct = ClipType.NoClip then ()
        else
            _fillrule <- fillRule
            _cliptype <- ct
            this.Reset()
            let mutable y = 0L
            if this.PopScanline(&y) then
                let mutable done_ = false
                while this._succeeded && not done_ do
                    this.InsertLocalMinimaIntoAEL(y)
                    let mutable ae: Active = null
                    while this.PopHorz(&ae) do
                        this.DoHorizontal(ae)
                    if _horzSegList.Count > 0 then
                        this.ConvertHorzSegsToJoins()
                        _horzSegList.Clear()
                    _currentBotY <- y // bottom of scanbeam
                    if not (this.PopScanline(&y)) then
                        done_ <- true
                    else
                        this.DoIntersections(y)
                        this.DoTopOfScanbeam(y)
                        let mutable ae2: Active = null
                        while this.PopHorz(&ae2) do
                            this.DoHorizontal(ae2)

                if this._succeeded then
                    this.ProcessHorzJoins()

    member private this.DeleteFromAEL(ae: Active) : unit =
        let prev = ae.prevInAEL
        let next = ae.nextInAEL
        if isNull prev && isNull next && not (obj.ReferenceEquals(ae, _actives)) then ()
        else
            if not (isNull prev) then
                prev.nextInAEL <- next
            else
                _actives <- next
            if not (isNull next) then
                next.prevInAEL <- prev
            this.PoolDeletedActive(ae)

    member private this.PoolDeletedActive(ae: Active) : unit =
        ae.bot <- Point64()
        ae.top <- Point64()
        ae.dx <- 0.0
        ae.windCount <- 0
        ae.windCount2 <- 0
        ae.outrec <- null
        ae.prevInAEL <- null
        ae.nextInAEL <- null
        ae.prevInSEL <- null
        ae.nextInSEL <- null
        ae.jump <- null
        ae.vertexTop <- null
        ae.localMin <- LocalMinima()
        ae.isLeftBound <- false
        ae.joinWith <- JoinWith.None
        _freeActives.Push(ae)

    member private this.AdjustCurrXAndCopyToSEL(topY: int64) : unit =
        let mutable ae = _actives
        _sel <- ae
        while not (isNull ae) do
            ae.prevInSEL <- ae.prevInAEL
            ae.nextInSEL <- ae.nextInAEL
            ae.jump <- ae.nextInSEL
            ae.curX <- ClipperBase.TopX(ae, topY)
            ae <- ae.nextInAEL

    member private this.DoIntersections(topY: int64) : unit =
        if this.BuildIntersectList(topY) then
            this.ProcessIntersectList()
            this.DisposeIntersectNodes()

    member private this.DisposeIntersectNodes() : unit =
        _intersectList.Clear()

    member private this.AddNewIntersectNode(ae1: Active, ae2: Active, topY: int64) : unit =
        let mutable ip = Point64()
        if not (InternalClipper.GetLineIntersectPt(ae1.bot, ae1.top, ae2.bot, ae2.top, &ip)) then
            ip <- Point64(ae1.curX, topY)

        if ip.Y > _currentBotY || ip.Y < topY then
            let absDx1 = Math.Abs(ae1.dx)
            let absDx2 = Math.Abs(ae2.dx)
            if absDx1 > 100.0 && absDx2 > 100.0 then
                if absDx1 > absDx2 then
                    ip <- InternalClipper.GetClosestPtOnSegment(ip, ae1.bot, ae1.top)
                else
                    ip <- InternalClipper.GetClosestPtOnSegment(ip, ae2.bot, ae2.top)
            elif absDx1 > 100.0 then
                ip <- InternalClipper.GetClosestPtOnSegment(ip, ae1.bot, ae1.top)
            elif absDx2 > 100.0 then
                ip <- InternalClipper.GetClosestPtOnSegment(ip, ae2.bot, ae2.top)
            else
                if ip.Y < topY then ip.Y <- topY
                else ip.Y <- _currentBotY
                if absDx1 < absDx2 then ip.X <- ClipperBase.TopX(ae1, ip.Y)
                else ip.X <- ClipperBase.TopX(ae2, ip.Y)

        _intersectList.Add(IntersectNode(ip, ae1, ae2))

    static member private ExtractFromSEL(ae: Active) : Active =
        let res = ae.nextInSEL
        if not (isNull res) then
            res.prevInSEL <- ae.prevInSEL
        ae.prevInSEL.nextInSEL <- res
        res

    static member private Insert1Before2InSEL(ae1: Active, ae2: Active) : unit =
        ae1.prevInSEL <- ae2.prevInSEL
        if not (isNull ae1.prevInSEL) then
            ae1.prevInSEL.nextInSEL <- ae1
        ae1.nextInSEL <- ae2
        ae2.prevInSEL <- ae1

    member private this.BuildIntersectList(topY: int64) : bool =
        if isNull _actives || isNull _actives.nextInAEL then false
        else
            this.AdjustCurrXAndCopyToSEL(topY)
            let mutable left = _sel

            while not (isNull left) && not (isNull left.jump) do
                let mutable prevBase: Active = null
                while not (isNull left) && not (isNull left.jump) do
                    let mutable currBase = left
                    let mutable right = left.jump
                    let mutable lEnd = right
                    let rEnd = right.jump
                    left.jump <- rEnd
                    while not (obj.ReferenceEquals(left, lEnd)) && not (obj.ReferenceEquals(right, rEnd)) do
                        if right.curX < left.curX then
                            let mutable tmp = right.prevInSEL
                            let mutable done_ = false
                            while not done_ do
                                this.AddNewIntersectNode(tmp, right, topY)
                                if obj.ReferenceEquals(tmp, left) then
                                    done_ <- true
                                else
                                    tmp <- tmp.prevInSEL

                            let tmp2 = right
                            right <- ClipperBase.ExtractFromSEL(tmp2)
                            lEnd <- right
                            ClipperBase.Insert1Before2InSEL(tmp2, left)
                            if obj.ReferenceEquals(left, currBase) then
                                currBase <- tmp2
                                currBase.jump <- rEnd
                                if isNull prevBase then _sel <- currBase
                                else prevBase.jump <- currBase
                        else
                            left <- left.nextInSEL

                    prevBase <- currBase
                    left <- rEnd
                left <- _sel

            _intersectList.Count > 0

    member private this.ProcessIntersectList() : unit =
        _intersectList.Sort(fun a b ->
            if a.pt.Y <> b.pt.Y then
                if a.pt.Y > b.pt.Y then -1 else 1
            elif a.pt.X = b.pt.X then 0
            elif a.pt.X < b.pt.X then -1
            else 1)

        for i = 0 to _intersectList.Count - 1 do
            if not (ClipperBase.EdgesAdjacentInAEL(_intersectList.[i])) then
                let mutable j = i + 1
                while not (ClipperBase.EdgesAdjacentInAEL(_intersectList.[j])) do
                    j <- j + 1
                let tmp = _intersectList.[j]
                _intersectList.[j] <- _intersectList.[i]
                _intersectList.[i] <- tmp

            let node = _intersectList.[i]
            this.IntersectEdges(node.edge1, node.edge2, node.pt)
            this.SwapPositionsInAEL(node.edge1, node.edge2)

            node.edge1.curX <- node.pt.X
            node.edge2.curX <- node.pt.X
            this.CheckJoinLeft(node.edge2, node.pt, true)
            this.CheckJoinRight(node.edge1, node.pt, true)

    static member private ResetHorzDirection(horz: Active, vertexMax: Vertex, leftX: byref<int64>, rightX: byref<int64>) : bool =
        if horz.bot.X = horz.top.X then
            leftX <- horz.curX
            rightX <- horz.curX
            let mutable ae = horz.nextInAEL
            while not (isNull ae) && not (obj.ReferenceEquals(ae.vertexTop, vertexMax)) do
                ae <- ae.nextInAEL
            not (isNull ae)
        elif horz.curX < horz.top.X then
            leftX <- horz.curX
            rightX <- horz.top.X
            true
        else
            leftX <- horz.top.X
            rightX <- horz.curX
            false

    member private this.TrimHorz(horzEdge: Active, preserveCollinear: bool) : unit =
        let mutable wasTrimmed = false
        let mutable pt = ClipperBase.NextVertex(horzEdge).pt

        while pt.Y = horzEdge.top.Y do
            if preserveCollinear &&
               ((pt.X < horzEdge.top.X) <> (horzEdge.bot.X < horzEdge.top.X)) then
                pt <- Point64(pt.X, pt.Y + 1L)
            else
                horzEdge.vertexTop <- ClipperBase.NextVertex(horzEdge)
                horzEdge.top <- pt
                wasTrimmed <- true
                if ClipperBase.IsMaxima(horzEdge) then
                    pt <- Point64(pt.X, pt.Y + 1L)
                else
                    pt <- ClipperBase.NextVertex(horzEdge).pt
        if wasTrimmed then
            ClipperBase.SetDx(horzEdge)

    member private this.AddToHorzSegList(op: OutPt) : unit =
        if op.outrec.isOpen then ()
        else
            _horzSegList.Add(HorzSegment(op))

    static member private GetLastOp(hotEdge: Active) : OutPt =
        let outrec = hotEdge.outrec
        if obj.ReferenceEquals(hotEdge, outrec.frontEdge) then outrec.pts
        else outrec.pts.next

    member private this.DoHorizontal(horz: Active) : unit =
        let horzIsOpen = ClipperBase.IsOpen(horz)
        let y = horz.bot.Y
        let vertexMax =
            if horzIsOpen then ClipperBase.GetCurrYMaximaVertex_Open(horz)
            else ClipperBase.GetCurrYMaximaVertex(horz)

        let mutable leftX = 0L
        let mutable rightX = 0L
        let mutable isLeftToRight = ClipperBase.ResetHorzDirection(horz, vertexMax, &leftX, &rightX)

        if ClipperBase.IsHotEdge(horz) then
#if USINGZ
            let op = this.AddOutPt(horz, Point64(horz.curX, y, horz.bot.Z))
#else
            let op = this.AddOutPt(horz, Point64(horz.curX, y))
#endif
            this.AddToHorzSegList(op)

        let mutable loop_ = true
        while loop_ do
            let mutable ae = if isLeftToRight then horz.nextInAEL else horz.prevInAEL

            while not (isNull ae) do
                if obj.ReferenceEquals(ae.vertexTop, vertexMax) then
                    if ClipperBase.IsHotEdge(horz) && this.IsJoined(ae) then this.Split(ae, ae.top)

                    if ClipperBase.IsHotEdge(horz) then
                        while not (obj.ReferenceEquals(horz.vertexTop, vertexMax)) do
                            this.AddOutPt(horz, horz.top) |> ignore
                            this.UpdateEdgeIntoAEL(horz)
                        if isLeftToRight then
                            this.AddLocalMaxPoly(horz, ae, horz.top) |> ignore
                        else
                            this.AddLocalMaxPoly(ae, horz, horz.top) |> ignore
                    this.DeleteFromAEL(ae)
                    this.DeleteFromAEL(horz)
                    ae <- null
                    loop_ <- false
                else
                    let mutable shouldBreak = false
                    let mutable pt = Point64()
                    if not (obj.ReferenceEquals(vertexMax, horz.vertexTop)) || ClipperBase.IsOpenEnd(horz) then
                        if (isLeftToRight && ae.curX > rightX) || (not isLeftToRight && ae.curX < leftX) then
                            shouldBreak <- true
                        elif ae.curX = horz.top.X && not (ClipperBase.IsHorizontal(ae)) then
                            pt <- ClipperBase.NextVertex(horz).pt
                            if ClipperBase.IsOpen(ae) && not (ClipperBase.IsSamePolyType(ae, horz)) && not (ClipperBase.IsHotEdge(ae)) then
                                if (isLeftToRight && ClipperBase.TopX(ae, pt.Y) > pt.X) ||
                                   (not isLeftToRight && ClipperBase.TopX(ae, pt.Y) < pt.X) then
                                    shouldBreak <- true
                            elif (isLeftToRight && ClipperBase.TopX(ae, pt.Y) >= pt.X) ||
                                 (not isLeftToRight && ClipperBase.TopX(ae, pt.Y) <= pt.X) then
                                shouldBreak <- true

                    if shouldBreak then
                        ae <- null
                    else
                        pt <- Point64(ae.curX, y)
                        if isLeftToRight then
                            this.IntersectEdges(horz, ae, pt)
                            this.SwapPositionsInAEL(horz, ae)
                            this.CheckJoinLeft(ae, pt)
                            horz.curX <- ae.curX
                            ae <- horz.nextInAEL
                        else
                            this.IntersectEdges(ae, horz, pt)
                            this.SwapPositionsInAEL(ae, horz)
                            this.CheckJoinRight(ae, pt)
                            horz.curX <- ae.curX
                            ae <- horz.prevInAEL

                        if ClipperBase.IsHotEdge(horz) then
                            this.AddToHorzSegList(ClipperBase.GetLastOp(horz))

            if loop_ then
                if horzIsOpen && ClipperBase.IsOpenEnd(horz) then
                    if ClipperBase.IsHotEdge(horz) then
                        this.AddOutPt(horz, horz.top) |> ignore
                        if ClipperBase.IsFront(horz) then
                            horz.outrec.frontEdge <- null
                        else
                            horz.outrec.backEdge <- null
                        horz.outrec <- null
                    this.DeleteFromAEL(horz)
                    loop_ <- false
                elif ClipperBase.NextVertex(horz).pt.Y <> horz.top.Y then
                    loop_ <- false
                else
                    if ClipperBase.IsHotEdge(horz) then
                        this.AddOutPt(horz, horz.top) |> ignore

                    this.UpdateEdgeIntoAEL(horz)
                    isLeftToRight <- ClipperBase.ResetHorzDirection(horz, vertexMax, &leftX, &rightX)

        if not (isNull horz.outrec) && ClipperBase.IsHotEdge(horz) then
            let op = this.AddOutPt(horz, horz.top)
            this.AddToHorzSegList(op)

        if not (isNull horz.vertexTop) && horz.top.Y = horz.vertexTop.pt.Y then
            this.UpdateEdgeIntoAEL(horz)

    member private this.DoTopOfScanbeam(y: int64) : unit =
        _sel <- null
        let mutable ae = _actives
        while not (isNull ae) do
            if ae.top.Y = y then
                ae.curX <- ae.top.X
                if ClipperBase.IsMaxima(ae) then
                    ae <- this.DoMaxima(ae)
                else
                    if ClipperBase.IsHotEdge(ae) then
                        this.AddOutPt(ae, ae.top) |> ignore
                    this.UpdateEdgeIntoAEL(ae)
                    if ClipperBase.IsHorizontal(ae) then
                        this.PushHorz(ae)
                    ae <- ae.nextInAEL
            else
                ae.curX <- ClipperBase.TopX(ae, y)
                ae <- ae.nextInAEL

    member private this.DoMaxima(ae: Active) : Active =
        let prevE = ae.prevInAEL
        let mutable nextE = ae.nextInAEL

        if ClipperBase.IsOpenEnd(ae) then
            if ClipperBase.IsHotEdge(ae) then this.AddOutPt(ae, ae.top) |> ignore
            if ClipperBase.IsHorizontal(ae) then nextE
            else
                if ClipperBase.IsHotEdge(ae) then
                    if ClipperBase.IsFront(ae) then
                        ae.outrec.frontEdge <- null
                    else
                        ae.outrec.backEdge <- null
                    ae.outrec <- null
                this.DeleteFromAEL(ae)
                nextE
        else
            let maxPair = ClipperBase.GetMaximaPair(ae)
            if isNull maxPair then nextE
            else
                if this.IsJoined(ae) then this.Split(ae, ae.top)
                if this.IsJoined(maxPair) then this.Split(maxPair, maxPair.top)

                while not (obj.ReferenceEquals(nextE, maxPair)) do
                    this.IntersectEdges(ae, nextE, ae.top)
                    this.SwapPositionsInAEL(ae, nextE)
                    nextE <- ae.nextInAEL

                if ClipperBase.IsOpen(ae) then
                    if ClipperBase.IsHotEdge(ae) then
                        this.AddLocalMaxPoly(ae, maxPair, ae.top) |> ignore
                    this.DeleteFromAEL(maxPair)
                    this.DeleteFromAEL(ae)
                    if not (isNull prevE) then prevE.nextInAEL else _actives
                else
                    if ClipperBase.IsHotEdge(ae) then
                        this.AddLocalMaxPoly(ae, maxPair, ae.top) |> ignore
                    this.DeleteFromAEL(ae)
                    this.DeleteFromAEL(maxPair)
                    if not (isNull prevE) then prevE.nextInAEL else _actives

    static member private IsOdd(value: int) : bool =
        (value &&& 1) <> 0

    static member private IsHotEdge(ae: Active) : bool =
        not (isNull ae.outrec)

    static member private IsOpen(ae: Active) : bool =
        ae.localMin.isOpen

    static member private IsOpenEnd(ae: Active) : bool =
        ae.localMin.isOpen && ClipperBase.IsOpenEnd(ae.vertexTop)

    static member private IsOpenEnd(v: Vertex) : bool =
        (v.flags &&& (VertexFlags.OpenStart ||| VertexFlags.OpenEnd)) <> VertexFlags.None

    static member private GetPrevHotEdge(ae: Active) : Active =
        let mutable prev = ae.prevInAEL
        while not (isNull prev) && (ClipperBase.IsOpen(prev) || not (ClipperBase.IsHotEdge(prev))) do
            prev <- prev.prevInAEL
        prev

    static member private IsFront(ae: Active) : bool =
        obj.ReferenceEquals(ae, ae.outrec.frontEdge)

    static member private GetDx(pt1: Point64, pt2: Point64) : float =
        let dy = float (pt2.Y - pt1.Y)
        if dy <> 0.0 then
            float (pt2.X - pt1.X) / dy
        else if pt2.X > pt1.X then
            Double.NegativeInfinity
        else
            Double.PositiveInfinity

    static member private TopX(ae: Active, currentY: int64) : int64 =
        if currentY = ae.top.Y || ae.top.X = ae.bot.X then ae.top.X
        elif currentY = ae.bot.Y then ae.bot.X
        else
            ae.bot.X + int64 (Math.Round(ae.dx * float (currentY - ae.bot.Y), MidpointRounding.ToEven))

    static member private IsHorizontal(ae: Active) : bool =
        ae.top.Y = ae.bot.Y

    static member private IsHeadingRightHorz(ae: Active) : bool =
        Double.IsNegativeInfinity(ae.dx)

    static member private IsHeadingLeftHorz(ae: Active) : bool =
        Double.IsPositiveInfinity(ae.dx)

    static member private SwapActives(ae1: byref<Active>, ae2: byref<Active>) : unit =
        let tmp = ae1
        ae1 <- ae2
        ae2 <- tmp

    static member private GetPolyType(ae: Active) : PathType =
        ae.localMin.polytype

    static member private IsSamePolyType(ae1: Active, ae2: Active) : bool =
        ae1.localMin.polytype = ae2.localMin.polytype

    static member private SetDx(ae: Active) : unit =
        ae.dx <- ClipperBase.GetDx(ae.bot, ae.top)

    static member private NextVertex(ae: Active) : Vertex =
        if ae.windDx > 0 then ae.vertexTop.next else ae.vertexTop.prev

    static member private PrevPrevVertex(ae: Active) : Vertex =
        if ae.windDx > 0 then ae.vertexTop.prev.prev else ae.vertexTop.next.next

    static member private IsMaxima(vertex: Vertex) : bool =
        (vertex.flags &&& VertexFlags.LocalMax) <> VertexFlags.None

    static member private IsMaxima(ae: Active) : bool =
        ClipperBase.IsMaxima(ae.vertexTop)

    static member private GetMaximaPair(ae: Active) : Active =
        let mutable ae2 = ae.nextInAEL
        let mutable found = false
        while not (isNull ae2) && not found do
            if obj.ReferenceEquals(ae2.vertexTop, ae.vertexTop) then
                found <- true
            else
                ae2 <- ae2.nextInAEL
        ae2

    static member private GetCurrYMaximaVertex_Open(ae: Active) : Vertex =
        let mutable result = ae.vertexTop
        if ae.windDx > 0 then
            while result.next.pt.Y = result.pt.Y &&
                  ((result.flags &&& (VertexFlags.OpenEnd ||| VertexFlags.LocalMax)) = VertexFlags.None) do
                result <- result.next
        else
            while result.prev.pt.Y = result.pt.Y &&
                  ((result.flags &&& (VertexFlags.OpenEnd ||| VertexFlags.LocalMax)) = VertexFlags.None) do
                result <- result.prev
        if not (ClipperBase.IsMaxima(result)) then null else result

    static member private GetCurrYMaximaVertex(ae: Active) : Vertex =
        let mutable result = ae.vertexTop
        if ae.windDx > 0 then
            while result.next.pt.Y = result.pt.Y do
                result <- result.next
        else
            while result.prev.pt.Y = result.pt.Y do
                result <- result.prev
        if not (ClipperBase.IsMaxima(result)) then null else result

    static member private SetSides(outrec: OutRec, startEdge: Active, endEdge: Active) : unit =
        outrec.frontEdge <- startEdge
        outrec.backEdge <- endEdge

    static member private SwapOutrecs(ae1: Active, ae2: Active) : unit =
        let or1 = ae1.outrec
        let or2 = ae2.outrec
        if obj.ReferenceEquals(or1, or2) then
            let ae = or1.frontEdge
            or1.frontEdge <- or1.backEdge
            or1.backEdge <- ae
        else
            if not (isNull or1) then
                if obj.ReferenceEquals(ae1, or1.frontEdge) then
                    or1.frontEdge <- ae2
                else
                    or1.backEdge <- ae2

            if not (isNull or2) then
                if obj.ReferenceEquals(ae2, or2.frontEdge) then
                    or2.frontEdge <- ae1
                else
                    or2.backEdge <- ae1

            ae1.outrec <- or2
            ae2.outrec <- or1

    static member private SetOwner(outrec: OutRec, newOwner: OutRec) : unit =
        while not (isNull newOwner.owner) && isNull newOwner.owner.pts do
            newOwner.owner <- newOwner.owner.owner

        let mutable tmp = newOwner
        while not (isNull tmp) && not (obj.ReferenceEquals(tmp, outrec)) do
            tmp <- tmp.owner
        if not (isNull tmp) then
            newOwner.owner <- outrec.owner
        outrec.owner <- newOwner

    static member private Area(op: OutPt) : float =
        let mutable area = 0.0
        let mutable op2 = op
        let mutable done_ = false
        while not done_ do
            area <- area + float (op2.prev.pt.Y + op2.pt.Y) * float (op2.prev.pt.X - op2.pt.X)
            op2 <- op2.next
            done_ <- obj.ReferenceEquals(op2, op)
        area * 0.5

    static member private AreaTriangle(pt1: Point64, pt2: Point64, pt3: Point64) : float =
        float (pt3.Y + pt1.Y) * float (pt3.X - pt1.X) +
        float (pt1.Y + pt2.Y) * float (pt1.X - pt2.X) +
        float (pt2.Y + pt3.Y) * float (pt2.X - pt3.X)

    static member private GetRealOutRec(outRec: OutRec) : OutRec =
        let mutable outRec = outRec
        while not (isNull outRec) && isNull outRec.pts do
            outRec <- outRec.owner
        outRec

    static member private IsValidOwner(outRec: OutRec, testOwner: OutRec) : bool =
        let mutable testOwner = testOwner
        while not (isNull testOwner) && not (obj.ReferenceEquals(testOwner, outRec)) do
            testOwner <- testOwner.owner
        isNull testOwner

    static member private UncoupleOutRec(ae: Active) : unit =
        let outrec = ae.outrec
        if isNull outrec then ()
        else
            outrec.frontEdge.outrec <- null
            outrec.backEdge.outrec <- null
            outrec.frontEdge <- null
            outrec.backEdge <- null

    static member private OutrecIsAscending(hotEdge: Active) : bool =
        obj.ReferenceEquals(hotEdge, hotEdge.outrec.frontEdge)

    static member private SwapFrontBackSides(outrec: OutRec) : unit =
        let ae2 = outrec.frontEdge
        outrec.frontEdge <- outrec.backEdge
        outrec.backEdge <- ae2
        outrec.pts <- outrec.pts.next

    static member private EdgesAdjacentInAEL(inode: IntersectNode) : bool =
        obj.ReferenceEquals(inode.edge1.nextInAEL, inode.edge2) || obj.ReferenceEquals(inode.edge1.prevInAEL, inode.edge2)

    member private this.IsContributingClosed(ae: Active) : bool =
        let mutable passesFillRule = true
        match _fillrule with
        | FillRule.Positive ->
            if ae.windCount <> 1 then passesFillRule <- false
        | FillRule.Negative ->
            if ae.windCount <> -1 then passesFillRule <- false
        | FillRule.NonZero ->
            if Math.Abs(ae.windCount) <> 1 then passesFillRule <- false
        | _ -> ()

        if not passesFillRule then false
        else
            match _cliptype with
            | ClipType.Intersection ->
                match _fillrule with
                | FillRule.Positive -> ae.windCount2 > 0
                | FillRule.Negative -> ae.windCount2 < 0
                | _ -> ae.windCount2 <> 0
            | ClipType.Union ->
                match _fillrule with
                | FillRule.Positive -> ae.windCount2 <= 0
                | FillRule.Negative -> ae.windCount2 >= 0
                | _ -> ae.windCount2 = 0
            | ClipType.Difference ->
                let result =
                    match _fillrule with
                    | FillRule.Positive -> ae.windCount2 <= 0
                    | FillRule.Negative -> ae.windCount2 >= 0
                    | _ -> ae.windCount2 = 0
                if ClipperBase.GetPolyType(ae) = PathType.Subject then result else not result
            | ClipType.Xor -> true
            | _ -> false

    member private this.IsContributingOpen(ae: Active) : bool =
        let mutable isInSubj = false
        let mutable isInClip = false
        match _fillrule with
        | FillRule.Positive ->
            isInSubj <- ae.windCount > 0
            isInClip <- ae.windCount2 > 0
        | FillRule.Negative ->
            isInSubj <- ae.windCount < 0
            isInClip <- ae.windCount2 < 0
        | _ ->
            isInSubj <- ae.windCount <> 0
            isInClip <- ae.windCount2 <> 0

        match _cliptype with
        | ClipType.Intersection -> isInClip
        | ClipType.Union -> not isInSubj && not isInClip
        | _ -> not isInClip

    member private this.SetWindCountForClosedPathEdge(ae: Active) : unit =
        let mutable ae2 = ae.prevInAEL
        let pt = ClipperBase.GetPolyType(ae)
        while not (isNull ae2) && (ClipperBase.GetPolyType(ae2) <> pt || ClipperBase.IsOpen(ae2)) do
            ae2 <- ae2.prevInAEL

        if isNull ae2 then
            ae.windCount <- ae.windDx
            ae2 <- _actives
        elif _fillrule = FillRule.EvenOdd then
            ae.windCount <- ae.windDx
            ae.windCount2 <- ae2.windCount2
            ae2 <- ae2.nextInAEL
        else
            if ae2.windCount * ae2.windDx < 0 then
                if Math.Abs(ae2.windCount) > 1 then
                    if ae2.windDx * ae.windDx < 0 then
                        ae.windCount <- ae2.windCount
                    else
                        ae.windCount <- ae2.windCount + ae.windDx
                else
                    ae.windCount <- if ClipperBase.IsOpen(ae) then 1 else ae.windDx
            else
                if ae2.windDx * ae.windDx < 0 then
                    ae.windCount <- ae2.windCount
                else
                    ae.windCount <- ae2.windCount + ae.windDx

            ae.windCount2 <- ae2.windCount2
            ae2 <- ae2.nextInAEL

        if _fillrule = FillRule.EvenOdd then
            while not (obj.ReferenceEquals(ae2, ae)) do
                if ClipperBase.GetPolyType(ae2) <> pt && not (ClipperBase.IsOpen(ae2)) then
                    ae.windCount2 <- if ae.windCount2 = 0 then 1 else 0
                ae2 <- ae2.nextInAEL
        else
            while not (obj.ReferenceEquals(ae2, ae)) do
                if ClipperBase.GetPolyType(ae2) <> pt && not (ClipperBase.IsOpen(ae2)) then
                    ae.windCount2 <- ae.windCount2 + ae2.windDx
                ae2 <- ae2.nextInAEL

    member private this.SetWindCountForOpenPathEdge(ae: Active) : unit =
        let mutable ae2 = _actives
        if _fillrule = FillRule.EvenOdd then
            let mutable cnt1 = 0
            let mutable cnt2 = 0
            while not (obj.ReferenceEquals(ae2, ae)) do
                if ClipperBase.GetPolyType(ae2) = PathType.Clip then
                    cnt2 <- cnt2 + 1
                elif not (ClipperBase.IsOpen(ae2)) then
                    cnt1 <- cnt1 + 1
                ae2 <- ae2.nextInAEL

            ae.windCount <- if ClipperBase.IsOdd(cnt1) then 1 else 0
            ae.windCount2 <- if ClipperBase.IsOdd(cnt2) then 1 else 0
        else
            while not (obj.ReferenceEquals(ae2, ae)) do
                if ClipperBase.GetPolyType(ae2) = PathType.Clip then
                    ae.windCount2 <- ae.windCount2 + ae2.windDx
                elif not (ClipperBase.IsOpen(ae2)) then
                    ae.windCount <- ae.windCount + ae2.windDx
                ae2 <- ae2.nextInAEL

    static member private IsValidAelOrder(resident: Active, newcomer: Active) : bool =
        if newcomer.curX <> resident.curX then
            newcomer.curX > resident.curX
        else
            let d = InternalClipper.CrossProductSign(resident.top, newcomer.bot, newcomer.top)
            if d <> 0 then
                d < 0
            else
                if not (ClipperBase.IsMaxima(resident)) && resident.top.Y > newcomer.top.Y then
                    InternalClipper.CrossProductSign(newcomer.bot, resident.top, ClipperBase.NextVertex(resident).pt) <= 0
                elif not (ClipperBase.IsMaxima(newcomer)) && newcomer.top.Y > resident.top.Y then
                    InternalClipper.CrossProductSign(newcomer.bot, newcomer.top, ClipperBase.NextVertex(newcomer).pt) >= 0
                else
                    let y = newcomer.bot.Y
                    let newcomerIsLeft = newcomer.isLeftBound
                    if resident.bot.Y <> y || resident.localMin.vertex.pt.Y <> y then
                        newcomer.isLeftBound
                    elif resident.isLeftBound <> newcomerIsLeft then
                        newcomerIsLeft
                    elif InternalClipper.IsCollinear(ClipperBase.PrevPrevVertex(resident).pt, resident.bot, resident.top) then
                        true
                    else
                        (InternalClipper.CrossProductSign(ClipperBase.PrevPrevVertex(resident).pt, newcomer.bot, ClipperBase.PrevPrevVertex(newcomer).pt) > 0) = newcomerIsLeft

    member private this.InsertLeftEdge(ae: Active) : unit =
        if isNull _actives then
            ae.prevInAEL <- null
            ae.nextInAEL <- null
            _actives <- ae
        elif not (ClipperBase.IsValidAelOrder(_actives, ae)) then
            ae.prevInAEL <- null
            ae.nextInAEL <- _actives
            _actives.prevInAEL <- ae
            _actives <- ae
        else
            let mutable ae2 = _actives
            while not (isNull ae2.nextInAEL) && ClipperBase.IsValidAelOrder(ae2.nextInAEL, ae) do
                ae2 <- ae2.nextInAEL
            if ae2.joinWith = JoinWith.Right then
                ae2 <- ae2.nextInAEL
            ae.nextInAEL <- ae2.nextInAEL
            if not (isNull ae2.nextInAEL) then
                ae2.nextInAEL.prevInAEL <- ae
            ae.prevInAEL <- ae2
            ae2.nextInAEL <- ae

    static member private InsertRightEdge(ae: Active, ae2: Active) : unit =
        ae2.nextInAEL <- ae.nextInAEL
        if not (isNull ae.nextInAEL) then
            ae.nextInAEL.prevInAEL <- ae2
        ae2.prevInAEL <- ae
        ae.nextInAEL <- ae2

    member private this.InsertLocalMinimaIntoAEL(botY: int64) : unit =
        while this.HasLocMinAtY(botY) do
            let localMinima = this.PopLocalMinima()
            let mutable leftBound: Active = null
            if (localMinima.vertex.flags &&& VertexFlags.OpenStart) = VertexFlags.None then
                leftBound <- this.NewActive()
                leftBound.bot <- localMinima.vertex.pt
                leftBound.curX <- localMinima.vertex.pt.X
                leftBound.windDx <- -1
                leftBound.vertexTop <- localMinima.vertex.prev
                leftBound.top <- localMinima.vertex.prev.pt
                leftBound.outrec <- null
                leftBound.localMin <- localMinima
                ClipperBase.SetDx(leftBound)

            let mutable rightBound: Active = null
            if (localMinima.vertex.flags &&& VertexFlags.OpenEnd) = VertexFlags.None then
                rightBound <- this.NewActive()
                rightBound.bot <- localMinima.vertex.pt
                rightBound.curX <- localMinima.vertex.pt.X
                rightBound.windDx <- 1
                rightBound.vertexTop <- localMinima.vertex.next
                rightBound.top <- localMinima.vertex.next.pt
                rightBound.outrec <- null
                rightBound.localMin <- localMinima
                ClipperBase.SetDx(rightBound)

            if not (isNull leftBound) && not (isNull rightBound) then
                if ClipperBase.IsHorizontal(leftBound) then
                    if ClipperBase.IsHeadingRightHorz(leftBound) then
                        ClipperBase.SwapActives(&leftBound, &rightBound)
                elif ClipperBase.IsHorizontal(rightBound) then
                    if ClipperBase.IsHeadingLeftHorz(rightBound) then
                        ClipperBase.SwapActives(&leftBound, &rightBound)
                elif leftBound.dx < rightBound.dx then
                    ClipperBase.SwapActives(&leftBound, &rightBound)
            elif isNull leftBound then
                leftBound <- rightBound
                rightBound <- null

            let mutable contributing = false
            leftBound.isLeftBound <- true
            this.InsertLeftEdge(leftBound)

            if ClipperBase.IsOpen(leftBound) then
                this.SetWindCountForOpenPathEdge(leftBound)
                contributing <- this.IsContributingOpen(leftBound)
            else
                this.SetWindCountForClosedPathEdge(leftBound)
                contributing <- this.IsContributingClosed(leftBound)

            if not (isNull rightBound) then
                rightBound.windCount <- leftBound.windCount
                rightBound.windCount2 <- leftBound.windCount2
                ClipperBase.InsertRightEdge(leftBound, rightBound)

                if contributing then
                    this.AddLocalMinPoly(leftBound, rightBound, leftBound.bot, true) |> ignore
                    if not (ClipperBase.IsHorizontal(leftBound)) then
                        this.CheckJoinLeft(leftBound, leftBound.bot)

                while not (isNull rightBound.nextInAEL) &&
                      ClipperBase.IsValidAelOrder(rightBound.nextInAEL, rightBound) do
                    this.IntersectEdges(rightBound, rightBound.nextInAEL, rightBound.bot)
                    this.SwapPositionsInAEL(rightBound, rightBound.nextInAEL)

                if ClipperBase.IsHorizontal(rightBound) then
                    this.PushHorz(rightBound)
                else
                    this.CheckJoinRight(rightBound, rightBound.bot)
                    this.InsertScanline(rightBound.top.Y)
            elif contributing then
                this.StartOpenPath(leftBound, leftBound.bot) |> ignore

            if ClipperBase.IsHorizontal(leftBound) then
                this.PushHorz(leftBound)
            else
                this.InsertScanline(leftBound.top.Y)

    member private this.PushHorz(ae: Active) : unit =
        ae.nextInSEL <- _sel
        _sel <- ae

    member private this.PopHorz(ae: byref<Active>) : bool =
        ae <- _sel
        if isNull _sel then false
        else
            _sel <- _sel.nextInSEL
            true

    member private this.AddLocalMinPoly(ae1: Active, ae2: Active, pt: Point64, ?isNew: bool) : OutPt =
        let isNew = defaultArg isNew false
        let outrec = this.NewOutRec()
        ae1.outrec <- outrec
        ae2.outrec <- outrec

        if ClipperBase.IsOpen(ae1) then
            outrec.owner <- null
            outrec.isOpen <- true
            if ae1.windDx > 0 then
                ClipperBase.SetSides(outrec, ae1, ae2)
            else
                ClipperBase.SetSides(outrec, ae2, ae1)
        else
            outrec.isOpen <- false
            let prevHotEdge = ClipperBase.GetPrevHotEdge(ae1)
            if not (isNull prevHotEdge) then
                if this._using_polytree then
                    ClipperBase.SetOwner(outrec, prevHotEdge.outrec)
                outrec.owner <- prevHotEdge.outrec
                if (ClipperBase.OutrecIsAscending(prevHotEdge) = isNew) then
                    ClipperBase.SetSides(outrec, ae2, ae1)
                else
                    ClipperBase.SetSides(outrec, ae1, ae2)
            else
                outrec.owner <- null
                if isNew then
                    ClipperBase.SetSides(outrec, ae1, ae2)
                else
                    ClipperBase.SetSides(outrec, ae2, ae1)

        let op = _outPtPool.Add(pt, outrec)
        outrec.pts <- op
        op

    member private this.AddLocalMaxPoly(ae1: Active, ae2: Active, pt: Point64) : OutPt =
        if this.IsJoined(ae1) then this.Split(ae1, pt)
        if this.IsJoined(ae2) then this.Split(ae2, pt)

        if ClipperBase.IsFront(ae1) = ClipperBase.IsFront(ae2) then
            if ClipperBase.IsOpenEnd(ae1) then
                ClipperBase.SwapFrontBackSides(ae1.outrec)
                null
            elif ClipperBase.IsOpenEnd(ae2) then
                ClipperBase.SwapFrontBackSides(ae2.outrec)
                null
            else
                this._succeeded <- false
                null
        else
            let result = this.AddOutPt(ae1, pt)
            if obj.ReferenceEquals(ae1.outrec, ae2.outrec) then
                let outrec = ae1.outrec
                outrec.pts <- result

                if this._using_polytree then
                    let e = ClipperBase.GetPrevHotEdge(ae1)
                    if isNull e then
                        outrec.owner <- null
                    else
                        ClipperBase.SetOwner(outrec, e.outrec)
                ClipperBase.UncoupleOutRec(ae1)
            elif ClipperBase.IsOpen(ae1) then
                if ae1.windDx < 0 then
                    ClipperBase.JoinOutrecPaths(ae1, ae2)
                else
                    ClipperBase.JoinOutrecPaths(ae2, ae1)
            elif ae1.outrec.idx < ae2.outrec.idx then
                ClipperBase.JoinOutrecPaths(ae1, ae2)
            else
                ClipperBase.JoinOutrecPaths(ae2, ae1)
            result

    static member private JoinOutrecPaths(ae1: Active, ae2: Active) : unit =
        let p1Start = ae1.outrec.pts
        let p2Start = ae2.outrec.pts
        let p1End = p1Start.next
        let p2End = p2Start.next
        if ClipperBase.IsFront(ae1) then
            p2End.prev <- p1Start
            p1Start.next <- p2End
            p2Start.next <- p1End
            p1End.prev <- p2Start
            ae1.outrec.pts <- p2Start
            ae1.outrec.frontEdge <- ae2.outrec.frontEdge
            if not (isNull ae1.outrec.frontEdge) then
                ae1.outrec.frontEdge.outrec <- ae1.outrec
        else
            p1End.prev <- p2Start
            p2Start.next <- p1End
            p1Start.next <- p2End
            p2End.prev <- p1Start

            ae1.outrec.backEdge <- ae2.outrec.backEdge
            if not (isNull ae1.outrec.backEdge) then
                ae1.outrec.backEdge.outrec <- ae1.outrec

        ae2.outrec.frontEdge <- null
        ae2.outrec.backEdge <- null
        ae2.outrec.pts <- null
        ae1.outrec.outPtCount <- ae1.outrec.outPtCount + ae2.outrec.outPtCount
        ClipperBase.SetOwner(ae2.outrec, ae1.outrec)

        if ClipperBase.IsOpenEnd(ae1) then
            ae2.outrec.pts <- ae1.outrec.pts
            ae1.outrec.pts <- null

        ae1.outrec <- null
        ae2.outrec <- null

    member private this.AddOutPt(ae: Active, pt: Point64) : OutPt =
        let outrec = ae.outrec
        let toFront = ClipperBase.IsFront(ae)
        let opFront = outrec.pts
        let opBack = opFront.next

        if toFront && pt = opFront.pt then
            opFront
        elif not toFront && pt = opBack.pt then
            opBack
        else
            let newOp = _outPtPool.Add(pt, outrec)
            opBack.prev <- newOp
            newOp.prev <- opFront
            newOp.next <- opBack
            opFront.next <- newOp
            if toFront then outrec.pts <- newOp
            newOp

    member private this.NewOutRec() : OutRec =
        let idx = _outrecList.Count
        let result = _outrecList.Add()
        result.idx <- idx
        result

    member private this.StartOpenPath(ae: Active, pt: Point64) : OutPt =
        let outrec = this.NewOutRec()
        outrec.isOpen <- true
        if ae.windDx > 0 then
            outrec.frontEdge <- ae
            outrec.backEdge <- null
        else
            outrec.frontEdge <- null
            outrec.backEdge <- ae

        ae.outrec <- outrec
        let op = _outPtPool.Add(pt, outrec)
        outrec.pts <- op
        op

    member private this.UpdateEdgeIntoAEL(ae: Active) : unit =
        ae.bot <- ae.top
        ae.vertexTop <- ClipperBase.NextVertex(ae)
        ae.top <- ae.vertexTop.pt
        ae.curX <- ae.bot.X
        ClipperBase.SetDx(ae)

        if this.IsJoined(ae) then
            this.Split(ae, ae.bot)

        if ClipperBase.IsHorizontal(ae) then
            if not (ClipperBase.IsOpen(ae)) then
                this.TrimHorz(ae, this.PreserveCollinear)
        else
            this.InsertScanline(ae.top.Y)
            this.CheckJoinLeft(ae, ae.bot)
            this.CheckJoinRight(ae, ae.bot, true)

    static member private FindEdgeWithMatchingLocMin(e: Active) : Active =
        let mutable result = e.nextInAEL
        let mutable done_ = false
        while not (isNull result) && not done_ do
            if result.localMin = e.localMin then
                done_ <- true
            elif not (ClipperBase.IsHorizontal(result)) && e.bot <> result.bot then
                result <- null
                done_ <- true
            else
                result <- result.nextInAEL

        if not (isNull result) then result
        else
            result <- e.prevInAEL
            let mutable done2 = false
            while not (isNull result) && not done2 do
                if result.localMin = e.localMin then
                    done2 <- true
                elif not (ClipperBase.IsHorizontal(result)) && e.bot <> result.bot then
                    result <- null
                    done2 <- true
                else
                    result <- result.prevInAEL
            result

    member private this.NewActive() : Active =
        if _freeActives.Count = 0 then
            Active()
        else
            _freeActives.Pop()

    member private this.IsJoined(e: Active) : bool =
        e.joinWith <> JoinWith.None

    member private this.Split(e: Active, currPt: Point64) : unit =
        if e.joinWith = JoinWith.Right then
            e.joinWith <- JoinWith.None
            e.nextInAEL.joinWith <- JoinWith.None
            this.AddLocalMinPoly(e, e.nextInAEL, currPt, true) |> ignore
        else
            e.joinWith <- JoinWith.None
            e.prevInAEL.joinWith <- JoinWith.None
            this.AddLocalMinPoly(e.prevInAEL, e, currPt, true) |> ignore

    member private this.CheckJoinLeft(e: Active, pt: Point64, ?checkCurrX: bool) : unit =
        let checkCurrX = defaultArg checkCurrX false
        let prev = e.prevInAEL
        if isNull prev ||
           not (ClipperBase.IsHotEdge(e)) || not (ClipperBase.IsHotEdge(prev)) ||
           ClipperBase.IsHorizontal(e) || ClipperBase.IsHorizontal(prev) ||
           ClipperBase.IsOpen(e) || ClipperBase.IsOpen(prev) then ()
        elif (pt.Y < e.top.Y + 2L || pt.Y < prev.top.Y + 2L) &&
             ((e.bot.Y > pt.Y) || (prev.bot.Y > pt.Y)) then ()
        elif checkCurrX && ClipperPrimitives.PerpendicDistFromLineSqrd(pt, prev.bot, prev.top) > 0.25 then ()
        elif not checkCurrX && e.curX <> prev.curX then ()
        elif not (InternalClipper.IsCollinear(e.top, pt, prev.top)) then ()
        else
            if e.outrec.idx = prev.outrec.idx then
                this.AddLocalMaxPoly(prev, e, pt) |> ignore
            elif e.outrec.idx < prev.outrec.idx then
                ClipperBase.JoinOutrecPaths(e, prev)
            else
                ClipperBase.JoinOutrecPaths(prev, e)
            prev.joinWith <- JoinWith.Right
            e.joinWith <- JoinWith.Left

    member private this.CheckJoinRight(e: Active, pt: Point64, ?checkCurrX: bool) : unit =
        let checkCurrX = defaultArg checkCurrX false
        let next = e.nextInAEL
        if isNull next ||
           not (ClipperBase.IsHotEdge(e)) || not (ClipperBase.IsHotEdge(next)) ||
           ClipperBase.IsHorizontal(e) || ClipperBase.IsHorizontal(next) ||
           ClipperBase.IsOpen(e) || ClipperBase.IsOpen(next) then ()
        elif (pt.Y < e.top.Y + 2L || pt.Y < next.top.Y + 2L) &&
             ((e.bot.Y > pt.Y) || (next.bot.Y > pt.Y)) then ()
        elif checkCurrX && ClipperPrimitives.PerpendicDistFromLineSqrd(pt, next.bot, next.top) > 0.25 then ()
        elif not checkCurrX && e.curX <> next.curX then ()
        elif not (InternalClipper.IsCollinear(e.top, pt, next.top)) then ()
        else
            if e.outrec.idx = next.outrec.idx then
                this.AddLocalMaxPoly(e, next, pt) |> ignore
            elif e.outrec.idx < next.outrec.idx then
                ClipperBase.JoinOutrecPaths(e, next)
            else
                ClipperBase.JoinOutrecPaths(next, e)
            e.joinWith <- JoinWith.Right
            next.joinWith <- JoinWith.Left

    static member private FixOutRecPts(outrec: OutRec) : unit =
        let mutable op = outrec.pts
        let mutable done_ = false
        while not done_ do
            op.outrec <- outrec
            op <- op.next
            done_ <- obj.ReferenceEquals(op, outrec.pts)

    static member private SetHorzSegHeadingForward(hs: HorzSegment, opP: OutPt, opN: OutPt) : bool =
        if opP.pt.X = opN.pt.X then false
        elif opP.pt.X < opN.pt.X then
            hs.leftOp <- opP
            hs.rightOp <- opN
            hs.leftToRight <- true
            true
        else
            hs.leftOp <- opN
            hs.rightOp <- opP
            hs.leftToRight <- false
            true

    static member private UpdateHorzSegment(hs: HorzSegment) : bool =
        let op = hs.leftOp
        let outrec = ClipperBase.GetRealOutRec(op.outrec)
        let outrecHasEdges = not (isNull outrec.frontEdge)
        let curr_y = op.pt.Y
        let mutable opP = op
        let mutable opN = op
        if outrecHasEdges then
            let opA = outrec.pts
            let opZ = opA.next
            while not (obj.ReferenceEquals(opP, opZ)) && opP.prev.pt.Y = curr_y do
                opP <- opP.prev
            while not (obj.ReferenceEquals(opN, opA)) && opN.next.pt.Y = curr_y do
                opN <- opN.next
        else
            while not (obj.ReferenceEquals(opP.prev, opN)) && opP.prev.pt.Y = curr_y do
                opP <- opP.prev
            while not (obj.ReferenceEquals(opN.next, opP)) && opN.next.pt.Y = curr_y do
                opN <- opN.next

        let result =
            ClipperBase.SetHorzSegHeadingForward(hs, opP, opN) &&
            isNull hs.leftOp.horz

        if result then
            hs.leftOp.horz <- hs
        else
            hs.rightOp <- null
        result

    member private this.DuplicateOp(op: OutPt, insert_after: bool) : OutPt =
        let result = _outPtPool.Add(op.pt, op.outrec)
        if insert_after then
            result.next <- op.next
            result.next.prev <- result
            result.prev <- op
            op.next <- result
        else
            result.prev <- op.prev
            result.prev.next <- result
            result.next <- op
            op.prev <- result
        result

    static member private HorzSegSort(hs1: HorzSegment, hs2: HorzSegment) : int =
        if isNull hs1 || isNull hs2 then 0
        elif isNull hs1.rightOp then
            if isNull hs2.rightOp then 0 else 1
        elif isNull hs2.rightOp then
            -1
        else
            hs1.leftOp.pt.X.CompareTo(hs2.leftOp.pt.X)

    member private this.ConvertHorzSegsToJoins() : unit =
        let mutable k = 0
        for hs in _horzSegList do
            if ClipperBase.UpdateHorzSegment(hs) then
                k <- k + 1
        if k < 2 then ()
        else
            _horzSegList.Sort(fun hs1 hs2 -> ClipperBase.HorzSegSort(hs1, hs2))
            for i = 0 to k - 2 do
                let hs1 = _horzSegList.[i]
                for j = i + 1 to k - 1 do
                    let hs2 = _horzSegList.[j]
                    if hs2.leftOp.pt.X >= hs1.rightOp.pt.X ||
                       hs2.leftToRight = hs1.leftToRight ||
                       hs2.rightOp.pt.X <= hs1.leftOp.pt.X then ()
                    else
                        let curr_y = hs1.leftOp.pt.Y
                        if hs1.leftToRight then
                            while hs1.leftOp.next.pt.Y = curr_y &&
                                  hs1.leftOp.next.pt.X <= hs2.leftOp.pt.X do
                                hs1.leftOp <- hs1.leftOp.next
                            while hs2.leftOp.prev.pt.Y = curr_y &&
                                  hs2.leftOp.prev.pt.X <= hs1.leftOp.pt.X do
                                hs2.leftOp <- hs2.leftOp.prev
                            _horzJoinList.Add(
                                this.DuplicateOp(hs1.leftOp, true),
                                this.DuplicateOp(hs2.leftOp, false))
                            |> ignore
                        else
                            while hs1.leftOp.prev.pt.Y = curr_y &&
                                  hs1.leftOp.prev.pt.X <= hs2.leftOp.pt.X do
                                hs1.leftOp <- hs1.leftOp.prev
                            while hs2.leftOp.next.pt.Y = curr_y &&
                                  hs2.leftOp.next.pt.X <= hs1.leftOp.pt.X do
                                hs2.leftOp <- hs2.leftOp.next
                            _horzJoinList.Add(
                                this.DuplicateOp(hs2.leftOp, true),
                                this.DuplicateOp(hs1.leftOp, false))
                            |> ignore

    static member private GetCleanPath(op: OutPt) : Path64 =
        let result = Path64()
        let mutable op2 = op
        while not (obj.ReferenceEquals(op2.next, op)) &&
              ((op2.pt.X = op2.next.pt.X && op2.pt.X = op2.prev.pt.X) ||
               (op2.pt.Y = op2.next.pt.Y && op2.pt.Y = op2.prev.pt.Y)) do
            op2 <- op2.next
        result.Add(op2.pt)
        let mutable prevOp = op2
        op2 <- op2.next
        while not (obj.ReferenceEquals(op2, op)) do
            if (op2.pt.X <> op2.next.pt.X || op2.pt.X <> prevOp.pt.X) &&
               (op2.pt.Y <> op2.next.pt.Y || op2.pt.Y <> prevOp.pt.Y) then
                result.Add(op2.pt)
                prevOp <- op2
            op2 <- op2.next
        result

    static member private PointInOpPolygon(pt: Point64, opArg: OutPt) : PointInPolygonResult =
        if obj.ReferenceEquals(opArg, opArg.next) || obj.ReferenceEquals(opArg.prev, opArg.next) then
            PointInPolygonResult.IsOutside
        else
            let mutable op = opArg
            let op2Start = op
            let mutable done_ = false
            while not done_ do
                if op.pt.Y <> pt.Y then
                    done_ <- true
                else
                    op <- op.next
                    done_ <- obj.ReferenceEquals(op, op2Start)

            if op.pt.Y = pt.Y then
                PointInPolygonResult.IsOutside
            else
                let mutable isAbove = op.pt.Y < pt.Y
                let startingAbove = isAbove
                let mutable val_ = 0
                let mutable op2 = op.next
                let mutable finished = false
                let mutable result = PointInPolygonResult.IsOutside

                while not finished && not (obj.ReferenceEquals(op2, op)) do
                    if isAbove then
                        while not (obj.ReferenceEquals(op2, op)) && op2.pt.Y < pt.Y do
                            op2 <- op2.next
                    else
                        while not (obj.ReferenceEquals(op2, op)) && op2.pt.Y > pt.Y do
                            op2 <- op2.next
                    if obj.ReferenceEquals(op2, op) then
                        finished <- true
                    else
                        if op2.pt.Y = pt.Y then
                            if op2.pt.X = pt.X ||
                               (op2.pt.Y = op2.prev.pt.Y &&
                                ((pt.X < op2.prev.pt.X) <> (pt.X < op2.pt.X))) then
                                result <- PointInPolygonResult.IsOn
                                finished <- true
                            else
                                op2 <- op2.next
                                if obj.ReferenceEquals(op2, op) then
                                    finished <- true

                        if not finished then
                            if op2.pt.X <= pt.X || op2.prev.pt.X <= pt.X then
                                if op2.prev.pt.X < pt.X && op2.pt.X < pt.X then
                                    val_ <- 1 - val_
                                else
                                    let d = InternalClipper.CrossProductSign(op2.prev.pt, op2.pt, pt)
                                    if d = 0 then
                                        result <- PointInPolygonResult.IsOn
                                        finished <- true
                                    elif (d < 0) = isAbove then
                                        val_ <- 1 - val_
                            if not finished then
                                isAbove <- not isAbove
                                op2 <- op2.next

                if result = PointInPolygonResult.IsOn then
                    result
                elif isAbove = startingAbove then
                    if val_ = 0 then PointInPolygonResult.IsOutside else PointInPolygonResult.IsInside
                else
                    let d = InternalClipper.CrossProductSign(op2.prev.pt, op2.pt, pt)
                    if d = 0 then PointInPolygonResult.IsOn
                    else
                        if (d < 0) = isAbove then
                            val_ <- 1 - val_
                        if val_ = 0 then PointInPolygonResult.IsOutside else PointInPolygonResult.IsInside

    static member private Path1InsidePath2(op1: OutPt, op2: OutPt) : bool =
        let mutable pip = PointInPolygonResult.IsOn
        let mutable op = op1
        let mutable done_ = false
        let mutable result = false
        while not done_ && not result do
            match ClipperBase.PointInOpPolygon(op.pt, op2) with
            | PointInPolygonResult.IsOutside ->
                if pip = PointInPolygonResult.IsOutside then
                    done_ <- true
                else
                    pip <- PointInPolygonResult.IsOutside
                    op <- op.next
                    done_ <- obj.ReferenceEquals(op, op1)
            | PointInPolygonResult.IsInside ->
                if pip = PointInPolygonResult.IsInside then
                    result <- true
                else
                    pip <- PointInPolygonResult.IsInside
                    op <- op.next
                    done_ <- obj.ReferenceEquals(op, op1)
            | _ ->
                op <- op.next
                done_ <- obj.ReferenceEquals(op, op1)

        if result then true
        elif done_ then
            InternalClipper.Path2ContainsPath1(ClipperBase.GetCleanPath(op1), ClipperBase.GetCleanPath(op2))
        else
            false

    static member private MoveSplits(fromOr: OutRec, toOr: OutRec) : unit =
        if isNull fromOr.splits then ()
        else
            if isNull toOr.splits then
                toOr.splits <- List<int>()
            for i in fromOr.splits do
                if i <> toOr.idx then
                    toOr.splits.Add(i)
            fromOr.splits <- null

    member private this.ProcessHorzJoins() : unit =
        for j in _horzJoinList do
            let or1 = ClipperBase.GetRealOutRec(j.op1.outrec)
            let mutable or2 = ClipperBase.GetRealOutRec(j.op2.outrec)

            let op1b = j.op1.next
            let op2b = j.op2.prev
            j.op1.next <- j.op2
            j.op2.prev <- j.op1
            op1b.prev <- op2b
            op2b.next <- op1b

            if obj.ReferenceEquals(or1, or2) then
                or2 <- this.NewOutRec()
                or2.pts <- op1b
                ClipperBase.FixOutRecPts(or2)

                if obj.ReferenceEquals(or1.pts.outrec, or2) then
                    or1.pts <- j.op1
                    or1.pts.outrec <- or1

                if this._using_polytree then
                    if ClipperBase.Path1InsidePath2(or1.pts, or2.pts) then
                        let tmp = or2.pts
                        or2.pts <- or1.pts
                        or1.pts <- tmp
                        ClipperBase.FixOutRecPts(or1)
                        ClipperBase.FixOutRecPts(or2)
                        or2.owner <- or1
                    elif ClipperBase.Path1InsidePath2(or2.pts, or1.pts) then
                        or2.owner <- or1
                    else
                        or2.owner <- or1.owner

                    if isNull or1.splits then
                        or1.splits <- List<int>()
                    or1.splits.Add(or2.idx)
                else
                    or2.owner <- or1
            else
                or2.pts <- null
                if this._using_polytree then
                    ClipperBase.SetOwner(or2, or1)
                    ClipperBase.MoveSplits(or2, or1)
                else
                    or2.owner <- or1

    member private this.IntersectEdges(ae1Arg: Active, ae2Arg: Active, pt: Point64) : unit =
        let mutable ae1 = ae1Arg
        let mutable ae2 = ae2Arg
        let mutable resultOp: OutPt = null
#if USINGZ
        let setZOnOutPt (op: OutPt) : unit =
            let mutable zPt = op.pt
            this.SetZ(ae1, ae2, &zPt)
            op.pt <- zPt
#endif

        if _hasOpenPaths && (ClipperBase.IsOpen(ae1) || ClipperBase.IsOpen(ae2)) then
            if ClipperBase.IsOpen(ae1) && ClipperBase.IsOpen(ae2) then ()
            else
                if ClipperBase.IsOpen(ae2) then
                    ClipperBase.SwapActives(&ae1, &ae2)
                if this.IsJoined(ae2) then
                    this.Split(ae2, pt)

                let mutable shouldExit = false
                if _cliptype = ClipType.Union then
                    if not (ClipperBase.IsHotEdge(ae2)) then
                        shouldExit <- true
                elif ae2.localMin.polytype = PathType.Subject then
                    shouldExit <- true

                if not shouldExit then
                    match _fillrule with
                    | FillRule.Positive ->
                        if ae2.windCount <> 1 then
                            shouldExit <- true
                    | FillRule.Negative ->
                        if ae2.windCount <> -1 then
                            shouldExit <- true
                    | _ ->
                        if Math.Abs(ae2.windCount) <> 1 then
                            shouldExit <- true

                if not shouldExit then
                    if ClipperBase.IsHotEdge(ae1) then
                        resultOp <- this.AddOutPt(ae1, pt)
#if USINGZ
                        setZOnOutPt resultOp
#endif
                        if ClipperBase.IsFront(ae1) then
                            ae1.outrec.frontEdge <- null
                        else
                            ae1.outrec.backEdge <- null
                        ae1.outrec <- null
                    elif pt = ae1.localMin.vertex.pt &&
                         not (ClipperBase.IsOpenEnd(ae1.localMin.vertex)) then
                        let ae3 = ClipperBase.FindEdgeWithMatchingLocMin(ae1)
                        if not (isNull ae3) && ClipperBase.IsHotEdge(ae3) then
                            ae1.outrec <- ae3.outrec
                            if ae1.windDx > 0 then
                                ClipperBase.SetSides(ae3.outrec, ae1, ae3)
                            else
                                ClipperBase.SetSides(ae3.outrec, ae3, ae1)
                        else
                            resultOp <- this.StartOpenPath(ae1, pt)
#if USINGZ
                            setZOnOutPt resultOp
#endif
                    else
                        resultOp <- this.StartOpenPath(ae1, pt)
#if USINGZ
                        setZOnOutPt resultOp
#endif
        else
            if this.IsJoined(ae1) then
                this.Split(ae1, pt)
            if this.IsJoined(ae2) then
                this.Split(ae2, pt)

            let mutable oldE1WindCount = 0
            let mutable oldE2WindCount = 0
            if ae1.localMin.polytype = ae2.localMin.polytype then
                if _fillrule = FillRule.EvenOdd then
                    oldE1WindCount <- ae1.windCount
                    ae1.windCount <- ae2.windCount
                    ae2.windCount <- oldE1WindCount
                else
                    if ae1.windCount + ae2.windDx = 0 then
                        ae1.windCount <- -ae1.windCount
                    else
                        ae1.windCount <- ae1.windCount + ae2.windDx
                    if ae2.windCount - ae1.windDx = 0 then
                        ae2.windCount <- -ae2.windCount
                    else
                        ae2.windCount <- ae2.windCount - ae1.windDx
            else
                if _fillrule <> FillRule.EvenOdd then
                    ae1.windCount2 <- ae1.windCount2 + ae2.windDx
                else
                    ae1.windCount2 <- if ae1.windCount2 = 0 then 1 else 0
                if _fillrule <> FillRule.EvenOdd then
                    ae2.windCount2 <- ae2.windCount2 - ae1.windDx
                else
                    ae2.windCount2 <- if ae2.windCount2 = 0 then 1 else 0

            match _fillrule with
            | FillRule.Positive ->
                oldE1WindCount <- ae1.windCount
                oldE2WindCount <- ae2.windCount
            | FillRule.Negative ->
                oldE1WindCount <- -ae1.windCount
                oldE2WindCount <- -ae2.windCount
            | _ ->
                oldE1WindCount <- Math.Abs(ae1.windCount)
                oldE2WindCount <- Math.Abs(ae2.windCount)

            let e1WindCountIs0or1 = oldE1WindCount = 0 || oldE1WindCount = 1
            let e2WindCountIs0or1 = oldE2WindCount = 0 || oldE2WindCount = 1

            if (not (ClipperBase.IsHotEdge(ae1)) && not e1WindCountIs0or1) ||
               (not (ClipperBase.IsHotEdge(ae2)) && not e2WindCountIs0or1) then ()
            elif ClipperBase.IsHotEdge(ae1) && ClipperBase.IsHotEdge(ae2) then
                if (oldE1WindCount <> 0 && oldE1WindCount <> 1) ||
                   (oldE2WindCount <> 0 && oldE2WindCount <> 1) ||
                   (ae1.localMin.polytype <> ae2.localMin.polytype && _cliptype <> ClipType.Xor) then
                    resultOp <- this.AddLocalMaxPoly(ae1, ae2, pt)
#if USINGZ
                    if not (isNull resultOp) then
                        setZOnOutPt resultOp
#endif
                elif ClipperBase.IsFront(ae1) || obj.ReferenceEquals(ae1.outrec, ae2.outrec) then
                    resultOp <- this.AddLocalMaxPoly(ae1, ae2, pt)
#if USINGZ
                    let op2 = this.AddLocalMinPoly(ae1, ae2, pt)
                    if not (isNull resultOp) then
                        setZOnOutPt resultOp
                    setZOnOutPt op2
#else
                    this.AddLocalMinPoly(ae1, ae2, pt) |> ignore
#endif
                else
                    resultOp <- this.AddOutPt(ae1, pt)
#if USINGZ
                    let op2 = this.AddOutPt(ae2, pt)
                    setZOnOutPt resultOp
                    setZOnOutPt op2
#else
                    this.AddOutPt(ae2, pt) |> ignore
#endif
                    ClipperBase.SwapOutrecs(ae1, ae2)
            elif ClipperBase.IsHotEdge(ae1) then
                resultOp <- this.AddOutPt(ae1, pt)
#if USINGZ
                setZOnOutPt resultOp
#endif
                ClipperBase.SwapOutrecs(ae1, ae2)
            elif ClipperBase.IsHotEdge(ae2) then
                resultOp <- this.AddOutPt(ae2, pt)
#if USINGZ
                setZOnOutPt resultOp
#endif
                ClipperBase.SwapOutrecs(ae1, ae2)
            else
                let mutable e1Wc2 = 0
                let mutable e2Wc2 = 0
                match _fillrule with
                    | FillRule.Positive ->
                        e1Wc2 <- ae1.windCount2
                        e2Wc2 <- ae2.windCount2
                    | FillRule.Negative ->
                        e1Wc2 <- -ae1.windCount2
                        e2Wc2 <- -ae2.windCount2
                    | _ ->
                        e1Wc2 <- Math.Abs(ae1.windCount2)
                        e2Wc2 <- Math.Abs(ae2.windCount2)

                if not (ClipperBase.IsSamePolyType(ae1, ae2)) then
                    resultOp <- this.AddLocalMinPoly(ae1, ae2, pt)
#if USINGZ
                    setZOnOutPt resultOp
#endif
                elif oldE1WindCount = 1 && oldE2WindCount = 1 then
                    resultOp <- null
                    match _cliptype with
                    | ClipType.Union ->
                        if e1Wc2 > 0 && e2Wc2 > 0 then ()
                        else
                            resultOp <- this.AddLocalMinPoly(ae1, ae2, pt)
                    | ClipType.Difference ->
                        if ((ClipperBase.GetPolyType(ae1) = PathType.Clip) && (e1Wc2 > 0) && (e2Wc2 > 0)) ||
                           ((ClipperBase.GetPolyType(ae1) = PathType.Subject) && (e1Wc2 <= 0) && (e2Wc2 <= 0)) then
                            resultOp <- this.AddLocalMinPoly(ae1, ae2, pt)
                    | ClipType.Xor ->
                        resultOp <- this.AddLocalMinPoly(ae1, ae2, pt)
                    | _ ->
                        if e1Wc2 <= 0 || e2Wc2 <= 0 then ()
                        else
                            resultOp <- this.AddLocalMinPoly(ae1, ae2, pt)
#if USINGZ
                    if not (isNull resultOp) then
                        setZOnOutPt resultOp
#endif

    member private this.SwapPositionsInAEL(ae1: Active, ae2: Active) : unit =
        let next = ae2.nextInAEL
        if not (isNull next) then
            next.prevInAEL <- ae1
        let prev = ae1.prevInAEL
        if not (isNull prev) then
            prev.nextInAEL <- ae2
        ae2.prevInAEL <- prev
        ae2.nextInAEL <- ae1
        ae1.prevInAEL <- ae2
        ae1.nextInAEL <- next
        if isNull ae2.prevInAEL then
            _actives <- ae2

    static member private PtsReallyClose(pt1: Point64, pt2: Point64) : bool =
        Math.Abs(pt1.X - pt2.X) < 2L && Math.Abs(pt1.Y - pt2.Y) < 2L

    static member private IsVerySmallTriangle(op: OutPt) : bool =
        obj.ReferenceEquals(op.next.next, op.prev) &&
        (ClipperBase.PtsReallyClose(op.prev.pt, op.next.pt) ||
         ClipperBase.PtsReallyClose(op.pt, op.next.pt) ||
         ClipperBase.PtsReallyClose(op.pt, op.prev.pt))

    static member private IsValidClosedPath(op: OutPt) : bool =
        not (isNull op) &&
        not (obj.ReferenceEquals(op.next, op)) &&
        (not (obj.ReferenceEquals(op.next, op.prev)) || not (ClipperBase.IsVerySmallTriangle(op)))

    static member private DisposeOutPt(op: OutPt) : OutPt =
        let result =
            if obj.ReferenceEquals(op.next, op) then null
            else op.next
        op.prev.next <- op.next
        op.next.prev <- op.prev
        result

    member private this.CleanCollinear(outrecArg: OutRec) : unit =
        let outrec = ClipperBase.GetRealOutRec(outrecArg)
        if isNull outrec || outrec.isOpen then ()
        elif not (ClipperBase.IsValidClosedPath(outrec.pts)) then
            outrec.pts <- null
        else
            let mutable startOp = outrec.pts
            let mutable op2 = startOp
            let mutable done_ = false
            while not done_ do
                if InternalClipper.IsCollinear(op2.prev.pt, op2.pt, op2.next.pt) &&
                   (op2.pt = op2.prev.pt ||
                    op2.pt = op2.next.pt ||
                    not this.PreserveCollinear ||
                    InternalClipper.DotProduct(op2.prev.pt, op2.pt, op2.next.pt) < 0.0) then
                    if obj.ReferenceEquals(op2, outrec.pts) then
                        outrec.pts <- op2.prev
                    op2 <- ClipperBase.DisposeOutPt(op2)
                    if not (ClipperBase.IsValidClosedPath(op2)) then
                        outrec.pts <- null
                        done_ <- true
                    else
                        startOp <- op2
                else
                    op2 <- op2.next
                    done_ <- obj.ReferenceEquals(op2, startOp)

            if not (isNull outrec.pts) then
                this.FixSelfIntersects(outrec)

    member private this.DoSplitOp(outrec: OutRec, splitOp: OutPt) : unit =
        let prevOp = splitOp.prev
        let nextNextOp = splitOp.next.next
        outrec.pts <- prevOp

        let mutable ip = Point64()
        InternalClipper.GetLineIntersectPt(prevOp.pt, splitOp.pt, splitOp.next.pt, nextNextOp.pt, &ip) |> ignore

#if USINGZ
        if not (isNull this._zCallback) then
            this._zCallback.Invoke(prevOp.pt, splitOp.pt, splitOp.next.pt, nextNextOp.pt, &ip)
#endif

        let area1 = ClipperBase.Area(prevOp)
        let absArea1 = Math.Abs(area1)
        if absArea1 < 2.0 then
            outrec.pts <- null
        else
            let area2 = ClipperBase.AreaTriangle(ip, splitOp.pt, splitOp.next.pt)
            let absArea2 = Math.Abs(area2)

            if ip = prevOp.pt || ip = nextNextOp.pt then
                nextNextOp.prev <- prevOp
                prevOp.next <- nextNextOp
            else
                let newOp2 = _outPtPool.Add(ip, outrec)
                newOp2.prev <- prevOp
                newOp2.next <- nextNextOp
                nextNextOp.prev <- newOp2
                prevOp.next <- newOp2

            if absArea2 > 1.0 &&
               (absArea2 > absArea1 || ((area2 > 0.0) = (area1 > 0.0))) then
                let newOutRec = this.NewOutRec()
                newOutRec.owner <- outrec.owner
                splitOp.outrec <- newOutRec
                splitOp.next.outrec <- newOutRec

                let newOp = _outPtPool.Add(ip, newOutRec)
                newOp.prev <- splitOp.next
                newOp.next <- splitOp
                newOutRec.pts <- newOp
                splitOp.prev <- newOp
                splitOp.next.next <- newOp

                if this._using_polytree then
                    if ClipperBase.Path1InsidePath2(prevOp, newOp) then
                        if isNull newOutRec.splits then
                            newOutRec.splits <- List<int>()
                        newOutRec.splits.Add(outrec.idx)
                    else
                        if isNull outrec.splits then
                            outrec.splits <- List<int>()
                        outrec.splits.Add(newOutRec.idx)

    member private this.FixSelfIntersects(outrec: OutRec) : unit =
        let mutable op2 = outrec.pts
        if obj.ReferenceEquals(op2.prev, op2.next.next) then ()
        else
            let mutable done_ = false
            while not done_ do
                if InternalClipper.SegsIntersect(op2.prev.pt, op2.pt, op2.next.pt, op2.next.next.pt) then
                    if obj.ReferenceEquals(op2, outrec.pts) || obj.ReferenceEquals(op2.next, outrec.pts) then
                        outrec.pts <- outrec.pts.prev
                    this.DoSplitOp(outrec, op2)
                    if isNull outrec.pts then
                        done_ <- true
                    else
                        op2 <- outrec.pts
                        if obj.ReferenceEquals(op2.prev, op2.next.next) then
                            done_ <- true
                else
                    op2 <- op2.next
                    done_ <- obj.ReferenceEquals(op2, outrec.pts)

    static member internal BuildPath(op: OutPt, reverse: bool, isOpen: bool, path: Path64) : bool =
        if isNull op || Object.ReferenceEquals(op.next, op) || (not isOpen && Object.ReferenceEquals(op.next, op.prev)) then false
        else
            path.Clear()

            let mutable op = op
            let mutable lastPt = Point64()
            let mutable op2: OutPt = null
            if reverse then
                lastPt <- op.pt
                op2 <- op.prev
            else
                op <- op.next
                lastPt <- op.pt
                op2 <- op.next
            path.Add(lastPt)

            while not (Object.ReferenceEquals(op2, op)) do
                if op2.pt <> lastPt then
                    lastPt <- op2.pt
                    path.Add(lastPt)
                if reverse then
                    op2 <- op2.prev
                else
                    op2 <- op2.next

            path.Count <> 3 || isOpen || not (ClipperBase.IsVerySmallTriangle(op2))

    member internal this.BuildPaths(solutionClosed: Paths64, solutionOpen: Paths64) : bool =
        solutionClosed.Clear()
        solutionOpen.Clear()
        ClipperEngine.EnsureCapacity(solutionClosed, _outrecList.Count)
        ClipperEngine.EnsureCapacity(solutionOpen, _outrecList.Count)

        let mutable i = 0
        while i < _outrecList.Count do
            let outrec = _outrecList.[i]
            i <- i + 1

            if not (isNull outrec.pts) then
                let path = Path64(outrec.outPtCount)
                if outrec.isOpen then
                    if ClipperBase.BuildPath(outrec.pts, this.ReverseSolution, true, path) then
                        solutionOpen.Add(path)
                else
                    this.CleanCollinear(outrec)
                    if ClipperBase.BuildPath(outrec.pts, this.ReverseSolution, false, path) then
                        solutionClosed.Add(path)
        true

    member private this.CheckBounds(outrec: OutRec) : bool =
        if isNull outrec.pts then false
        elif not (outrec.bounds.IsEmpty()) then true
        else
            this.CleanCollinear(outrec)
            if isNull outrec.pts ||
               not (ClipperBase.BuildPath(outrec.pts, this.ReverseSolution, false, outrec.path)) then
                false
            else
                outrec.bounds <- InternalClipper.GetBounds(outrec.path)
                true

    member private this.CheckSplitOwner(outrec: OutRec, splits: List<int>) : bool =
        let mutable i = 0
        let mutable found = false
        while i < splits.Count && not found do
            let mutable split = _outrecList.[splits.[i]]
            i <- i + 1
            if isNull split then ()
            elif isNull split.pts && not (isNull split.splits) &&
                 this.CheckSplitOwner(outrec, split.splits) then
                found <- true
            else
                split <- ClipperBase.GetRealOutRec(split)
                if isNull split ||
                   obj.ReferenceEquals(split, outrec) ||
                   obj.ReferenceEquals(split.recursiveSplit, outrec) then ()
                else
                    split.recursiveSplit <- outrec
                    if not (isNull split.splits) && this.CheckSplitOwner(outrec, split.splits) then
                        found <- true
                    elif this.CheckBounds(split) &&
                         split.bounds.Contains(outrec.bounds) &&
                         ClipperBase.Path1InsidePath2(outrec.pts, split.pts) then
                        if not (ClipperBase.IsValidOwner(outrec, split)) then
                            split.owner <- outrec.owner
                        outrec.owner <- split
                        found <- true
        found

    member private this.RecursiveCheckOwners(outrec: OutRec, polypath: PolyPathBase) : unit =
        if not (isNull outrec.polypath) || outrec.bounds.IsEmpty() then ()
        else
            let mutable ownerFound = false
            while not ownerFound && not (isNull outrec.owner) do
                if (not (isNull outrec.owner.splits) &&
                    this.CheckSplitOwner(outrec, outrec.owner.splits)) ||
                   (not (isNull outrec.owner.pts) &&
                    this.CheckBounds(outrec.owner) &&
                    ClipperBase.Path1InsidePath2(outrec.pts, outrec.owner.pts)) then
                    ownerFound <- true
                else
                    outrec.owner <- outrec.owner.owner

            if not (isNull outrec.owner) then
                if isNull outrec.owner.polypath then
                    this.RecursiveCheckOwners(outrec.owner, polypath)
                outrec.polypath <- outrec.owner.polypath.AddChild(outrec.path)
            else
                outrec.polypath <- polypath.AddChild(outrec.path)

    member internal this.BuildTree(polytree: PolyPathBase, solutionOpen: Paths64) : unit =
        polytree.Clear()
        solutionOpen.Clear()
        if _hasOpenPaths then
            ClipperEngine.EnsureCapacity(solutionOpen, _outrecList.Count)

        let mutable i = 0
        while i < _outrecList.Count do
            let outrec = _outrecList.[i]
            i <- i + 1

            if not (isNull outrec.pts) then
                if outrec.isOpen then
                    let openPath = Path64(outrec.outPtCount)
                    if ClipperBase.BuildPath(outrec.pts, this.ReverseSolution, true, openPath) then
                        solutionOpen.Add(openPath)
                else
                    if this.CheckBounds(outrec) then
                        this.RecursiveCheckOwners(outrec, polytree)

    member this.GetBounds() : Rect64 =
        let mutable bounds = ClipperPrimitives.InvalidRect64
        for t in _vertexList do
            let mutable v = t
            let mutable done_ = false
            while not done_ do
                if v.pt.X < bounds.left then bounds.left <- v.pt.X
                if v.pt.X > bounds.right then bounds.right <- v.pt.X
                if v.pt.Y < bounds.top then bounds.top <- v.pt.Y
                if v.pt.Y > bounds.bottom then bounds.bottom <- v.pt.Y
                v <- v.next
                done_ <- Object.ReferenceEquals(v, t)
        if bounds.IsEmpty() then Rect64(0L, 0L, 0L, 0L) else bounds

// PolyPath64 (: PolyPathBase)
[<AllowNullLiteral>]
type PolyPath64([<Optional; DefaultParameterValue(null: PolyPathBase)>] parent: PolyPathBase) =
    inherit PolyPathBase(parent)

    // new() = PolyPath64(null)

    member val Polygon: Path64 = null with get, set

    override this.AddChild(p: Path64) : PolyPathBase =
        let newChild = PolyPath64(this) :> PolyPathBase
        (newChild :?> PolyPath64).Polygon <- p
        this._childs.Add(newChild)
        newChild

    member this.Item
        with get(index: int) : PolyPath64 =
            if index < 0 || index >= this._childs.Count then
                raise (InvalidOperationException())
            this._childs.[index] :?> PolyPath64

    member this.Child(index: int) : PolyPath64 =
        if index < 0 || index >= this._childs.Count then
            raise (InvalidOperationException())
        this._childs.[index] :?> PolyPath64

    member this.Area() : float =
        let mutable result = if isNull this.Polygon then 0.0 else ClipperPrimitives.Area(this.Polygon)
        for polyPathBase in this._childs do
            let child = polyPathBase :?> PolyPath64
            result <- result + child.Area()
        result

// PolyTree64 (: PolyPath64)
[<AllowNullLiteral>]
type PolyTree64() =
    inherit PolyPath64()

// PolyPathD (: PolyPathBase)
[<AllowNullLiteral>]
type PolyPathD([<Optional; DefaultParameterValue(null: PolyPathBase)>] parent: PolyPathBase) =
    inherit PolyPathBase(parent)

    // new() = PolyPathD(null)

    member val internal Scale: float = 0.0 with get, set
    member val Polygon: PathD = null with get, set

    override this.AddChild(p: Path64) : PolyPathBase =
        let newChild = PolyPathD(this) :> PolyPathBase
        let child = newChild :?> PolyPathD
        child.Scale <- this.Scale
        child.Polygon <- ClipperPrimitives.ScalePathD(p, 1.0 / this.Scale)
        this._childs.Add(newChild)
        newChild

    member this.AddChild(p: PathD) : PolyPathBase =
        let newChild = PolyPathD(this) :> PolyPathBase
        let child = newChild :?> PolyPathD
        child.Scale <- this.Scale
        child.Polygon <- p
        this._childs.Add(newChild)
        newChild

    member this.Item
        with get(index: int) : PolyPathD =
            if index < 0 || index >= this._childs.Count then
                raise (InvalidOperationException())
            this._childs.[index] :?> PolyPathD

    member this.Area() : float =
        let mutable result = if isNull this.Polygon then 0.0 else ClipperPrimitives.Area(this.Polygon)
        for polyPathBase in this._childs do
            let child = polyPathBase :?> PolyPathD
            result <- result + child.Area()
        result

// PolyTreeD (: PolyPathD)
type PolyTreeD() =
    inherit PolyPathD()
    member this.Scale = (this :> PolyPathD).Scale

// Clipper64 (: ClipperBase)
type Clipper64() =
    inherit ClipperBase()

    member this.AddPath(path: Path64, polytype: PathType,
                        [<Optional; DefaultParameterValue(false)>] isOpen: bool) : unit =
        base.AddPath(path, polytype, isOpen)

    member this.AddReuseableData(reuseableData: ReuseableDataContainer64) : unit =
        base.AddReuseableData(reuseableData)

    member this.AddPaths(paths: Paths64, polytype: PathType,
                         [<Optional; DefaultParameterValue(false)>] isOpen: bool) : unit =
        base.AddPaths(paths, polytype, isOpen)

    member this.AddSubject(paths: Paths64) : unit =
        this.AddPaths(paths, PathType.Subject)

    member this.AddOpenSubject(paths: Paths64) : unit =
        this.AddPaths(paths, PathType.Subject, true)

    member this.AddClip(paths: Paths64) : unit =
        this.AddPaths(paths, PathType.Clip)

    member this.Execute(clipType: ClipType, fillRule: FillRule, solutionClosed: Paths64, solutionOpen: Paths64) : bool =
        solutionClosed.Clear()
        solutionOpen.Clear()
        try
            this.ExecuteInternal(clipType, fillRule)
            this.BuildPaths(solutionClosed, solutionOpen) |> ignore
        with
        | _ -> this._succeeded <- false

        this.ClearSolutionOnly()
        this._succeeded

    member this.Execute(clipType: ClipType, fillRule: FillRule, solutionClosed: Paths64) : bool =
        this.Execute(clipType, fillRule, solutionClosed, Paths64())

    member this.Execute(clipType: ClipType, fillRule: FillRule, polytree: PolyTree64, openPaths: Paths64) : bool =
        polytree.Clear()
        openPaths.Clear()
        this._using_polytree <- true
        try
            this.ExecuteInternal(clipType, fillRule)
            this.BuildTree(polytree, openPaths)
        with
        | _ -> this._succeeded <- false

        this.ClearSolutionOnly()
        this._succeeded

    member this.Execute(clipType: ClipType, fillRule: FillRule, polytree: PolyTree64) : bool =
        this.Execute(clipType, fillRule, polytree, Paths64())

#if USINGZ
    member this.ZCallback
        with get () = this._zCallback
        and set value = this._zCallback <- value
#endif

#if USINGZ
type ZCallbackD = delegate of PointD * PointD * PointD * PointD * byref<PointD> -> unit
#endif

// ClipperD (: ClipperBase)
type ClipperD([<Optional; DefaultParameterValue(2)>] roundingDecimalPrecision: int) =
    inherit ClipperBase()

    static let precision_range_error = "Engine Clipper2 Error: Precision is out of range."

    let _scale =
        if roundingDecimalPrecision < -8 || roundingDecimalPrecision > 8 then
            raise (ClipperLibException(precision_range_error))
        Math.Pow(10.0, float roundingDecimalPrecision)

    let _invScale = 1.0 / _scale

    // new() = ClipperD(2)

#if USINGZ
    member val ZCallback: ZCallbackD = null with get, set

    member private this.CheckZCallback() : unit =
        if isNull this.ZCallback then
            this._zCallback <- null
        else
            this._zCallback <- ZCallback64(fun bot1 top1 bot2 top2 intersectPt -> this.ZCB(bot1, top1, bot2, top2, &intersectPt))

    member private this.ZCB(bot1: Point64, top1: Point64, bot2: Point64, top2: Point64, intersectPt: byref<Point64>) : unit =
        let mutable tmp = ClipperPrimitives.ScalePointD(intersectPt, _invScale)
        this.ZCallback.Invoke(
            ClipperPrimitives.ScalePointD(bot1, _invScale),
            ClipperPrimitives.ScalePointD(top1, _invScale),
            ClipperPrimitives.ScalePointD(bot2, _invScale),
            ClipperPrimitives.ScalePointD(top2, _invScale),
            &tmp)
        intersectPt <- Point64(intersectPt.X, intersectPt.Y, tmp.z)
#endif

    member this.AddPath(path: PathD, polytype: PathType,
                     [<Optional; DefaultParameterValue(false)>] isOpen: bool) : unit =
        base.AddPath(ClipperPrimitives.ScalePath64(path, _scale), polytype, isOpen)

    member this.AddPaths(paths: PathsD, polytype: PathType,
                      [<Optional; DefaultParameterValue(false)>] isOpen: bool) : unit =
        base.AddPaths(ClipperPrimitives.ScalePaths64(paths, _scale), polytype, isOpen)

    member this.AddSubject(path: PathD) : unit = this.AddPath(path, PathType.Subject)
    member this.AddOpenSubject(path: PathD) : unit = this.AddPath(path, PathType.Subject, true)
    member this.AddClip(path: PathD) : unit = this.AddPath(path, PathType.Clip)
    member this.AddSubject(paths: PathsD) : unit = this.AddPaths(paths, PathType.Subject)
    member this.AddOpenSubject(paths: PathsD) : unit = this.AddPaths(paths, PathType.Subject, true)
    member this.AddClip(paths: PathsD) : unit = this.AddPaths(paths, PathType.Clip)

    member this.Execute(clipType: ClipType, fillRule: FillRule, solutionClosed: PathsD, solutionOpen: PathsD) : bool =
        let solClosed64 = Paths64()
        let solOpen64 = Paths64()

        let mutable success = true
        solutionClosed.Clear()
        solutionOpen.Clear()
#if USINGZ
        this.CheckZCallback()
#endif
        try
            this.ExecuteInternal(clipType, fillRule)
            this.BuildPaths(solClosed64, solOpen64) |> ignore
        with
        | _ -> success <- false

        this.ClearSolutionOnly()
        if not success then false
        else
            for path in solClosed64 do
                solutionClosed.Add(ClipperPrimitives.ScalePathD(path, _invScale))
            for path in solOpen64 do
                solutionOpen.Add(ClipperPrimitives.ScalePathD(path, _invScale))
            true

    member this.Execute(clipType: ClipType, fillRule: FillRule, solutionClosed: PathsD) : bool =
        this.Execute(clipType, fillRule, solutionClosed, PathsD())

    member this.Execute(clipType: ClipType, fillRule: FillRule, polytree: PolyTreeD, openPaths: PathsD) : bool =
        polytree.Clear()
        openPaths.Clear()
        this._using_polytree <- true
        (polytree :> PolyPathD).Scale <- _scale

        let oPaths = Paths64()
        let mutable success = true
#if USINGZ
        this.CheckZCallback()
#endif
        try
            this.ExecuteInternal(clipType, fillRule)
            this.BuildTree(polytree, oPaths)
        with
        | _ -> success <- false

        this.ClearSolutionOnly()
        if not success then false
        else
            for path in oPaths do
                openPaths.Add(ClipperPrimitives.ScalePathD(path, _invScale))
            true

    member this.Execute(clipType: ClipType, fillRule: FillRule, polytree: PolyTreeD) : bool =
        this.Execute(clipType, fillRule, polytree, PathsD())

// ClipperLibException
and ClipperLibException(description: string) =
    inherit Exception(description)

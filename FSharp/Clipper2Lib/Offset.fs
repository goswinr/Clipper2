#if USINGZ
namespace Clipper2ZLib
#else
namespace Clipper2Lib
#endif

open System
open System.Collections.Generic
open System.Runtime.InteropServices

type JoinType =
    | Miter = 0
    | Square = 1
    | Bevel = 2
    | Round = 3

type EndType =
    | Polygon = 0
    | Joined = 1
    | Butt = 2
    | Square = 3
    | Round = 4

// Delegate type for dynamic delta (declared before ClipperOffset so it can be referenced)
type DeltaCallback64 = delegate of Path64 * PathD * int * int -> float

// Group class (was nested in C#, extracted for F#)
[<AllowNullLiteral>]
type OffsetGroup internal (paths: Paths64, joinType: JoinType, endType: EndType) =

    let mutable _inPaths = Paths64()
    let mutable _pathsReversed = false
    let mutable _lowestPathIdx = -1

    do
        let isJoined = (endType = EndType.Polygon) || (endType = EndType.Joined)
        let inp = Paths64(paths.Count)
        for path in paths do
            inp.Add(ClipperPrimitives.StripDuplicates(path, isJoined))
        _inPaths <- inp

        if endType = EndType.Polygon then
            let mutable lowestIdx = 0
            let mutable isNeg = false
            ClipperOffset.GetLowestPathInfo(inp, &lowestIdx, &isNeg)
            _lowestPathIdx <- lowestIdx
            // the lowermost path must be an outer path, so if its orientation is negative,
            // then flag that the whole group is 'reversed' (will negate delta etc.)
            // as this is much more efficient than reversing every path.
            _pathsReversed <- (lowestIdx >= 0) && isNeg
        else
            _lowestPathIdx <- -1
            _pathsReversed <- false

    member _.inPaths
        with get () = _inPaths
        and set v = _inPaths <- v
    member _.joinType = joinType
    member _.endType = endType
    member _.pathsReversed
        with get () = _pathsReversed
        and set v = _pathsReversed <- v
    member _.lowestPathIdx
        with get () = _lowestPathIdx
        and set v = _lowestPathIdx <- v

and ClipperOffset([<Optional; DefaultParameterValue(2.0)>] miterLimit: float,
                   [<Optional; DefaultParameterValue(0.0)>] arcTolerance: float,
                   [<Optional; DefaultParameterValue(false)>] preserveCollinear: bool,
                   [<Optional; DefaultParameterValue(false)>] reverseSolution: bool) =

    static let arc_const = 0.002 // <-- 1/500

    let _groupList = List<OffsetGroup>()
    let mutable pathOut = Path64()
    let _normals = PathD()
    let mutable _solution = Paths64()
    let mutable _solutionTree: PolyTree64 = null

    let mutable _groupDelta = 0.0
    let mutable _delta = 0.0
    let mutable _mitLimSqr = 0.0
    let mutable _stepsPerRad = 0.0
    let mutable _stepSin = 0.0
    let mutable _stepCos = 0.0
    let mutable _joinType = JoinType.Miter
    let mutable _endType = EndType.Polygon

    member val ArcTolerance = arcTolerance with get, set
    member val MergeGroups = true with get, set
    member val MiterLimit = miterLimit with get, set
    member val PreserveCollinear = preserveCollinear with get, set
    member val ReverseSolution = reverseSolution with get, set

    [<DefaultValue>]
    val mutable DeltaCallback: DeltaCallback64

#if USINGZ
    member val ZCallback: ZCallback64 = null with get, set

    member internal this.ZCB(bot1: Point64, top1: Point64, bot2: Point64, top2: Point64, ip: byref<Point64>) =
        if bot1.Z <> 0L && (bot1.Z = bot2.Z || bot1.Z = top2.Z) then ip.Z <- bot1.Z
        elif bot2.Z <> 0L && bot2.Z = top1.Z then ip.Z <- bot2.Z
        elif top1.Z <> 0L && top1.Z = top2.Z then ip.Z <- top1.Z
        elif this.ZCallback <> null then this.ZCallback.Invoke(bot1, top1, bot2, top2, &ip)
#endif

    member this.Clear() = _groupList.Clear()

    member this.AddPath(path: Path64, joinType: JoinType, endType: EndType) : unit =
        let cnt = path.Count
        if cnt = 0 then ()
        else
            let pp = Paths64(1)
            pp.Add(path)
            this.AddPaths(pp, joinType, endType)

    member this.AddPaths(paths: Paths64, joinType: JoinType, endType: EndType) : unit =
        let cnt = paths.Count
        if cnt = 0 then ()
        else
            _groupList.Add(OffsetGroup(paths, joinType, endType))

    member private this.CalcSolutionCapacity() : int =
        let mutable result = 0
        for g in _groupList do
            result <- result + (if g.endType = EndType.Joined then g.inPaths.Count * 2 else g.inPaths.Count)
        result

    member internal this.CheckPathsReversed() : bool =
        let mutable result = false
        let mutable break_ = false
        let mutable i = 0
        while i < _groupList.Count && not break_ do
            if _groupList.[i].endType = EndType.Polygon then
                result <- _groupList.[i].pathsReversed
                break_ <- true
            else
                i <- i + 1
        result

    static member inline internal GetUnitNormal(pt1: Point64, pt2: Point64) : PointD =
        let mutable dx = float (pt2.X - pt1.X)
        let mutable dy = float (pt2.Y - pt1.Y)
        if (dx = 0.0) && (dy = 0.0) then PointD()
        else
            let f = 1.0 / Math.Sqrt(dx * dx + dy * dy)
            dx <- dx * f
            dy <- dy * f
            PointD(dy, -dx)

    static member internal GetLowestPathInfo(paths: Paths64, idx: byref<int>, isNegArea: byref<bool>) : unit =
        idx <- -1
        isNegArea <- false
        let mutable botPt = Point64(Int64.MaxValue, Int64.MinValue)
        for i = 0 to paths.Count - 1 do
            let mutable a = Double.MaxValue
            let mutable break_ = false
            let mutable j = 0
            while j < paths.[i].Count && not break_ do
                let pt = paths.[i].[j]
                if (pt.Y < botPt.Y) || ((pt.Y = botPt.Y) && (pt.X >= botPt.X)) then
                    j <- j + 1 // continue
                else
                    if a = Double.MaxValue then
                        a <- ClipperPrimitives.Area(paths.[i])
                        if a = 0.0 then
                            break_ <- true // invalid closed path so break from inner loop
                    if not break_ then
                        isNegArea <- a < 0.0
                        idx <- i
                        botPt.X <- pt.X
                        botPt.Y <- pt.Y
                        j <- j + 1
        ()

    static member inline private TranslatePoint(pt: PointD, dx: float, dy: float) : PointD =
#if USINGZ
        PointD(pt.x + dx, pt.y + dy, pt.z)
#else
        PointD(pt.x + dx, pt.y + dy)
#endif

    static member inline private ReflectPoint(pt: PointD, pivot: PointD) : PointD =
#if USINGZ
        PointD(pivot.x + (pivot.x - pt.x), pivot.y + (pivot.y - pt.y), pt.z)
#else
        PointD(pivot.x + (pivot.x - pt.x), pivot.y + (pivot.y - pt.y))
#endif

    static member inline private AlmostZero(value: float, [<Optional; DefaultParameterValue(0.001)>] epsilon: float) : bool =
        Math.Abs(value) < epsilon

    static member inline private Hypotenuse(x: float, y: float) : float =
        Math.Sqrt(Math.Pow(x, 2.0) + Math.Pow(y, 2.0))

    static member inline private NormalizeVector(vec: PointD) : PointD =
        let h = ClipperOffset.Hypotenuse(vec.x, vec.y)
        if ClipperOffset.AlmostZero(h) then PointD(0.0, 0.0)
        else
            let inverseHypot = 1.0 / h
            PointD(vec.x * inverseHypot, vec.y * inverseHypot)

    static member inline private GetAvgUnitVector(vec1: PointD, vec2: PointD) : PointD =
        ClipperOffset.NormalizeVector(PointD(vec1.x + vec2.x, vec1.y + vec2.y))

    member inline private this.GetPerpendic(pt: Point64, norm: PointD) : Point64 =
#if USINGZ
        Point64(float pt.X + norm.x * _groupDelta,
            float pt.Y + norm.y * _groupDelta, float pt.Z)
#else
        Point64(float pt.X + norm.x * _groupDelta,
            float pt.Y + norm.y * _groupDelta)
#endif

    member inline private this.GetPerpendicD(pt: Point64, norm: PointD) : PointD =
#if USINGZ
        PointD(float pt.X + norm.x * _groupDelta,
            float pt.Y + norm.y * _groupDelta, pt.Z)
#else
        PointD(float pt.X + norm.x * _groupDelta,
            float pt.Y + norm.y * _groupDelta)
#endif

    member inline private this.DoBevel(path: Path64, j: int, k: int) : unit =
        let mutable pt1 = Point64()
        let mutable pt2 = Point64()
        if j = k then
            let absDelta = Math.Abs(_groupDelta)
#if USINGZ
            pt1 <- Point64(
                float path.[j].X - absDelta * _normals.[j].x,
                float path.[j].Y - absDelta * _normals.[j].y, float path.[j].Z)
            pt2 <- Point64(
                float path.[j].X + absDelta * _normals.[j].x,
                float path.[j].Y + absDelta * _normals.[j].y, float path.[j].Z)
#else
            pt1 <- Point64(
                float path.[j].X - absDelta * _normals.[j].x,
                float path.[j].Y - absDelta * _normals.[j].y)
            pt2 <- Point64(
                float path.[j].X + absDelta * _normals.[j].x,
                float path.[j].Y + absDelta * _normals.[j].y)
#endif
        else
#if USINGZ
            pt1 <- Point64(
                float path.[j].X + _groupDelta * _normals.[k].x,
                float path.[j].Y + _groupDelta * _normals.[k].y, float path.[j].Z)
            pt2 <- Point64(
                float path.[j].X + _groupDelta * _normals.[j].x,
                float path.[j].Y + _groupDelta * _normals.[j].y, float path.[j].Z)
#else
            pt1 <- Point64(
                float path.[j].X + _groupDelta * _normals.[k].x,
                float path.[j].Y + _groupDelta * _normals.[k].y)
            pt2 <- Point64(
                float path.[j].X + _groupDelta * _normals.[j].x,
                float path.[j].Y + _groupDelta * _normals.[j].y)
#endif
        pathOut.Add(pt1)
        pathOut.Add(pt2)

    member inline private this.DoSquare(path: Path64, j: int, k: int) : unit =
        let mutable vec = PointD()
        if j = k then
            vec <- PointD(_normals.[j].y, -_normals.[j].x)
        else
            vec <- ClipperOffset.GetAvgUnitVector(
                PointD(-_normals.[k].y, _normals.[k].x),
                PointD(_normals.[j].y, -_normals.[j].x))

        let absDelta = Math.Abs(_groupDelta)
        // now offset the original vertex delta units along unit vector
        let mutable ptQ = PointD(path.[j])
        ptQ <- ClipperOffset.TranslatePoint(ptQ, absDelta * vec.x, absDelta * vec.y)

        // get perpendicular vertices
        let pt1 = ClipperOffset.TranslatePoint(ptQ, _groupDelta * vec.y, _groupDelta * -vec.x)
        let pt2 = ClipperOffset.TranslatePoint(ptQ, _groupDelta * -vec.y, _groupDelta * vec.x)
        // get 2 vertices along one edge offset
        let pt3 = this.GetPerpendicD(path.[k], _normals.[k])

        if j = k then
            let pt4 = PointD(
                pt3.x + vec.x * _groupDelta,
                pt3.y + vec.y * _groupDelta)
            let mutable pt = PointD()
            InternalClipper.GetLineIntersectPt(pt1, pt2, pt3, pt4, &pt) |> ignore
#if USINGZ
            pt.z <- ptQ.z
#endif
            //get the second intersect point through reflection
            pathOut.Add(Point64(ClipperOffset.ReflectPoint(pt, ptQ)))
            pathOut.Add(Point64(pt))
        else
            let pt4 = this.GetPerpendicD(path.[j], _normals.[k])
            let mutable pt = PointD()
            InternalClipper.GetLineIntersectPt(pt1, pt2, pt3, pt4, &pt) |> ignore
#if USINGZ
            pt.z <- ptQ.z
#endif
            pathOut.Add(Point64(pt))
            //get the second intersect point through reflection
            pathOut.Add(Point64(ClipperOffset.ReflectPoint(pt, ptQ)))

    member inline private this.DoMiter(path: Path64, j: int, k: int, cosA: float) : unit =
        let q = _groupDelta / (cosA + 1.0)
#if USINGZ
        pathOut.Add(Point64(
            float path.[j].X + (_normals.[k].x + _normals.[j].x) * q,
            float path.[j].Y + (_normals.[k].y + _normals.[j].y) * q,
            float path.[j].Z))
#else
        pathOut.Add(Point64(
            float path.[j].X + (_normals.[k].x + _normals.[j].x) * q,
            float path.[j].Y + (_normals.[k].y + _normals.[j].y) * q))
#endif

    member inline private this.DoRound(path: Path64, j: int, k: int, angle: float) : unit =
        if not (isNull (this.DeltaCallback :> obj)) then
            // when DeltaCallback is assigned, _groupDelta won't be constant,
            // so we'll need to do the following calculations for *every* vertex.
            let absDelta = Math.Abs(_groupDelta)
            let arcTol = if this.ArcTolerance > 0.01 then this.ArcTolerance else absDelta * arc_const
            let stepsPer360 = Math.PI / Math.Acos(1.0 - arcTol / absDelta)
            _stepSin <- Math.Sin((2.0 * Math.PI) / stepsPer360)
            _stepCos <- Math.Cos((2.0 * Math.PI) / stepsPer360)
            if _groupDelta < 0.0 then _stepSin <- -_stepSin
            _stepsPerRad <- stepsPer360 / (2.0 * Math.PI)

        let pt = path.[j]
        let mutable offsetVec = PointD(_normals.[k].x * _groupDelta, _normals.[k].y * _groupDelta)
        if j = k then offsetVec.Negate()
#if USINGZ
        pathOut.Add(Point64(float pt.X + offsetVec.x, float pt.Y + offsetVec.y, float pt.Z))
#else
        pathOut.Add(Point64(float pt.X + offsetVec.x, float pt.Y + offsetVec.y))
#endif
        let steps = int (Math.Ceiling(_stepsPerRad * Math.Abs(angle)))
        for i = 1 to steps - 1 do // ie 1 less than steps
            offsetVec <- PointD(offsetVec.x * _stepCos - _stepSin * offsetVec.y,
                offsetVec.x * _stepSin + offsetVec.y * _stepCos)
#if USINGZ
            pathOut.Add(Point64(float pt.X + offsetVec.x, float pt.Y + offsetVec.y, float pt.Z))
#else
            pathOut.Add(Point64(float pt.X + offsetVec.x, float pt.Y + offsetVec.y))
#endif
        pathOut.Add(this.GetPerpendic(pt, _normals.[j]))

    member inline private this.BuildNormals(path: Path64) : unit =
        let cnt = path.Count
        _normals.Clear()
        if cnt = 0 then ()
        else
            ClipperEngine.EnsureCapacity(_normals, cnt)
            for i = 0 to cnt - 2 do
                _normals.Add(ClipperOffset.GetUnitNormal(path.[i], path.[i + 1]))
            _normals.Add(ClipperOffset.GetUnitNormal(path.[cnt - 1], path.[0]))

    member private this.OffsetPoint(group: OffsetGroup, path: Path64, j: int, k: byref<int>) : unit =
        if path.[j] = path.[k] then k <- j
        else
            // Let A = change in angle where edges join
            // A == 0: ie no change in angle (flat join)
            // A == PI: edges 'spike'
            // sin(A) < 0: right turning
            // cos(A) < 0: change in angle is more than 90 degree
            let mutable sinA = InternalClipper.CrossProduct(_normals.[j], _normals.[k])
            let cosA = InternalClipper.DotProduct(_normals.[j], _normals.[k])
            if sinA > 1.0 then sinA <- 1.0
            elif sinA < -1.0 then sinA <- -1.0

            if not (isNull (this.DeltaCallback :> obj)) then
                _groupDelta <- this.DeltaCallback.Invoke(path, _normals, j, k)
                if group.pathsReversed then _groupDelta <- -_groupDelta

            if Math.Abs(_groupDelta) < ClipperOffset.Tolerance then
                pathOut.Add(path.[j])
            elif cosA > -0.999 && (sinA * _groupDelta < 0.0) then // test for concavity first (#593)
                // is concave
                pathOut.Add(this.GetPerpendic(path.[j], _normals.[k]))
                pathOut.Add(path.[j]) // (#405, #873, #916)
                pathOut.Add(this.GetPerpendic(path.[j], _normals.[j]))
            elif (cosA > 0.999) && (_joinType <> JoinType.Round) then
                // almost straight - less than 2.5 degree (#424, #482, #526 & #724)
                this.DoMiter(path, j, k, cosA)
            elif _joinType = JoinType.Miter && cosA > _mitLimSqr - 1.0 then
                // miter unless the angle is sufficiently acute to exceed ML
                this.DoMiter(path, j, k, cosA)
            elif _joinType = JoinType.Miter then
                this.DoSquare(path, j, k)
            elif _joinType = JoinType.Round then
                this.DoRound(path, j, k, Math.Atan2(sinA, cosA))
            elif _joinType = JoinType.Bevel then
                this.DoBevel(path, j, k)
            else
                this.DoSquare(path, j, k)

            k <- j

    member inline private this.OffsetPolygon(group: OffsetGroup, path: Path64) : unit =
        pathOut <- Path64()
        let cnt = path.Count
        let mutable prev = cnt - 1
        for i = 0 to cnt - 1 do
            this.OffsetPoint(group, path, i, &prev)
        _solution.Add(pathOut)

    member inline private this.OffsetOpenJoined(group: OffsetGroup, path: Path64) : unit =
        this.OffsetPolygon(group, path)
        let path2 = ClipperPrimitives.ReversePath(path)
        this.BuildNormals(path2)
        this.OffsetPolygon(group, path2)

    member private this.OffsetOpenPath(group: OffsetGroup, path: Path64) : unit =
        pathOut <- Path64()
        let highI = path.Count - 1

        if not (isNull (this.DeltaCallback :> obj)) then
            _groupDelta <- this.DeltaCallback.Invoke(path, _normals, 0, 0)

        // do the line start cap
        if Math.Abs(_groupDelta) < ClipperOffset.Tolerance then
            pathOut.Add(path.[0])
        else
            if _endType = EndType.Butt then
                this.DoBevel(path, 0, 0)
            elif _endType = EndType.Round then
                this.DoRound(path, 0, 0, Math.PI)
            else
                this.DoSquare(path, 0, 0)

        // offset the left side going forward
        let mutable k = 0
        for i = 1 to highI - 1 do
            this.OffsetPoint(group, path, i, &k)

        // reverse normals ...
        for i = highI downto 1 do
            _normals.[i] <- PointD(-_normals.[i - 1].x, -_normals.[i - 1].y)
        _normals.[0] <- _normals.[highI]

        if not (isNull (this.DeltaCallback :> obj)) then
            _groupDelta <- this.DeltaCallback.Invoke(path, _normals, highI, highI)

        // do the line end cap
        if Math.Abs(_groupDelta) < ClipperOffset.Tolerance then
            pathOut.Add(path.[highI])
        else
            if _endType = EndType.Butt then
                this.DoBevel(path, highI, highI)
            elif _endType = EndType.Round then
                this.DoRound(path, highI, highI, Math.PI)
            else
                this.DoSquare(path, highI, highI)

        // offset the left side going back
        let mutable k2 = highI
        for i = highI - 1 downto 1 do
            this.OffsetPoint(group, path, i, &k2)

        _solution.Add(pathOut)

    member private this.DoGroupOffset(group: OffsetGroup) : unit =
        if group.endType = EndType.Polygon then
            // a straight path (2 points) can now also be 'polygon' offset
            // where the ends will be treated as (180 deg.) joins
            if group.lowestPathIdx < 0 then _delta <- Math.Abs(_delta)
            _groupDelta <- if group.pathsReversed then -_delta else _delta
        else
            _groupDelta <- Math.Abs(_delta)

        let mutable absDelta = Math.Abs(_groupDelta)

        _joinType <- group.joinType
        _endType <- group.endType

        if group.joinType = JoinType.Round || group.endType = EndType.Round then
            let arcTol = if this.ArcTolerance > 0.01 then this.ArcTolerance else absDelta * arc_const
            let stepsPer360 = Math.PI / Math.Acos(1.0 - arcTol / absDelta)
            _stepSin <- Math.Sin((2.0 * Math.PI) / stepsPer360)
            _stepCos <- Math.Cos((2.0 * Math.PI) / stepsPer360)
            if _groupDelta < 0.0 then _stepSin <- -_stepSin
            _stepsPerRad <- stepsPer360 / (2.0 * Math.PI)

        let mutable pathIt = group.inPaths.GetEnumerator()
        while pathIt.MoveNext() do
            let p = pathIt.Current

            pathOut <- Path64()
            let cnt = p.Count

            if cnt = 1 then
                let pt = p.[0]

                if not (isNull (this.DeltaCallback :> obj)) then
                    _groupDelta <- this.DeltaCallback.Invoke(p, _normals, 0, 0)
                    if group.pathsReversed then _groupDelta <- -_groupDelta
                    absDelta <- Math.Abs(_groupDelta)

                // single vertex so build a circle or square ...
                if group.endType = EndType.Round then
                    let steps = int (Math.Ceiling(_stepsPerRad * 2.0 * Math.PI))
                    pathOut <- ClipperPrimitives.Ellipse(pt, absDelta, absDelta, steps)
#if USINGZ
                    pathOut <- InternalClipper.SetZ(pathOut, pt.Z)
#endif
                else
                    let d = int (Math.Ceiling(_groupDelta))
                    let mutable r = Rect64(int64 (pt.X - int64 d), int64 (pt.Y - int64 d), int64 (pt.X + int64 d), int64 (pt.Y + int64 d))
                    pathOut <- r.AsPath()
#if USINGZ
                    pathOut <- InternalClipper.SetZ(pathOut, pt.Z)
#endif
                _solution.Add(pathOut)
            else
                if cnt = 2 && group.endType = EndType.Joined then
                    _endType <- if group.joinType = JoinType.Round then EndType.Round else EndType.Square

                this.BuildNormals(p)
                if _endType = EndType.Polygon then
                    this.OffsetPolygon(group, p)
                elif _endType = EndType.Joined then
                    this.OffsetOpenJoined(group, p)
                else
                    this.OffsetOpenPath(group, p)

    member private this.ExecuteInternal(delta: float) : unit =
        if _groupList.Count = 0 then ()
        else
            ClipperEngine.EnsureCapacity(_solution, this.CalcSolutionCapacity())

            // make sure the offset delta is significant
            if Math.Abs(delta) < 0.5 then
                for group in _groupList do
                    for path in group.inPaths do
                        _solution.Add(path)
            else
                _delta <- delta
                _mitLimSqr <-
                    if this.MiterLimit <= 1.0 then 2.0
                    else 2.0 / ClipperPrimitives.Sqr(this.MiterLimit)

                for group in _groupList do
                    this.DoGroupOffset(group)

                if _groupList.Count = 0 then ()
                else
                    let pathsReversed = this.CheckPathsReversed()
                    let fillRule = if pathsReversed then FillRule.Negative else FillRule.Positive

                    // clean up self-intersections ...
                    let c = Clipper64()
                    c.PreserveCollinear <- this.PreserveCollinear
                    c.ReverseSolution <- this.ReverseSolution <> pathsReversed
#if USINGZ
                    c.ZCallback <- ZCallback64(fun (bot1: Point64) (top1: Point64) (bot2: Point64) (top2: Point64) (ip: byref<Point64>) ->
                        this.ZCB(bot1, top1, bot2, top2, &ip))
#endif
                    c.AddSubject(_solution)
                    if not (isNull _solutionTree) then
                        c.Execute(ClipType.Union, fillRule, _solutionTree) |> ignore
                    else
                        c.Execute(ClipType.Union, fillRule, _solution) |> ignore

    member this.Execute(delta: float, solution: Paths64) : unit =
        solution.Clear()
        _solution <- solution
        this.ExecuteInternal(delta)

    member this.Execute(delta: float, solutionTree: PolyTree64) : unit =
        solutionTree.Clear()
        _solutionTree <- solutionTree
        _solution.Clear()
        this.ExecuteInternal(delta)

    member this.Execute(deltaCallback: DeltaCallback64, solution: Paths64) : unit =
        this.DeltaCallback <- deltaCallback
        this.Execute(1.0, solution)

    static member private Tolerance = 1.0E-12

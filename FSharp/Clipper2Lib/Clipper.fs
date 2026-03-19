#if USINGZ
namespace Clipper2ZLib
#else
namespace Clipper2Lib
#endif

open System
open System.Collections.Generic
open System.Runtime.InteropServices

/// Public facade API - mirrors the C# static class Clipper
[<AbstractClass; Sealed>]
type Clipper private () =

    static let mutable invalidRect64 = Rect64(false)
    static let mutable invalidRectD = RectD(false)

    static member InvalidRect64 = invalidRect64
    static member InvalidRectD = invalidRectD

    // Boolean operations (Paths64)
    static member Intersect(subject: Paths64, clip: Paths64, fillRule: FillRule) : Paths64 =
        Clipper.BooleanOp(ClipType.Intersection, subject, clip, fillRule)

    static member Union(subject: Paths64, fillRule: FillRule) : Paths64 =
        Clipper.BooleanOp(ClipType.Union, subject, null, fillRule)

    static member Union(subject: Paths64, clip: Paths64, fillRule: FillRule) : Paths64 =
        Clipper.BooleanOp(ClipType.Union, subject, clip, fillRule)

    static member Difference(subject: Paths64, clip: Paths64, fillRule: FillRule) : Paths64 =
        Clipper.BooleanOp(ClipType.Difference, subject, clip, fillRule)

    static member Xor(subject: Paths64, clip: Paths64, fillRule: FillRule) : Paths64 =
        Clipper.BooleanOp(ClipType.Xor, subject, clip, fillRule)

    // Boolean operations (PathsD)
    static member Intersect(subject: PathsD, clip: PathsD, fillRule: FillRule,
                            [<Optional; DefaultParameterValue(2)>] precision: int) : PathsD =
        Clipper.BooleanOp(ClipType.Intersection, subject, clip, fillRule, precision)

    static member Union(subject: PathsD, fillRule: FillRule) : PathsD =
        Clipper.BooleanOp(ClipType.Union, subject, null, fillRule)

    static member Union(subject: PathsD, clip: PathsD, fillRule: FillRule,
                        [<Optional; DefaultParameterValue(2)>] precision: int) : PathsD =
        Clipper.BooleanOp(ClipType.Union, subject, clip, fillRule, precision)

    static member Difference(subject: PathsD, clip: PathsD, fillRule: FillRule,
                             [<Optional; DefaultParameterValue(2)>] precision: int) : PathsD =
        Clipper.BooleanOp(ClipType.Difference, subject, clip, fillRule, precision)

    static member Xor(subject: PathsD, clip: PathsD, fillRule: FillRule,
                      [<Optional; DefaultParameterValue(2)>] precision: int) : PathsD =
        Clipper.BooleanOp(ClipType.Xor, subject, clip, fillRule, precision)

    // BooleanOp overloads
    static member BooleanOp(clipType: ClipType, subject: Paths64, clip: Paths64, fillRule: FillRule) : Paths64 =
        let solution = Paths64()
        if isNull subject then solution
        else
            let c = Clipper64()
            c.AddPaths(subject, PathType.Subject)
            if not (isNull clip) then
                c.AddPaths(clip, PathType.Clip)
            c.Execute(clipType, fillRule, solution) |> ignore
            solution

    static member BooleanOp(clipType: ClipType, subject: Paths64, clip: Paths64, polytree: PolyTree64, fillRule: FillRule) : unit =
        if not (isNull subject) then
            let c = Clipper64()
            c.AddPaths(subject, PathType.Subject)
            if not (isNull clip) then
                c.AddPaths(clip, PathType.Clip)
            c.Execute(clipType, fillRule, polytree) |> ignore

    static member BooleanOp(clipType: ClipType, subject: PathsD, clip: PathsD, fillRule: FillRule,
                            [<Optional; DefaultParameterValue(2)>] precision: int) : PathsD =
        let solution = PathsD()
        let c = ClipperD(precision)
        c.AddSubject(subject)
        if not (isNull clip) then
            c.AddClip(clip)
        c.Execute(clipType, fillRule, solution) |> ignore
        solution

    static member BooleanOp(clipType: ClipType, subject: PathsD, clip: PathsD, polytree: PolyTreeD, fillRule: FillRule,
                            [<Optional; DefaultParameterValue(2)>] precision: int) : unit =
        if not (isNull subject) then
            let c = ClipperD(precision)
            c.AddPaths(subject, PathType.Subject)
            if not (isNull clip) then
                c.AddPaths(clip, PathType.Clip)
            c.Execute(clipType, fillRule, polytree) |> ignore

    // Inflate
    static member InflatePaths(paths: Paths64, delta: float, joinType: JoinType, endType: EndType,
                               [<Optional; DefaultParameterValue(2.0)>] miterLimit: float,
                               [<Optional; DefaultParameterValue(0.0)>] arcTolerance: float) : Paths64 =
        let co = ClipperOffset(miterLimit, arcTolerance)
        co.AddPaths(paths, joinType, endType)
        let solution = Paths64()
        co.Execute(delta, solution)
        solution

    static member InflatePaths(paths: PathsD, delta: float, joinType: JoinType, endType: EndType,
                               [<Optional; DefaultParameterValue(2.0)>] miterLimit: float,
                               [<Optional; DefaultParameterValue(2)>] precision: int,
                               [<Optional; DefaultParameterValue(0.0)>] arcTolerance: float) : PathsD =
        InternalClipper.CheckPrecision(precision)
        let scale = Math.Pow(10.0, float precision)
        let tmp = ClipperPrimitives.ScalePaths64(paths, scale)
        let co = ClipperOffset(miterLimit, scale * arcTolerance)
        co.AddPaths(tmp, joinType, endType)
        co.Execute(delta * scale, tmp)
        ClipperPrimitives.ScalePathsD(tmp, 1.0 / scale)

    // RectClip
    static member RectClip(rect: Rect64, paths: Paths64) : Paths64 =
        if rect.IsEmpty() || paths.Count = 0 then Paths64()
        else
            let rc = RectClip64(rect)
            rc.Execute(paths)

    static member RectClip(rect: Rect64, path: Path64) : Paths64 =
        if rect.IsEmpty() || path.Count = 0 then Paths64()
        else
            let tmp = Paths64()
            tmp.Add(path)
            Clipper.RectClip(rect, tmp)

    static member RectClip(rect: RectD, paths: PathsD,
                           [<Optional; DefaultParameterValue(2)>] precision: int) : PathsD =
        InternalClipper.CheckPrecision(precision)
        if rect.IsEmpty() || paths.Count = 0 then PathsD()
        else
            let scale = Math.Pow(10.0, float precision)
            let r = ClipperPrimitives.ScaleRect(rect, scale)
            let tmpPath = ClipperPrimitives.ScalePaths64(paths, scale)
            let rc = RectClip64(r)
            let tmpPath = rc.Execute(tmpPath)
            ClipperPrimitives.ScalePathsD(tmpPath, 1.0 / scale)

    static member RectClip(rect: RectD, path: PathD,
                           [<Optional; DefaultParameterValue(2)>] precision: int) : PathsD =
        if rect.IsEmpty() || path.Count = 0 then PathsD()
        else
            let tmp = PathsD()
            tmp.Add(path)
            Clipper.RectClip(rect, tmp, precision)

    // RectClipLines
    static member RectClipLines(rect: Rect64, paths: Paths64) : Paths64 =
        if rect.IsEmpty() || paths.Count = 0 then Paths64()
        else
            let rc = RectClipLines64(rect)
            rc.Execute(paths)

    static member RectClipLines(rect: Rect64, path: Path64) : Paths64 =
        if rect.IsEmpty() || path.Count = 0 then Paths64()
        else
            let tmp = Paths64()
            tmp.Add(path)
            Clipper.RectClipLines(rect, tmp)

    static member RectClipLines(rect: RectD, paths: PathsD,
                                [<Optional; DefaultParameterValue(2)>] precision: int) : PathsD =
        InternalClipper.CheckPrecision(precision)
        if rect.IsEmpty() || paths.Count = 0 then PathsD()
        else
            let scale = Math.Pow(10.0, float precision)
            let r = ClipperPrimitives.ScaleRect(rect, scale)
            let tmpPath = ClipperPrimitives.ScalePaths64(paths, scale)
            let rc = RectClipLines64(r)
            let tmpPath = rc.Execute(tmpPath)
            ClipperPrimitives.ScalePathsD(tmpPath, 1.0 / scale)

    static member RectClipLines(rect: RectD, path: PathD,
                                [<Optional; DefaultParameterValue(2)>] precision: int) : PathsD =
        if rect.IsEmpty() || path.Count = 0 then PathsD()
        else
            let tmp = PathsD()
            tmp.Add(path)
            Clipper.RectClipLines(rect, tmp, precision)

    // Minkowski wrappers
    static member MinkowskiSum(pattern: Path64, path: Path64, isClosed: bool) : Paths64 =
        Minkowski.Sum(pattern, path, isClosed)

    static member MinkowskiSum(pattern: PathD, path: PathD, isClosed: bool) : PathsD =
        Minkowski.Sum(pattern, path, isClosed)

    static member MinkowskiDiff(pattern: Path64, path: Path64, isClosed: bool) : Paths64 =
        Minkowski.Diff(pattern, path, isClosed)

    static member MinkowskiDiff(pattern: PathD, path: PathD, isClosed: bool) : PathsD =
        Minkowski.Diff(pattern, path, isClosed)

    // Area
    static member Area(path: Path64) : float = ClipperPrimitives.Area(path)
    static member Area(paths: Paths64) : float = ClipperPrimitives.Area(paths)
    static member Area(path: PathD) : float = ClipperPrimitives.Area(path)
    static member Area(paths: PathsD) : float = ClipperPrimitives.Area(paths)

    static member inline IsPositive(poly: Path64) : bool = ClipperPrimitives.IsPositive(poly)

    static member inline IsPositive(poly: PathD) : bool = ClipperPrimitives.IsPositive(poly)

    // String helpers
    static member Path64ToString(path: Path64) : string =
        let mutable result = ""
        for pt in path do
            result <- result + pt.ToString()
        result + "\n"

    static member Paths64ToString(paths: Paths64) : string =
        let mutable result = ""
        for path in paths do
            result <- result + Clipper.Path64ToString(path)
        result

    static member PathDToString(path: PathD) : string =
        path.ToString(2) + "\n"

    static member PathsDToString(paths: PathsD) : string =
        let mutable result = ""
        for path in paths do
            result <- result + Clipper.PathDToString(path)
        result

    // Offset / Translate
    static member OffsetPath(path: Path64, dx: int64, dy: int64) : Path64 =
        let result = Path64(path.Count)
        for pt in path do
            result.Add(Point64(pt.X + dx, pt.Y + dy))
        result

    static member inline ScalePoint64(pt: Point64, scale: float) : Point64 =
        let mutable result = Point64()
        result.X <- int64 (Math.Round(float pt.X * scale, MidpointRounding.AwayFromZero))
        result.Y <- int64 (Math.Round(float pt.Y * scale, MidpointRounding.AwayFromZero))
#if USINGZ
        result.Z <- pt.Z
#endif
        result

    static member inline ScalePointD(pt: Point64, scale: float) : PointD =
        let mutable result = PointD()
        result.x <- float pt.X * scale
        result.y <- float pt.Y * scale
#if USINGZ
        result.z <- pt.Z
#endif
        result

    static member inline ScaleRect(r: RectD, scale: float) : Rect64 =
        let mutable result = Rect64()
        result.left <- int64 (r.left * scale)
        result.top <- int64 (r.top * scale)
        result.right <- int64 (r.right * scale)
        result.bottom <- int64 (r.bottom * scale)
        result

    static member ScalePath(path: Path64, scale: float) : Path64 =
        if InternalClipper.IsAlmostZero(scale - 1.0) then path
        else
            let result = Path64(path.Count)
#if USINGZ
            for pt in path do
                result.Add(Point64(float pt.X * scale, float pt.Y * scale, float pt.Z))
#else
            for pt in path do
                result.Add(Point64(float pt.X * scale, float pt.Y * scale))
#endif
            result

    static member ScalePaths(paths: Paths64, scale: float) : Paths64 =
        if InternalClipper.IsAlmostZero(scale - 1.0) then paths
        else
            let result = Paths64(paths.Count)
            for path in paths do
                result.Add(Clipper.ScalePath(path, scale))
            result

    static member ScalePath(path: PathD, scale: float) : PathD =
        if InternalClipper.IsAlmostZero(scale - 1.0) then path
        else
            let result = PathD(path.Count)
            for pt in path do
                result.Add(PointD(pt, scale))
            result

    static member ScalePaths(paths: PathsD, scale: float) : PathsD =
        if InternalClipper.IsAlmostZero(scale - 1.0) then paths
        else
            let result = PathsD(paths.Count)
            for path in paths do
                result.Add(Clipper.ScalePath(path, scale))
            result

    static member ScalePath64(path: PathD, scale: float) : Path64 =
        let result = Path64(path.Count)
        for pt in path do
            result.Add(Point64(pt, scale))
        result

    static member ScalePaths64(paths: PathsD, scale: float) : Paths64 =
        let result = Paths64(paths.Count)
        for path in paths do
            result.Add(Clipper.ScalePath64(path, scale))
        result

    static member ScalePathD(path: Path64, scale: float) : PathD =
        let result = PathD(path.Count)
        for pt in path do
            result.Add(PointD(pt, scale))
        result

    static member ScalePathsD(paths: Paths64, scale: float) : PathsD =
        let result = PathsD(paths.Count)
        for path in paths do
            result.Add(Clipper.ScalePathD(path, scale))
        result

    // Type conversion (no scaling)
    static member Path64(path: PathD) : Path64 =
        let result = Path64(path.Count)
        for pt in path do
            result.Add(Point64(pt))
        result

    static member Paths64(paths: PathsD) : Paths64 =
        let result = Paths64(paths.Count)
        for path in paths do
            let p = Path64(path.Count)
            for pt in path do
                p.Add(Point64(pt))
            result.Add(p)
        result

    static member PathsD(paths: Paths64) : PathsD =
        let result = PathsD(paths.Count)
        for path in paths do
            let p = PathD(path.Count)
            for pt in path do
                p.Add(PointD(pt))
            result.Add(p)
        result

    static member PathD(path: Path64) : PathD =
        let result = PathD(path.Count)
        for pt in path do
            result.Add(PointD(pt))
        result

    // Translate
    static member TranslatePath(path: Path64, dx: int64, dy: int64) : Path64 =
        let result = Path64(path.Count)
        for pt in path do
            result.Add(Point64(pt.X + dx, pt.Y + dy))
        result

    static member TranslatePaths(paths: Paths64, dx: int64, dy: int64) : Paths64 =
        let result = Paths64(paths.Count)
        for path in paths do
            result.Add(Clipper.OffsetPath(path, dx, dy))
        result

    static member TranslatePath(path: PathD, dx: float, dy: float) : PathD =
        let result = PathD(path.Count)
        for pt in path do
            result.Add(PointD(pt.x + dx, pt.y + dy))
        result

    static member TranslatePaths(paths: PathsD, dx: float, dy: float) : PathsD =
        let result = PathsD(paths.Count)
        for path in paths do
            result.Add(Clipper.TranslatePath(path, dx, dy))
        result

    // Reverse
    static member ReversePath(path: Path64) : Path64 =
        let result = Path64(path)
        result.Reverse()
        result

    static member ReversePath(path: PathD) : PathD =
        let result = PathD(path)
        result.Reverse()
        result

    static member ReversePaths(paths: Paths64) : Paths64 =
        let result = Paths64(paths.Count)
        for path in paths do
            result.Add(Clipper.ReversePath(path))
        result

    static member ReversePaths(paths: PathsD) : PathsD =
        let result = PathsD(paths.Count)
        for path in paths do
            result.Add(Clipper.ReversePath(path))
        result

    // GetBounds
    static member GetBounds(path: Path64) : Rect64 =
        let mutable result = invalidRect64
        for pt in path do
            if pt.X < result.left then result.left <- pt.X
            if pt.X > result.right then result.right <- pt.X
            if pt.Y < result.top then result.top <- pt.Y
            if pt.Y > result.bottom then result.bottom <- pt.Y
        if result.left = Int64.MaxValue then Rect64() else result

    static member GetBounds(paths: Paths64) : Rect64 =
        let mutable result = invalidRect64
        for path in paths do
            for pt in path do
                if pt.X < result.left then result.left <- pt.X
                if pt.X > result.right then result.right <- pt.X
                if pt.Y < result.top then result.top <- pt.Y
                if pt.Y > result.bottom then result.bottom <- pt.Y
        if result.left = Int64.MaxValue then Rect64() else result

    static member GetBounds(path: PathD) : RectD =
        let mutable result = invalidRectD
        for pt in path do
            if pt.x < result.left then result.left <- pt.x
            if pt.x > result.right then result.right <- pt.x
            if pt.y < result.top then result.top <- pt.y
            if pt.y > result.bottom then result.bottom <- pt.y
        if Math.Abs(result.left - Double.MaxValue) < InternalClipper.floatingPointTolerance then RectD() else result

    static member GetBounds(paths: PathsD) : RectD =
        let mutable result = invalidRectD
        for path in paths do
            for pt in path do
                if pt.x < result.left then result.left <- pt.x
                if pt.x > result.right then result.right <- pt.x
                if pt.y < result.top then result.top <- pt.y
                if pt.y > result.bottom then result.bottom <- pt.y
        if Math.Abs(result.left - Double.MaxValue) < InternalClipper.floatingPointTolerance then RectD() else result

    // MakePath
    static member MakePath(arr: int[]) : Path64 =
        let len = arr.Length / 2
        let p = Path64(len)
        for i = 0 to len - 1 do
            p.Add(Point64(int64 arr.[i * 2], int64 arr.[i * 2 + 1]))
        p

    static member MakePath(arr: int64[]) : Path64 =
        let len = arr.Length / 2
        let p = Path64(len)
        for i = 0 to len - 1 do
            p.Add(Point64(arr.[i * 2], arr.[i * 2 + 1]))
        p

    static member MakePath(arr: float[]) : PathD =
        let len = arr.Length / 2
        let p = PathD(len)
        for i = 0 to len - 1 do
            p.Add(PointD(arr.[i * 2], arr.[i * 2 + 1]))
        p

#if USINGZ
    static member MakePathZ(arr: int64[]) : Path64 =
        let len = arr.Length / 3
        let p = Path64(len)
        for i = 0 to len - 1 do
            p.Add(Point64(arr.[i * 3], arr.[i * 3 + 1], arr.[i * 3 + 2]))
        p

    static member MakePathZ(arr: float[]) : PathD =
        let len = arr.Length / 3
        let p = PathD(len)
        for i = 0 to len - 1 do
            p.Add(PointD(arr.[i * 3], arr.[i * 3 + 1], int64 arr.[i * 3 + 2]))
        p
#endif

    // Math helpers
    static member inline Sqr(v: float) : float = v * v

    static member inline Sqr(v: int64) : float = float v * float v

    static member inline DistanceSqr(pt1: Point64, pt2: Point64) : float =
        Clipper.Sqr(pt1.X - pt2.X) + Clipper.Sqr(pt1.Y - pt2.Y)

    static member inline MidPoint(pt1: Point64, pt2: Point64) : Point64 =
        Point64((pt1.X + pt2.X) / 2L, (pt1.Y + pt2.Y) / 2L)

    static member inline MidPoint(pt1: PointD, pt2: PointD) : PointD =
        PointD((pt1.x + pt2.x) / 2.0, (pt1.y + pt2.y) / 2.0)

    static member inline InflateRect(r: byref<Rect64>, dx: int, dy: int) : unit =
        r.left <- r.left - int64 dx
        r.right <- r.right + int64 dx
        r.top <- r.top - int64 dy
        r.bottom <- r.bottom + int64 dy

    static member inline InflateRect(r: byref<RectD>, dx: float, dy: float) : unit =
        r.left <- r.left - dx
        r.right <- r.right + dx
        r.top <- r.top - dy
        r.bottom <- r.bottom + dy

    static member inline PointsNearEqual(pt1: PointD, pt2: PointD, distanceSqrd: float) : bool =
        Clipper.Sqr(pt1.x - pt2.x) + Clipper.Sqr(pt1.y - pt2.y) < distanceSqrd

    static member StripNearDuplicates(path: PathD, minEdgeLenSqrd: float, isClosedPath: bool) : PathD =
        let cnt = path.Count
        let result = PathD(cnt)
        if cnt = 0 then result
        else
            let mutable lastPt = path.[0]
            result.Add(lastPt)
            for i = 1 to cnt - 1 do
                if not (Clipper.PointsNearEqual(lastPt, path.[i], minEdgeLenSqrd)) then
                    lastPt <- path.[i]
                    result.Add(lastPt)
            if isClosedPath && Clipper.PointsNearEqual(lastPt, result.[0], minEdgeLenSqrd) then
                result.RemoveAt(result.Count - 1)
            result

    static member StripDuplicates(path: Path64, isClosedPath: bool) : Path64 =
        let cnt = path.Count
        let result = Path64(cnt)
        if cnt = 0 then result
        else
            let mutable lastPt = path.[0]
            result.Add(lastPt)
            for i = 1 to cnt - 1 do
                if lastPt <> path.[i] then
                    lastPt <- path.[i]
                    result.Add(lastPt)
            if isClosedPath && lastPt = result.[0] then
                result.RemoveAt(result.Count - 1)
            result

    // PolyTree helpers
    static member PolyTreeToPaths64(polyTree: PolyTree64) : Paths64 =
        let rec addPolyNodeToPaths (polyPath: PolyPath64) (paths: Paths64) : unit =
            if polyPath.Polygon.Count > 0 then
                paths.Add(polyPath.Polygon)
            for i = 0 to polyPath.Count - 1 do
                addPolyNodeToPaths (polyPath.[i]) paths

        let result = Paths64()
        for i = 0 to polyTree.Count - 1 do
            addPolyNodeToPaths (polyTree.[i]) result
        result

    static member PolyTreeToPathsD(polyTree: PolyTreeD) : PathsD =
        let rec addPolyNodeToPathsD (polyPath: PolyPathD) (paths: PathsD) : unit =
            if polyPath.Polygon.Count > 0 then
                paths.Add(polyPath.Polygon)
            for i = 0 to polyPath.Count - 1 do
                addPolyNodeToPathsD (polyPath.[i]) paths

        let result = PathsD()
        for polyPathBase in polyTree do
            let p = polyPathBase :?> PolyPathD
            addPolyNodeToPathsD p result
        result

    // Perpendic dist
    static member inline PerpendicDistFromLineSqrd(pt: PointD, line1: PointD, line2: PointD) : float =
        let a = pt.x - line1.x
        let b = pt.y - line1.y
        let c = line2.x - line1.x
        let d = line2.y - line1.y
        if c = 0.0 && d = 0.0 then 0.0
        else Clipper.Sqr(a * d - c * b) / (c * c + d * d)

    static member inline PerpendicDistFromLineSqrd(pt: Point64, line1: Point64, line2: Point64) : float =
        let a = float pt.X - float line1.X
        let b = float pt.Y - float line1.Y
        let c = float line2.X - float line1.X
        let d = float line2.Y - float line1.Y
        if c = 0.0 && d = 0.0 then 0.0
        else Clipper.Sqr(a * d - c * b) / (c * c + d * d)

    static member private GetNext(current: int, high: int, flags: bool[]) : int =
        let mutable current = current + 1
        while current <= high && flags.[current] do
            current <- current + 1
        if current <= high then current
        else
            let mutable current = 0
            while flags.[current] do
                current <- current + 1
            current

    static member private GetPrior(current: int, high: int, flags: bool[]) : int =
        let mutable current = if current = 0 then high else current - 1
        while current > 0 && flags.[current] do
            current <- current - 1
        if not flags.[current] then current
        else
            let mutable current = high
            while flags.[current] do
                current <- current - 1
            current

    // RDP simplification
    static member internal RDP(path: Path64, begin_: int, end_: int, epsSqrd: float, flags: List<bool>) : unit =
        let mutable begin_ = begin_
        let mutable end_ = end_
        let mutable continue_ = true
        while continue_ do
            let mutable idx = 0
            let mutable max_d = 0.0
            while end_ > begin_ && path.[begin_] = path.[end_] do
                flags.[end_] <- false
                end_ <- end_ - 1
            for i = begin_ + 1 to end_ - 1 do
                let d = Clipper.PerpendicDistFromLineSqrd(path.[i], path.[begin_], path.[end_])
                if d > max_d then
                    max_d <- d
                    idx <- i
            if max_d <= epsSqrd then
                continue_ <- false
            else
                flags.[idx] <- true
                if idx > begin_ + 1 then
                    Clipper.RDP(path, begin_, idx, epsSqrd, flags)
                if idx < end_ - 1 then
                    begin_ <- idx
                else
                    continue_ <- false

    static member RamerDouglasPeucker(path: Path64, epsilon: float) : Path64 =
        let len = path.Count
        if len < 5 then path
        else
            let flags = List<bool>(Array.zeroCreate len)
            flags.[0] <- true
            flags.[len - 1] <- true
            Clipper.RDP(path, 0, len - 1, Clipper.Sqr(epsilon), flags)
            let result = Path64(len)
            for i = 0 to len - 1 do
                if flags.[i] then result.Add(path.[i])
            result

    static member RamerDouglasPeucker(paths: Paths64, epsilon: float) : Paths64 =
        let result = Paths64(paths.Count)
        for path in paths do
            result.Add(Clipper.RamerDouglasPeucker(path, epsilon))
        result

    static member internal RDP(path: PathD, begin_: int, end_: int, epsSqrd: float, flags: List<bool>) : unit =
        let mutable begin_ = begin_
        let mutable end_ = end_
        let mutable continue_ = true
        while continue_ do
            let mutable idx = 0
            let mutable max_d = 0.0
            while end_ > begin_ && path.[begin_] = path.[end_] do
                flags.[end_] <- false
                end_ <- end_ - 1
            for i = begin_ + 1 to end_ - 1 do
                let d = Clipper.PerpendicDistFromLineSqrd(path.[i], path.[begin_], path.[end_])
                if d > max_d then
                    max_d <- d
                    idx <- i
            if max_d <= epsSqrd then
                continue_ <- false
            else
                flags.[idx] <- true
                if idx > begin_ + 1 then
                    Clipper.RDP(path, begin_, idx, epsSqrd, flags)
                if idx < end_ - 1 then
                    begin_ <- idx
                else
                    continue_ <- false

    static member RamerDouglasPeucker(path: PathD, epsilon: float) : PathD =
        let len = path.Count
        if len < 5 then path
        else
            let flags = List<bool>(Array.zeroCreate len)
            flags.[0] <- true
            flags.[len - 1] <- true
            Clipper.RDP(path, 0, len - 1, Clipper.Sqr(epsilon), flags)
            let result = PathD(len)
            for i = 0 to len - 1 do
                if flags.[i] then result.Add(path.[i])
            result

    static member RamerDouglasPeucker(paths: PathsD, epsilon: float) : PathsD =
        let result = PathsD(paths.Count)
        for path in paths do
            result.Add(Clipper.RamerDouglasPeucker(path, epsilon))
        result

    // Simplify
    static member SimplifyPath(path: Path64, epsilon: float,
                               [<Optional; DefaultParameterValue(true)>] isClosedPath: bool) : Path64 =
        let len = path.Count
        let high = len - 1
        let epsSqr = Clipper.Sqr(epsilon)
        if len < 4 then path
        else
            let flags = Array.zeroCreate<bool> len
            let dsq = Array.zeroCreate<float> len
            let mutable curr = 0

            if isClosedPath then
                dsq.[0] <- Clipper.PerpendicDistFromLineSqrd(path.[0], path.[high], path.[1])
                dsq.[high] <- Clipper.PerpendicDistFromLineSqrd(path.[high], path.[0], path.[high - 1])
            else
                dsq.[0] <- Double.MaxValue
                dsq.[high] <- Double.MaxValue

            for i = 1 to high - 1 do
                dsq.[i] <- Clipper.PerpendicDistFromLineSqrd(path.[i], path.[i - 1], path.[i + 1])

            let mutable finished = false
            while not finished do
                if dsq.[curr] > epsSqr then
                    let start = curr
                    let mutable loop_ = true
                    while loop_ do
                        curr <- Clipper.GetNext(curr, high, flags)
                        if curr = start || dsq.[curr] <= epsSqr then
                            loop_ <- false
                    if curr = start then
                        finished <- true

                if not finished then
                    let mutable prev = Clipper.GetPrior(curr, high, flags)
                    let mutable next = Clipper.GetNext(curr, high, flags)
                    if next = prev then
                        finished <- true
                    else
                        let prior2 =
                            if dsq.[next] < dsq.[curr] then
                                let prior2 = prev
                                prev <- curr
                                curr <- next
                                next <- Clipper.GetNext(next, high, flags)
                                prior2
                            else
                                Clipper.GetPrior(prev, high, flags)

                        flags.[curr] <- true
                        curr <- next
                        next <- Clipper.GetNext(next, high, flags)
                        if isClosedPath || ((curr <> high) && (curr <> 0)) then
                            dsq.[curr] <- Clipper.PerpendicDistFromLineSqrd(path.[curr], path.[prev], path.[next])
                        if isClosedPath || ((prev <> 0) && (prev <> high)) then
                            dsq.[prev] <- Clipper.PerpendicDistFromLineSqrd(path.[prev], path.[prior2], path.[curr])

            let result = Path64(len)
            for i = 0 to len - 1 do
                if not flags.[i] then result.Add(path.[i])
            result
    static member SimplifyPaths(paths: Paths64, epsilon: float,
                                [<Optional; DefaultParameterValue(true)>] isClosedPaths: bool) : Paths64 =
        let result = Paths64(paths.Count)
        for path in paths do
            result.Add(Clipper.SimplifyPath(path, epsilon, isClosedPaths))
        result
    static member SimplifyPath(path: PathD, epsilon: float,
                               [<Optional; DefaultParameterValue(true)>] isClosedPath: bool) : PathD =
        let len = path.Count
        let high = len - 1
        let epsSqr = Clipper.Sqr(epsilon)
        if len < 4 then path
        else
            let flags = Array.zeroCreate<bool> len
            let dsq = Array.zeroCreate<float> len
            let mutable curr = 0

            if isClosedPath then
                dsq.[0] <- Clipper.PerpendicDistFromLineSqrd(path.[0], path.[high], path.[1])
                dsq.[high] <- Clipper.PerpendicDistFromLineSqrd(path.[high], path.[0], path.[high - 1])
            else
                dsq.[0] <- Double.MaxValue
                dsq.[high] <- Double.MaxValue

            for i = 1 to high - 1 do
                dsq.[i] <- Clipper.PerpendicDistFromLineSqrd(path.[i], path.[i - 1], path.[i + 1])

            let mutable finished = false
            while not finished do
                if dsq.[curr] > epsSqr then
                    let start = curr
                    let mutable loop_ = true
                    while loop_ do
                        curr <- Clipper.GetNext(curr, high, flags)
                        if curr = start || dsq.[curr] <= epsSqr then
                            loop_ <- false
                    if curr = start then
                        finished <- true

                if not finished then
                    let mutable prev = Clipper.GetPrior(curr, high, flags)
                    let mutable next = Clipper.GetNext(curr, high, flags)
                    if next = prev then
                        finished <- true
                    else
                        let prior2 =
                            if dsq.[next] < dsq.[curr] then
                                let prior2 = prev
                                prev <- curr
                                curr <- next
                                next <- Clipper.GetNext(next, high, flags)
                                prior2
                            else
                                Clipper.GetPrior(prev, high, flags)

                        flags.[curr] <- true
                        curr <- next
                        next <- Clipper.GetNext(next, high, flags)
                        if isClosedPath || ((curr <> high) && (curr <> 0)) then
                            dsq.[curr] <- Clipper.PerpendicDistFromLineSqrd(path.[curr], path.[prev], path.[next])
                        if isClosedPath || ((prev <> 0) && (prev <> high)) then
                            dsq.[prev] <- Clipper.PerpendicDistFromLineSqrd(path.[prev], path.[prior2], path.[curr])

            let result = PathD(len)
            for i = 0 to len - 1 do
                if not flags.[i] then result.Add(path.[i])
            result
    static member SimplifyPaths(paths: PathsD, epsilon: float,
                                [<Optional; DefaultParameterValue(true)>] isClosedPath: bool) : PathsD =
        let result = PathsD(paths.Count)
        for path in paths do
            result.Add(Clipper.SimplifyPath(path, epsilon, isClosedPath))
        result

    // TrimCollinear
    static member TrimCollinear(path: Path64,
                                [<Optional; DefaultParameterValue(false)>] isOpen: bool) : Path64 =
        let mutable len = path.Count
        let mutable i = 0
        if not isOpen then
            while i < len - 1 && InternalClipper.IsCollinear(path.[len - 1], path.[i], path.[i + 1]) do
                i <- i + 1
            while i < len - 1 && InternalClipper.IsCollinear(path.[len - 2], path.[len - 1], path.[i]) do
                len <- len - 1

        if len - i < 3 then
            if (not isOpen) || len < 2 || path.[0] = path.[1] then Path64()
            else path
        else
            let result = Path64(len - i)
            let mutable last = path.[i]
            result.Add(last)
            i <- i + 1
            while i < len - 1 do
                if not (InternalClipper.IsCollinear(last, path.[i], path.[i + 1])) then
                    last <- path.[i]
                    result.Add(last)
                i <- i + 1

            if isOpen then
                result.Add(path.[len - 1])
            elif not (InternalClipper.IsCollinear(last, path.[len - 1], result.[0])) then
                result.Add(path.[len - 1])
            else
                while result.Count > 2 && InternalClipper.IsCollinear(result.[result.Count - 1], result.[result.Count - 2], result.[0]) do
                    result.RemoveAt(result.Count - 1)
                if result.Count < 3 then
                    result.Clear()
            result
    static member TrimCollinear(path: PathD, precision: int,
                                [<Optional; DefaultParameterValue(false)>] isOpen: bool) : PathD =
        InternalClipper.CheckPrecision(precision)
        let scale = Math.Pow(10.0, float precision)
        let p = ClipperPrimitives.ScalePath64(path, scale)
        let p = Clipper.TrimCollinear(p, isOpen)
        ClipperPrimitives.ScalePathD(p, 1.0 / scale)

    // PointInPolygon
    static member PointInPolygon(pt: Point64, polygon: Path64) : PointInPolygonResult =
        InternalClipper.PointInPolygon(pt, polygon)
    static member PointInPolygon(pt: PointD, polygon: PathD,
                                 [<Optional; DefaultParameterValue(2)>] precision: int) : PointInPolygonResult =
        InternalClipper.CheckPrecision(precision)
        let scale = Math.Pow(10.0, float precision)
        let p = Point64(pt, scale)
        let path = ClipperPrimitives.ScalePath64(polygon, scale)
        InternalClipper.PointInPolygon(p, path)

    // Ellipse
    static member Ellipse(center: Point64, radiusX: float,
                          [<Optional; DefaultParameterValue(0.0)>] radiusY: float,
                          [<Optional; DefaultParameterValue(0)>] steps: int) : Path64 =
        if radiusX <= 0.0 then Path64()
        else
            let mutable radiusY = radiusY
            let mutable steps = steps
            if radiusY <= 0.0 then radiusY <- radiusX
            if steps <= 2 then
                steps <- int (Math.Ceiling(Math.PI * Math.Sqrt((radiusX + radiusY) / 2.0)))
            let si = Math.Sin(2.0 * Math.PI / float steps)
            let co = Math.Cos(2.0 * Math.PI / float steps)
            let mutable dx = co
            let mutable dy = si
            let result = Path64(steps)
            result.Add(Point64(float center.X + radiusX, float center.Y))
            for _ = 1 to steps - 1 do
                result.Add(Point64(float center.X + radiusX * dx, float center.Y + radiusY * dy))
                let x = dx * co - dy * si
                dy <- dy * co + dx * si
                dx <- x
            result
    static member Ellipse(center: PointD, radiusX: float,
                          [<Optional; DefaultParameterValue(0.0)>] radiusY: float,
                          [<Optional; DefaultParameterValue(0)>] steps: int) : PathD =
        if radiusX <= 0.0 then PathD()
        else
            let mutable radiusY = radiusY
            let mutable steps = steps
            if radiusY <= 0.0 then radiusY <- radiusX
            if steps <= 2 then
                steps <- int (Math.Ceiling(Math.PI * Math.Sqrt((radiusX + radiusY) / 2.0)))
            let si = Math.Sin(2.0 * Math.PI / float steps)
            let co = Math.Cos(2.0 * Math.PI / float steps)
            let mutable dx = co
            let mutable dy = si
            let result = PathD(steps)
            result.Add(PointD(center.x + radiusX, center.y))
            for _ = 1 to steps - 1 do
                result.Add(PointD(center.x + radiusX * dx, center.y + radiusY * dy))
                let x = dx * co - dy * si
                dy <- dy * co + dx * si
                dx <- x
            result

    // ShowPolyTreeStructure
    static member ShowPolyTreeStructure(polytree: PolyTree64) : unit =
        let rec showPolyPathStructure (pp: PolyPath64) (level: int) =
            let spaces = String(' ', level * 2)
            let caption = if pp.IsHole then "Hole " else "Outer "
            if pp.Count = 0 then
                Console.WriteLine(spaces + caption)
            else
                Console.WriteLine(spaces + caption + $"({pp.Count})")
                for i = 0 to pp.Count - 1 do
                    showPolyPathStructure (pp.[i]) (level + 1)

        Console.WriteLine("Polytree Root")
        for i = 0 to polytree.Count - 1 do
            showPolyPathStructure (polytree.[i]) 1

    static member ShowPolyTreeStructure(polytree: PolyTreeD) : unit =
        let rec showPolyPathStructure (pp: PolyPathD) (level: int) =
            let spaces = String(' ', level * 2)
            let caption = if pp.IsHole then "Hole " else "Outer "
            if pp.Count = 0 then
                Console.WriteLine(spaces + caption)
            else
                Console.WriteLine(spaces + caption + $"({pp.Count})")
                for i = 0 to pp.Count - 1 do
                    showPolyPathStructure (pp.[i]) (level + 1)

        Console.WriteLine("Polytree Root")
        for i = 0 to polytree.Count - 1 do
            showPolyPathStructure (polytree.[i]) 1

    // Triangulate
    static member Triangulate(pp: Paths64, solution: byref<Paths64>,
                              [<Optional; DefaultParameterValue(true)>] useDelaunay: bool) : TriangulateResult =
        let d = Delaunay(useDelaunay)
        let mutable sol = Paths64()
        let result = d.Execute(pp, &sol)
        solution <- sol
        result
    static member Triangulate(pp: PathsD, decPlaces: int, solution: byref<PathsD>,
                              [<Optional; DefaultParameterValue(true)>] useDelaunay: bool) : TriangulateResult =
        let scale =
            if decPlaces <= 0 then 1.0
            elif decPlaces > 8 then Math.Pow(10.0, 8.0)
            else Math.Pow(10.0, float decPlaces)

        let pp64 = ClipperPrimitives.ScalePaths64(pp, scale)
        let d = Delaunay(useDelaunay)
        let mutable sol64 = Paths64()
        let result = d.Execute(pp64, &sol64)
        if result = TriangulateResult.Success then
            solution <- ClipperPrimitives.ScalePathsD(sol64, 1.0 / scale)
        else
            solution <- PathsD()
        result

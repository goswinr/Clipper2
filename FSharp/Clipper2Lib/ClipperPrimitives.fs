#if USINGZ
namespace Clipper2ZLib
#else
namespace Clipper2Lib
#endif

open System
open System.Runtime.InteropServices

/// Helper functions extracted from the C# Clipper static class that are needed
/// by Engine, Offset, RectClip, and Minkowski before the facade Clipper.fs is compiled.
[<AbstractClass; Sealed>]
type ClipperPrimitives private () =

    static let mutable _invalidRect64 = Rect64(false)
    static let mutable _invalidRectD = RectD(false)

    static member InvalidRect64 = _invalidRect64
    static member InvalidRectD = _invalidRectD

    static member Area(path: Path64) : float =
        // https://en.wikipedia.org/wiki/Shoelace_formula
        let mutable a = 0.0
        let cnt = path.Count
        if cnt < 3 then 0.0
        else
            let mutable prevPt = path.[cnt - 1]
            for i = 0 to cnt - 1 do
                let pt = path.[i]
                a <- a + float (prevPt.Y + pt.Y) * float (prevPt.X - pt.X)
                prevPt <- pt
            a * 0.5

    static member Area(paths: Paths64) : float =
        let mutable a = 0.0
        for i = 0 to paths.Count - 1 do
            a <- a + ClipperPrimitives.Area(paths.[i])
        a

    static member Area(path: PathD) : float =
        let mutable a = 0.0
        let cnt = path.Count
        if cnt < 3 then 0.0
        else
            let mutable prevPt = path.[cnt - 1]
            for i = 0 to cnt - 1 do
                let pt = path.[i]
                a <- a + (prevPt.y + pt.y) * (prevPt.x - pt.x)
                prevPt <- pt
            a * 0.5

    static member Area(paths: PathsD) : float =
        let mutable a = 0.0
        for i = 0 to paths.Count - 1 do
            a <- a + ClipperPrimitives.Area(paths.[i])
        a

    static member inline IsPositive(poly: Path64) : bool =
        ClipperPrimitives.Area(poly) >= 0.0

    static member inline IsPositive(poly: PathD) : bool =
        ClipperPrimitives.Area(poly) >= 0.0

    static member inline Sqr(v: float) : float = v * v

    static member inline Sqr(v: int64) : float = float v * float v

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

    static member ScalePath64(path: PathD, scale: float) : Path64 =
        let cnt = path.Count
        let res = Path64(cnt)
        for i = 0 to cnt - 1 do
            res.Add(Point64(path.[i], scale))
        res

    static member ScalePaths64(paths: PathsD, scale: float) : Paths64 =
        let cnt = paths.Count
        let res = Paths64(cnt)
        for i = 0 to cnt - 1 do
            res.Add(ClipperPrimitives.ScalePath64(paths.[i], scale))
        res

    static member ScalePathD(path: Path64, scale: float) : PathD =
        let cnt = path.Count
        let res = PathD(cnt)
        for i = 0 to cnt - 1 do
            res.Add(PointD(path.[i], scale))
        res

    static member ScalePathsD(paths: Paths64, scale: float) : PathsD =
        let cnt = paths.Count
        let res = PathsD(cnt)
        for i = 0 to cnt - 1 do
            res.Add(ClipperPrimitives.ScalePathD(paths.[i], scale))
        res

    static member inline ScaleRect(r: RectD, scale: float) : Rect64 =
        let mutable result = Rect64()
        result.left <- int64 (r.left * scale)
        result.top <- int64 (r.top * scale)
        result.right <- int64 (r.right * scale)
        result.bottom <- int64 (r.bottom * scale)
        result

    static member inline PerpendicDistFromLineSqrd(pt: PointD, line1: PointD, line2: PointD) : float =
        let a = pt.x - line1.x
        let b = pt.y - line1.y
        let c = line2.x - line1.x
        let d = line2.y - line1.y
        if c = 0.0 && d = 0.0 then 0.0
        else ClipperPrimitives.Sqr(a * d - c * b) / (c * c + d * d)

    static member inline PerpendicDistFromLineSqrd(pt: Point64, line1: Point64, line2: Point64) : float =
        let a = float pt.X - float line1.X
        let b = float pt.Y - float line1.Y
        let c = float line2.X - float line1.X
        let d = float line2.Y - float line1.Y
        if c = 0.0 && d = 0.0 then 0.0
        else ClipperPrimitives.Sqr(a * d - c * b) / (c * c + d * d)

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

    static member ReversePath(path: Path64) : Path64 =
        let result = Path64.FromEnumerable(path)
        result.Reverse()
        result

    static member ReversePath(path: PathD) : PathD =
        let result = PathD.FromEnumerable(path)
        result.Reverse()
        result

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
            for i = 1 to steps - 1 do
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
            for i = 1 to steps - 1 do
                result.Add(PointD(center.x + radiusX * dx, center.y + radiusY * dy))
                let x = dx * co - dy * si
                dy <- dy * co + dx * si
                dx <- x
            result

    static member GetBounds(path: Path64) : Rect64 =
        let mutable result = _invalidRect64
        for i = 0 to path.Count - 1 do
            let pt = path.[i]
            if pt.X < result.left then result.left <- pt.X
            if pt.X > result.right then result.right <- pt.X
            if pt.Y < result.top then result.top <- pt.Y
            if pt.Y > result.bottom then result.bottom <- pt.Y
        if result.left = Int64.MaxValue then Rect64() else result

    static member GetBounds(paths: Paths64) : Rect64 =
        let mutable result = _invalidRect64
        for i = 0 to paths.Count - 1 do
            let path = paths.[i]
            for j = 0 to path.Count - 1 do
                let pt = path.[j]
                if pt.X < result.left then result.left <- pt.X
                if pt.X > result.right then result.right <- pt.X
                if pt.Y < result.top then result.top <- pt.Y
                if pt.Y > result.bottom then result.bottom <- pt.Y
        if result.left = Int64.MaxValue then Rect64() else result

    static member GetBounds(path: PathD) : RectD =
        let mutable result = _invalidRectD
        for i = 0 to path.Count - 1 do
            let pt = path.[i]
            if pt.x < result.left then result.left <- pt.x
            if pt.x > result.right then result.right <- pt.x
            if pt.y < result.top then result.top <- pt.y
            if pt.y > result.bottom then result.bottom <- pt.y
        if Math.Abs(result.left - Double.MaxValue) < InternalClipper.floatingPointTolerance then RectD() else result

    static member GetBounds(paths: PathsD) : RectD =
        let mutable result = _invalidRectD
        for i = 0 to paths.Count - 1 do
            let path = paths.[i]
            for j = 0 to path.Count - 1 do
                let pt = path.[j]
                if pt.x < result.left then result.left <- pt.x
                if pt.x > result.right then result.right <- pt.x
                if pt.y < result.top then result.top <- pt.y
                if pt.y > result.bottom then result.bottom <- pt.y
        if Math.Abs(result.left - Double.MaxValue) < InternalClipper.floatingPointTolerance then RectD() else result

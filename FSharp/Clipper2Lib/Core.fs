#if USINGZ
namespace Clipper2ZLib
#else
namespace Clipper2Lib
#endif

open System
open System.Collections.Generic

// Enums (no dependencies)
type ClipType =
    | NoClip = 0
    | Intersection = 1
    | Union = 2
    | Difference = 3
    | Xor = 4

type PathType =
    | Subject = 0
    | Clip = 1

type FillRule =
    | EvenOdd = 0
    | NonZero = 1
    | Positive = 2
    | Negative = 3

// Forward helper for PointD == / != (needs IsAlmostZero before InternalClipper is declared)
[<AutoOpen>]
module internal IsAlmostZeroHelper =
    let floatingPointTolerance = 1E-12
    let inline isAlmostZero (value: float) = Math.Abs(value) <= floatingPointTolerance

// Point64 and PointD - mutual group
[<Struct; CustomEquality; NoComparison>]
type Point64 =
    val mutable X: int64
    val mutable Y: int64
#if USINGZ
    val mutable Z: int64
#endif

#if USINGZ
    new(x: int64, y: int64, z: int64) = { X = x; Y = y; Z = z }
    new(x: int64, y: int64) = { X = x; Y = y; Z = 0L }
#else
    new(x: int64, y: int64) = { X = x; Y = y }
#endif

#if USINGZ
    new(x: float, y: float, z: float) =
        { X = int64 (Math.Round(x, MidpointRounding.AwayFromZero))
          Y = int64 (Math.Round(y, MidpointRounding.AwayFromZero))
          Z = int64 (Math.Round(z, MidpointRounding.AwayFromZero)) }
    new(x: float, y: float) =
        { X = int64 (Math.Round(x, MidpointRounding.AwayFromZero))
          Y = int64 (Math.Round(y, MidpointRounding.AwayFromZero))
          Z = 0L }
#else
    new(x: float, y: float) =
        { X = int64 (Math.Round(x, MidpointRounding.AwayFromZero))
          Y = int64 (Math.Round(y, MidpointRounding.AwayFromZero)) }
#endif

#if USINGZ
    new(pt: Point64) = { X = pt.X; Y = pt.Y; Z = pt.Z }
#else
    new(pt: Point64) = { X = pt.X; Y = pt.Y }
#endif

#if USINGZ
    new(pt: Point64, scale: float) =
        { X = int64 (Math.Round(float pt.X * scale, MidpointRounding.AwayFromZero))
          Y = int64 (Math.Round(float pt.Y * scale, MidpointRounding.AwayFromZero))
          Z = int64 (Math.Round(float pt.Z * scale, MidpointRounding.AwayFromZero)) }
#else
    new(pt: Point64, scale: float) =
        { X = int64 (Math.Round(float pt.X * scale, MidpointRounding.AwayFromZero))
          Y = int64 (Math.Round(float pt.Y * scale, MidpointRounding.AwayFromZero)) }
#endif

#if USINGZ
    new(pt: PointD) =
        { X = int64 (Math.Round(pt.x, MidpointRounding.AwayFromZero))
          Y = int64 (Math.Round(pt.y, MidpointRounding.AwayFromZero))
          Z = pt.z }
#else
    new(pt: PointD) =
        { X = int64 (Math.Round(pt.x, MidpointRounding.AwayFromZero))
          Y = int64 (Math.Round(pt.y, MidpointRounding.AwayFromZero)) }
#endif

#if USINGZ
    new(pt: PointD, scale: float) =
        { X = int64 (Math.Round(pt.x * scale, MidpointRounding.AwayFromZero))
          Y = int64 (Math.Round(pt.y * scale, MidpointRounding.AwayFromZero))
          Z = pt.z }
#else
    new(pt: PointD, scale: float) =
        { X = int64 (Math.Round(pt.x * scale, MidpointRounding.AwayFromZero))
          Y = int64 (Math.Round(pt.y * scale, MidpointRounding.AwayFromZero)) }
#endif

    static member op_Equality(lhs: Point64, rhs: Point64) =
        lhs.X = rhs.X && lhs.Y = rhs.Y

    static member op_Inequality(lhs: Point64, rhs: Point64) =
        lhs.X <> rhs.X || lhs.Y <> rhs.Y

#if USINGZ
    static member (+)(lhs: Point64, rhs: Point64) =
        Point64(lhs.X + rhs.X, lhs.Y + rhs.Y, lhs.Z + rhs.Z)

    static member (-)(lhs: Point64, rhs: Point64) =
        Point64(lhs.X - rhs.X, lhs.Y - rhs.Y, lhs.Z - rhs.Z)

    override this.ToString() =
        sprintf "%d,%d,%d " this.X this.Y this.Z
#else
    static member (+)(lhs: Point64, rhs: Point64) =
        Point64(lhs.X + rhs.X, lhs.Y + rhs.Y)

    static member (-)(lhs: Point64, rhs: Point64) =
        Point64(lhs.X - rhs.X, lhs.Y - rhs.Y)

    override this.ToString() =
        sprintf "%d,%d " this.X this.Y
#endif

    override this.Equals(obj: obj) =
        match obj with
        | :? Point64 as p -> this.X = p.X && this.Y = p.Y
        | _ -> false

    override this.GetHashCode() =
        HashCode.Combine(this.X, this.Y)

and [<Struct; CustomEquality; NoComparison>] PointD =
    val mutable x: float
    val mutable y: float
#if USINGZ
    val mutable z: int64
#endif

#if USINGZ
    new(x: float, y: float, z: int64) = { x = x; y = y; z = z }
    new(x: float, y: float) = { x = x; y = y; z = 0L }
#else
    new(x: float, y: float) = { x = x; y = y }
#endif

#if USINGZ
    new(x: int64, y: int64, z: int64) = { x = float x; y = float y; z = z }
    new(x: int64, y: int64) = { x = float x; y = float y; z = 0L }
#else
    new(x: int64, y: int64) = { x = float x; y = float y }
#endif

#if USINGZ
    new(pt: PointD) = { x = pt.x; y = pt.y; z = pt.z }
#else
    new(pt: PointD) = { x = pt.x; y = pt.y }
#endif

#if USINGZ
    new(pt: Point64) = { x = float pt.X; y = float pt.Y; z = pt.Z }
#else
    new(pt: Point64) = { x = float pt.X; y = float pt.Y }
#endif

#if USINGZ
    new(pt: Point64, scale: float) =
        { x = float pt.X * scale; y = float pt.Y * scale; z = pt.Z }
#else
    new(pt: Point64, scale: float) =
        { x = float pt.X * scale; y = float pt.Y * scale }
#endif

#if USINGZ
    new(pt: PointD, scale: float) =
        { x = pt.x * scale; y = pt.y * scale; z = pt.z }
#else
    new(pt: PointD, scale: float) =
        { x = pt.x * scale; y = pt.y * scale }
#endif

#if USINGZ
    member this.ToString(precision: int) : string =
        String.Format(sprintf "{0:F%d},{1:F%d},{2:D}" precision precision, this.x, this.y, this.z)
#else
    member this.ToString(precision: int) : string =
        String.Format(sprintf "{0:F%d},{1:F%d}" precision precision, this.x, this.y)
#endif

    static member op_Equality(lhs: PointD, rhs: PointD) =
        isAlmostZero (lhs.x - rhs.x) && isAlmostZero (lhs.y - rhs.y)

    static member op_Inequality(lhs: PointD, rhs: PointD) =
        not (isAlmostZero (lhs.x - rhs.x)) || not (isAlmostZero (lhs.y - rhs.y))

    override this.Equals(obj: obj) =
        match obj with
        | :? PointD as p -> isAlmostZero (this.x - p.x) && isAlmostZero (this.y - p.y)
        | _ -> false

    member this.Negate() =
        let mutable self = this
        self.x <- -self.x
        self.y <- -self.y

    override this.GetHashCode() =
        HashCode.Combine(this.x, this.y)

// Path / Paths classes
[<AllowNullLiteral>]
type Path64(capacity: int) =
    inherit List<Point64>(capacity)
    new() = Path64(0)
#if !FABLE_COMPILER
    new(path: IEnumerable<Point64>) as this =
        Path64(0)
        then this.AddRange(path)
#endif
    static member FromEnumerable(path: IEnumerable<Point64>) : Path64 =
        let result = Path64()
        result.AddRange(path)
        result
    override this.ToString() = String.Join(", ", this)

[<AllowNullLiteral>]
type Paths64(capacity: int) =
    inherit List<Path64>(capacity)
    new() = Paths64(0)
#if !FABLE_COMPILER
    new(paths: IEnumerable<Path64>) as this =
        Paths64(0)
        then this.AddRange(paths)
#endif
    static member FromEnumerable(paths: IEnumerable<Path64>) : Paths64 =
        let result = Paths64()
        result.AddRange(paths)
        result
    override this.ToString() = String.Join(Environment.NewLine, this)

[<AllowNullLiteral>]
type PathD(capacity: int) =
    inherit List<PointD>(capacity)
    new() = PathD(0)
#if !FABLE_COMPILER
    new(path: IEnumerable<PointD>) as this =
        PathD(0)
        then this.AddRange(path)
#endif
    static member FromEnumerable(path: IEnumerable<PointD>) : PathD =
        let result = PathD()
        result.AddRange(path)
        result
    member this.ToString(precision: int) : string =
        String.Join(", ", this.ConvertAll(fun x -> x.ToString(precision)))

[<AllowNullLiteral>]
type PathsD(capacity: int) =
    inherit List<PathD>(capacity)
    new() = PathsD(0)
#if !FABLE_COMPILER
    new(paths: IEnumerable<PathD>) as this =
        PathsD(0)
        then this.AddRange(paths)
#endif
    static member FromEnumerable(paths: IEnumerable<PathD>) : PathsD =
        let result = PathsD()
        result.AddRange(paths)
        result
    member this.ToString(precision: int) : string =
        String.Join(Environment.NewLine, this.ConvertAll(fun x -> x.ToString(precision)))

// Rect structs
[<Struct>]
type Rect64 =
    val mutable left: int64
    val mutable top: int64
    val mutable right: int64
    val mutable bottom: int64

    new(l: int64, t: int64, r: int64, b: int64) =
        { left = l; top = t; right = r; bottom = b }

    new(isValid: bool) =
        if isValid then { left = 0L; top = 0L; right = 0L; bottom = 0L }
        else { left = Int64.MaxValue; top = Int64.MaxValue; right = Int64.MinValue; bottom = Int64.MinValue }

    new(r: Rect64) =
        { left = r.left; top = r.top; right = r.right; bottom = r.bottom }

    member this.Width
        with get () = this.right - this.left
        and set value = this.right <- this.left + value

    member this.Height
        with get () = this.bottom - this.top
        and set value = this.bottom <- this.top + value

    member this.IsEmpty() = this.bottom <= this.top || this.right <= this.left
    member this.IsValid() = this.left < Int64.MaxValue
    member this.MidPoint() = Point64((this.left + this.right) / 2L, (this.top + this.bottom) / 2L)

    member this.Contains(pt: Point64) =
        pt.X > this.left && pt.X < this.right && pt.Y > this.top && pt.Y < this.bottom

    member this.Contains(r: Rect64) =
        r.left >= this.left && r.right <= this.right && r.top >= this.top && r.bottom <= this.bottom

    member this.Intersects(r: Rect64) =
        (Math.Max(this.left, r.left) <= Math.Min(this.right, r.right)) &&
        (Math.Max(this.top, r.top) <= Math.Min(this.bottom, r.bottom))

    member this.AsPath() =
        let result = Path64(4)
        result.Add(Point64(this.left, this.top))
        result.Add(Point64(this.right, this.top))
        result.Add(Point64(this.right, this.bottom))
        result.Add(Point64(this.left, this.bottom))
        result

[<Struct>]
type RectD =
    val mutable left: float
    val mutable top: float
    val mutable right: float
    val mutable bottom: float

    new(l: float, t: float, r: float, b: float) =
        { left = l; top = t; right = r; bottom = b }

    new(r: RectD) =
        { left = r.left; top = r.top; right = r.right; bottom = r.bottom }

    new(isValid: bool) =
        if isValid then { left = 0.0; top = 0.0; right = 0.0; bottom = 0.0 }
        else { left = Double.MaxValue; top = Double.MaxValue; right = -Double.MaxValue; bottom = -Double.MaxValue }

    member this.Width
        with get () = this.right - this.left
        and set value = this.right <- this.left + value

    member this.Height
        with get () = this.bottom - this.top
        and set value = this.bottom <- this.top + value

    member this.IsEmpty() = this.bottom <= this.top || this.right <= this.left

    member this.MidPoint() = PointD((this.left + this.right) / 2.0, (this.top + this.bottom) / 2.0)

    member this.Contains(pt: PointD) =
        pt.x > this.left && pt.x < this.right && pt.y > this.top && pt.y < this.bottom

    member this.Contains(r: RectD) =
        r.left >= this.left && r.right <= this.right && r.top >= this.top && r.bottom <= this.bottom

    member this.Intersects(r: RectD) =
        (Math.Max(this.left, r.left) < Math.Min(this.right, r.right)) &&
        (Math.Max(this.top, r.top) < Math.Min(this.bottom, r.bottom))

    member this.AsPath() =
        let result = PathD(4)
        result.Add(PointD(this.left, this.top))
        result.Add(PointD(this.right, this.top))
        result.Add(PointD(this.right, this.bottom))
        result.Add(PointD(this.left, this.bottom))
        result

// UInt128Struct
[<Struct>]
type UInt128Struct =
    val mutable lo64: uint64
    val mutable hi64: uint64

// PointInPolygonResult - declared here because InternalClipper uses it
[<Flags>]
type PointInPolygonResult =
    | IsOn = 0
    | IsInside = 1
    | IsOutside = 2

// InternalClipper static helper class
type InternalClipper private () =

    static let precision_range_error = "Internal Clipper2 Error: Precision is out of range."

    [<Literal>]
    static let MaxInt64 = 9223372036854775807L
    [<Literal>]
    static let MaxCoord = 9223372036854775807L / 4L

    static member internal maxCoord = MaxCoord
    static member internal max_coord: float = float MaxCoord
    static member internal min_coord: float = float -MaxCoord
    static member internal Invalid64 = MaxInt64
    static member internal floatingPointTolerance = 1E-12
    static member internal defaultMinimumEdgeLength = 0.1

    static member CrossProduct(pt1: Point64, pt2: Point64, pt3: Point64) : float =
        (float (pt2.X - pt1.X) * float (pt3.Y - pt2.Y) -
         float (pt2.Y - pt1.Y) * float (pt3.X - pt2.X))

    static member CrossProductSign(pt1: Point64, pt2: Point64, pt3: Point64) : int =
        let a = pt2.X - pt1.X
        let b = pt3.Y - pt2.Y
        let c = pt2.Y - pt1.Y
        let d = pt3.X - pt2.X
        let ab = InternalClipper.MultiplyUInt64(uint64 (Math.Abs(a)), uint64 (Math.Abs(b)))
        let cd = InternalClipper.MultiplyUInt64(uint64 (Math.Abs(c)), uint64 (Math.Abs(d)))
        let signAB = InternalClipper.TriSign(a) * InternalClipper.TriSign(b)
        let signCD = InternalClipper.TriSign(c) * InternalClipper.TriSign(d)

        if signAB = signCD then
            let mutable result = 0
            if ab.hi64 = cd.hi64 then
                if ab.lo64 = cd.lo64 then result <- 0
                else result <- (if ab.lo64 > cd.lo64 then 1 else -1)
            else result <- (if ab.hi64 > cd.hi64 then 1 else -1)
            if ab.hi64 = cd.hi64 && ab.lo64 = cd.lo64 then 0
            else if signAB > 0 then result else -result
        else
            if signAB > signCD then 1 else -1

    static member inline internal CheckPrecision(precision: int) : unit =
        if precision < -8 || precision > 8 then
            raise (Exception(precision_range_error))

    static member inline internal IsAlmostZero(value: float) : bool =
        Math.Abs(value) <= 1E-12

    static member internal TriSign(x: int64) : int =
        if x < 0L then -1 elif x > 0L then 1 else 0

    static member MultiplyUInt64(a: uint64, b: uint64) : UInt128Struct =
        let x1 = (a &&& 0xFFFFFFFFUL) * (b &&& 0xFFFFFFFFUL)
        let x2 = (a >>> 32) * (b &&& 0xFFFFFFFFUL) + (x1 >>> 32)
        let x3 = (a &&& 0xFFFFFFFFUL) * (b >>> 32) + (x2 &&& 0xFFFFFFFFUL)
        let mutable result = UInt128Struct()
        result.lo64 <- (x3 &&& 0xFFFFFFFFUL) <<< 32 ||| (x1 &&& 0xFFFFFFFFUL)
        result.hi64 <- (a >>> 32) * (b >>> 32) + (x2 >>> 32) + (x3 >>> 32)
        result

    static member internal ProductsAreEqual(a: int64, b: int64, c: int64, d: int64) : bool =
        let absA = uint64 (Math.Abs(a))
        let absB = uint64 (Math.Abs(b))
        let absC = uint64 (Math.Abs(c))
        let absD = uint64 (Math.Abs(d))
        let mul_ab = InternalClipper.MultiplyUInt64(absA, absB)
        let mul_cd = InternalClipper.MultiplyUInt64(absC, absD)
        let sign_ab = InternalClipper.TriSign(a) * InternalClipper.TriSign(b)
        let sign_cd = InternalClipper.TriSign(c) * InternalClipper.TriSign(d)
        mul_ab.lo64 = mul_cd.lo64 && mul_ab.hi64 = mul_cd.hi64 && sign_ab = sign_cd

    static member inline internal IsCollinear(pt1: Point64, sharedPt: Point64, pt2: Point64) : bool =
        let a = sharedPt.X - pt1.X
        let b = pt2.Y - sharedPt.Y
        let c = sharedPt.Y - pt1.Y
        let d = pt2.X - sharedPt.X
        InternalClipper.ProductsAreEqual(a, b, c, d)

    static member inline internal DotProduct(pt1: Point64, pt2: Point64, pt3: Point64) : float =
        (float (pt2.X - pt1.X) * float (pt3.X - pt2.X) +
         float (pt2.Y - pt1.Y) * float (pt3.Y - pt2.Y))

    static member inline internal CrossProduct(vec1: PointD, vec2: PointD) : float =
        (vec1.y * vec2.x - vec2.y * vec1.x)

    static member inline internal DotProduct(vec1: PointD, vec2: PointD) : float =
        (vec1.x * vec2.x + vec1.y * vec2.y)

    static member inline internal CheckCastInt64(v: float) : int64 =
        if (v >= InternalClipper.max_coord) || (v <= InternalClipper.min_coord) then InternalClipper.Invalid64
        else int64 (Math.Round(v, MidpointRounding.AwayFromZero))

    static member inline GetLineIntersectPt(ln1a: Point64, ln1b: Point64, ln2a: Point64, ln2b: Point64, ip: byref<Point64>) : bool =
        let dy1 = float (ln1b.Y - ln1a.Y)
        let dx1 = float (ln1b.X - ln1a.X)
        let dy2 = float (ln2b.Y - ln2a.Y)
        let dx2 = float (ln2b.X - ln2a.X)
        let det = dy1 * dx2 - dy2 * dx1
        if det = 0.0 then
            ip <- Point64()
            false
        else
            let t = ((float (ln1a.X - ln2a.X)) * dy2 - (float (ln1a.Y - ln2a.Y)) * dx2) / det
            if t <= 0.0 then ip <- ln1a
            elif t >= 1.0 then ip <- ln1b
            else
                let mutable p = Point64()
                p.X <- int64 (float ln1a.X + t * dx1)
                p.Y <- int64 (float ln1a.Y + t * dy1)
#if USINGZ
                p.Z <- 0L
#endif
                ip <- p
            true

    static member inline GetLineIntersectPt(ln1a: PointD, ln1b: PointD, ln2a: PointD, ln2b: PointD, ip: byref<PointD>) : bool =
        let dy1 = (ln1b.y - ln1a.y)
        let dx1 = (ln1b.x - ln1a.x)
        let dy2 = (ln2b.y - ln2a.y)
        let dx2 = (ln2b.x - ln2a.x)
        let det = dy1 * dx2 - dy2 * dx1
        if det = 0.0 then
            ip <- PointD()
            false
        else
            let t = ((ln1a.x - ln2a.x) * dy2 - (ln1a.y - ln2a.y) * dx2) / det
            if t <= 0.0 then ip <- ln1a
            elif t >= 1.0 then ip <- ln1b
            else
                let mutable p = PointD()
                p.x <- (ln1a.x + t * dx1)
                p.y <- (ln1a.y + t * dy1)
#if USINGZ
                p.z <- 0L
#endif
                ip <- p
            true

    static member internal SegsIntersect(seg1a: Point64, seg1b: Point64, seg2a: Point64, seg2b: Point64, ?inclusive: bool) : bool =
        let inclusive = defaultArg inclusive false
        let dy1 = float (seg1b.Y - seg1a.Y)
        let dx1 = float (seg1b.X - seg1a.X)
        let dy2 = float (seg2b.Y - seg2a.Y)
        let dx2 = float (seg2b.X - seg2a.X)
        let cp = dy1 * dx2 - dy2 * dx1
        if cp = 0.0 then false
        elif inclusive then
            let mutable t = (float (seg1a.X - seg2a.X)) * dy2 - (float (seg1a.Y - seg2a.Y)) * dx2
            if t = 0.0 then true
            elif t > 0.0 then
                if cp < 0.0 || t > cp then false
                else
                    t <- (float (seg1a.X - seg2a.X)) * dy1 - (float (seg1a.Y - seg2a.Y)) * dx1
                    if t = 0.0 then true
                    elif t > 0.0 then (cp > 0.0 && t <= cp)
                    else (cp < 0.0 && t >= cp)
            elif cp > 0.0 || t < cp then false
            else
                t <- (float (seg1a.X - seg2a.X)) * dy1 - (float (seg1a.Y - seg2a.Y)) * dx1
                if t = 0.0 then true
                elif t > 0.0 then (cp > 0.0 && t <= cp)
                else (cp < 0.0 && t >= cp)
        else
            let mutable t = (float (seg1a.X - seg2a.X)) * dy2 - (float (seg1a.Y - seg2a.Y)) * dx2
            if t = 0.0 then false
            elif t > 0.0 then
                if cp < 0.0 || t >= cp then false
                else
                    t <- (float (seg1a.X - seg2a.X)) * dy1 - (float (seg1a.Y - seg2a.Y)) * dx1
                    if t = 0.0 then false
                    elif t > 0.0 then (cp > 0.0 && t < cp)
                    else (cp < 0.0 && t > cp)
            elif cp > 0.0 || t <= cp then false
            else
                t <- (float (seg1a.X - seg2a.X)) * dy1 - (float (seg1a.Y - seg2a.Y)) * dx1
                if t = 0.0 then false
                elif t > 0.0 then (cp > 0.0 && t < cp)
                else (cp < 0.0 && t > cp)

    static member GetBounds(path: Path64) : Rect64 =
        if path.Count = 0 then Rect64()
        else
            let mutable result = Rect64(false)
            for pt in path do
                if pt.X < result.left then result.left <- pt.X
                if pt.X > result.right then result.right <- pt.X
                if pt.Y < result.top then result.top <- pt.Y
                if pt.Y > result.bottom then result.bottom <- pt.Y
            result

    static member GetClosestPtOnSegment(offPt: Point64, seg1: Point64, seg2: Point64) : Point64 =
        if seg1.X = seg2.X && seg1.Y = seg2.Y then seg1
        else
            let dx = float (seg2.X - seg1.X)
            let dy = float (seg2.Y - seg1.Y)
            let mutable q =
                ((float (offPt.X - seg1.X)) * dx + (float (offPt.Y - seg1.Y)) * dy) /
                ((dx * dx) + (dy * dy))
            if q < 0.0 then q <- 0.0
            elif q > 1.0 then q <- 1.0
            Point64(
                seg1.X + int64 (Math.Round(q * dx, MidpointRounding.ToEven)),
                seg1.Y + int64 (Math.Round(q * dy, MidpointRounding.ToEven))
            )

    static member PointInPolygon(pt: Point64, polygon: Path64) : PointInPolygonResult =
        let len = polygon.Count
        let mutable start = 0
        if len < 3 then PointInPolygonResult.IsOutside
        else
            while start < len && polygon.[start].Y = pt.Y do
                start <- start + 1
            if start = len then PointInPolygonResult.IsOutside
            else
                let mutable isAbove = polygon.[start].Y < pt.Y
                let startingAbove = isAbove
                let mutable val_ = 0
                let mutable i = start + 1
                let mutable end_ = len
                let mutable done_ = false

                while not done_ do
                    if i = end_ then
                        if end_ = 0 || start = 0 then
                            done_ <- true
                        else
                            end_ <- start
                            i <- 0

                    if not done_ then
                        if isAbove then
                            while i < end_ && polygon.[i].Y < pt.Y do
                                i <- i + 1
                        else
                            while i < end_ && polygon.[i].Y > pt.Y do
                                i <- i + 1

                        if i = end_ then
                            ()
                        else
                            let curr = polygon.[i]
                            let prev = if i > 0 then polygon.[i - 1] else polygon.[len - 1]

                            if curr.Y = pt.Y then
                                if curr.X = pt.X || (curr.Y = prev.Y && ((pt.X < prev.X) <> (pt.X < curr.X))) then
                                    done_ <- true
                                    val_ <- -1 // sentinel for IsOn
                                else
                                    i <- i + 1
                                    if i = start then done_ <- true
                            else
                                if pt.X < curr.X && pt.X < prev.X then
                                    ()
                                elif pt.X > prev.X && pt.X > curr.X then
                                    val_ <- 1 - val_
                                else
                                    let cps2 = InternalClipper.CrossProductSign(prev, curr, pt)
                                    if cps2 = 0 then
                                        done_ <- true
                                        val_ <- -1 // sentinel for IsOn
                                    elif (cps2 < 0) = isAbove then
                                        val_ <- 1 - val_
                                if not done_ then
                                    isAbove <- not isAbove
                                    i <- i + 1

                if val_ = -1 then PointInPolygonResult.IsOn
                elif isAbove = startingAbove then
                    if val_ = 0 then PointInPolygonResult.IsOutside else PointInPolygonResult.IsInside
                else
                    if i = len then i <- 0
                    let cps =
                        if i = 0 then InternalClipper.CrossProductSign(polygon.[len - 1], polygon.[0], pt)
                        else InternalClipper.CrossProductSign(polygon.[i - 1], polygon.[i], pt)
                    if cps = 0 then PointInPolygonResult.IsOn
                    else
                        if (cps < 0) = isAbove then val_ <- 1 - val_
                        if val_ = 0 then PointInPolygonResult.IsOutside else PointInPolygonResult.IsInside

    static member Path2ContainsPath1(path1: Path64, path2: Path64) : bool =
        let mutable pip = PointInPolygonResult.IsOn
        let mutable result = false
        let mutable decided = false
        let mutable i = 0
        while i < path1.Count && not decided do
            match InternalClipper.PointInPolygon(path1.[i], path2) with
            | PointInPolygonResult.IsOutside ->
                if pip = PointInPolygonResult.IsOutside then
                    result <- false
                    decided <- true
                else
                    pip <- PointInPolygonResult.IsOutside
            | PointInPolygonResult.IsInside ->
                if pip = PointInPolygonResult.IsInside then
                    result <- true
                    decided <- true
                else
                    pip <- PointInPolygonResult.IsInside
            | _ -> ()
            i <- i + 1

        if decided then result
        else
            let mp = InternalClipper.GetBounds(path1).MidPoint()
            InternalClipper.PointInPolygon(mp, path2) <> PointInPolygonResult.IsOutside

#if USINGZ
    static member SetZ(path: Path64, Z: int64) : Path64 =
        let result = Path64(path.Count)
        for pt in path do result.Add(Point64(pt.X, pt.Y, Z))
        result
#endif

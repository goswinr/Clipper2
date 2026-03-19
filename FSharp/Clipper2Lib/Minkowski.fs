#if USINGZ
namespace Clipper2ZLib
#else
namespace Clipper2Lib
#endif

open System
open System.Runtime.InteropServices

[<AbstractClass; Sealed>]
type Minkowski private () =

    static member private MinkowskiInternal(pattern: Path64, path: Path64, isSum: bool, isClosed: bool) : Paths64 =
        let delta = if isClosed then 0 else 1
        let patLen = pattern.Count
        let pathLen = path.Count
        let tmp = Paths64(pathLen)

        for pathPt in path do
            let path2 = Path64(patLen)
            if isSum then
                for basePt in pattern do
                    path2.Add(pathPt + basePt)
            else
                for basePt in pattern do
                    path2.Add(pathPt - basePt)
            tmp.Add(path2)

        let result = Paths64((pathLen - delta) * patLen)
        let mutable g = if isClosed then pathLen - 1 else 0

        let mutable h = patLen - 1
        for i = delta to pathLen - 1 do
            for j = 0 to patLen - 1 do
                let quad = Path64(4)
                quad.Add(tmp.[g].[h])
                quad.Add(tmp.[i].[h])
                quad.Add(tmp.[i].[j])
                quad.Add(tmp.[g].[j])
                if not (ClipperPrimitives.IsPositive(quad)) then
                    result.Add(ClipperPrimitives.ReversePath(quad))
                else
                    result.Add(quad)
                h <- j
            g <- i
        result

    static member Sum(pattern: Path64, path: Path64, isClosed: bool) : Paths64 =
        let subject = Minkowski.MinkowskiInternal(pattern, path, true, isClosed)
        let solution = Paths64()
        let c = Clipper64()
        c.AddPaths(subject, PathType.Subject)
        c.Execute(ClipType.Union, FillRule.NonZero, solution) |> ignore
        solution

    static member Sum(pattern: PathD, path: PathD, isClosed: bool,
                      [<Optional; DefaultParameterValue(2)>] decimalPlaces: int) : PathsD =
        let scale = Math.Pow(10.0, float decimalPlaces)
        let subject = Minkowski.MinkowskiInternal(ClipperPrimitives.ScalePath64(pattern, scale),
                        ClipperPrimitives.ScalePath64(path, scale), true, isClosed)
        let tmp = Paths64()
        let c = Clipper64()
        c.AddPaths(subject, PathType.Subject)
        c.Execute(ClipType.Union, FillRule.NonZero, tmp) |> ignore
        ClipperPrimitives.ScalePathsD(tmp, 1.0 / scale)

    static member Diff(pattern: Path64, path: Path64, isClosed: bool) : Paths64 =
        let subject = Minkowski.MinkowskiInternal(pattern, path, false, isClosed)
        let solution = Paths64()
        let c = Clipper64()
        c.AddPaths(subject, PathType.Subject)
        c.Execute(ClipType.Union, FillRule.NonZero, solution) |> ignore
        solution

    static member Diff(pattern: PathD, path: PathD, isClosed: bool,
                       [<Optional; DefaultParameterValue(2)>] decimalPlaces: int) : PathsD =
        let scale = Math.Pow(10.0, float decimalPlaces)
        let subject = Minkowski.MinkowskiInternal(ClipperPrimitives.ScalePath64(pattern, scale),
                        ClipperPrimitives.ScalePath64(path, scale), false, isClosed)
        let tmp = Paths64()
        let c = Clipper64()
        c.AddPaths(subject, PathType.Subject)
        c.Execute(ClipType.Union, FillRule.NonZero, tmp) |> ignore
        ClipperPrimitives.ScalePathsD(tmp, 1.0 / scale)

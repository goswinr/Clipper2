# Clipper2 F# Port

This folder contains a direct-port of the C# Clipper2 library to F# while staying as close as possible to the C# code shape.

## Documentation

> [!IMPORTANT]
> UP and DOWN here are premised on Y-axis positive down displays, which is the orientation used in Clipper's development.
> see Clipper.Engine.fs

## Files and Structure

### Ported project file:
- `Clipper2Lib/Clipper2Lib.fsproj`

### Copied over from C# but now referencing Clipper2Lib.fsproj

- `Tests/Tests1/Tests1.sln`
- `Tests/Tests2/TestsZ.sln`
- `USINGZ/Clipper2Lib_Z.fsproj` now references files from `Clipper2Lib.fsproj` and conditionally compiles in USINGZ code.

Utils:
- `Utils/SVG/Clipper2.SVG.csproj`
- `Utils/ClipFileIO/Clipper.FileIO.csproj`

### Not copied:
- `Benchmark/` folder
- `Examples/` folder
- `NuGet/` folder

## Porting
Primary planning document:
- `PORT_PLAN.md`

General porting to F# skill:
- `FSharpPortingSkill.md`

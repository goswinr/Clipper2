# Skill README: Fixing Fable Compiler Constraints (Clipper2 F#)

Use this document as a reusable `SKILL.md` when making the F# port compile under Fable without changing runtime semantics.

## Goal

Keep .NET behavior intact while making code acceptable to Fable's supported API surface and compiler restrictions.

## When to Use This Skill

- `fable <fsproj>` fails with unsupported .NET API errors.
- Fable reports `ILAsm` constructor issues.
- Fable reports unsupported `List<T>` members (`Capacity`, `BinarySearch`, etc.).
- Fable reports unsupported BCL helpers (`Array.Empty`, crypto RNG APIs, etc.).

## Fast Workflow

1. Run a forced fresh compile to avoid stale cache:
   - `fable <project.fsproj> --noCache --outDir <temp-dir>`
2. Group errors by pattern, not by file.
3. Apply conditional compilation and helper wrappers.
4. Re-run both:
   - `dotnet build <project.fsproj>`
   - `fable <project.fsproj> --noCache --outDir <temp-dir>`
5. Keep generated `.js` outputs and temp out dirs uncommitted unless intentionally versioned.

## Change Patterns to Reuse

### 1) Unsupported Crypto APIs

Signal:
- `System.Security.Cryptography.RandomNumberGenerator.Create/GetBytes is not supported by Fable`

Pattern:
- Keep the cryptographic path for .NET.
- Add `#if FABLE_COMPILER` fallback that is deterministic enough for hash seeding use, not security.

Example used:
- `FSharp/Clipper2Lib/HashCode.fs`

Template:
```fsharp
#if !FABLE_COMPILER
open System.Security.Cryptography
#endif

static member private GenerateGlobalSeed() : uint32 =
#if FABLE_COMPILER
    let ticks = DateTime.UtcNow.Ticks
    uint32 ticks ^^^ uint32 (ticks >>> 32)
#else
    use rng = RandomNumberGenerator.Create()
    let data = Array.zeroCreate<byte> 4
    rng.GetBytes(data)
    BitConverter.ToUInt32(data, 0)
#endif
```

Semantics note:
- For hashing, this keeps practical behavior parity. Only cryptographic strength differs under Fable.

### 2) ILAsm Errors from Secondary Constructors on `List<T>`-Derived Types

Signal:
- `Cannot compile expression ILAsm ("[I_ldarg 0us]", [], [])`
- Often emitted around `new(... ) as this = ... then this.AddRange(...)` in classes inheriting `List<T>`.

Pattern:
- Keep secondary constructors for non-Fable targets only.
- Add explicit static factory methods for Fable-friendly creation from enumerables.
- Update call sites to use factories.

Examples used:
- `FSharp/Clipper2Lib/Core.fs`
- `FSharp/Clipper2Lib/Clipper.fs`
- `FSharp/Clipper2Lib/ClipperPrimitives.fs`

Template:
```fsharp
type Path64(capacity: int) =
    inherit List<Point64>(capacity)
    new() = Path64(0)
#if !FABLE_COMPILER
    new(path: IEnumerable<Point64>) as this =
        Path64(0)
        then this.AddRange(path)
#endif
    static member FromEnumerable(path: IEnumerable<Point64>) =
        let result = Path64()
        result.AddRange(path)
        result
```

### 3) Unsupported `Array.Empty<T>()`

Signal:
- `System.Array.Empty (static) is not supported by Fable`

Pattern:
- Replace with explicit zero-length allocation.

Example used:
- `FSharp/Clipper2Lib/PooledList.fs`

Template:
```fsharp
let mutable items: 'T[] = Array.zeroCreate<'T> 0
```

### 4) Unsupported `List<T>.Capacity`

Signal:
- `get_Capacity/set_Capacity is not supported by Fable`

Pattern:
- Route preallocation through helper methods.
- No-op the helper for Fable; keep optimization for .NET.

Examples used:
- `FSharp/Clipper2Lib/Engine.fs`
- `FSharp/Clipper2Lib/Triangulation.fs`

Template:
```fsharp
static member inline EnsureCapacity(list: List<'T>, minCapacity: int) =
#if FABLE_COMPILER
    ()
#else
    if list.Capacity < minCapacity then
        list.Capacity <- minCapacity
#endif
```

Semantics note:
- Capacity preallocation is a performance optimization only; behavior is unchanged.

### 5) Unsupported `List<T>.BinarySearch`

Signal:
- `List<T>.BinarySearch is not supported by Fable`

Pattern:
- Implement local binary search with the same contract:
  - Return index when found.
  - Return bitwise-complement insertion index when not found (`~~~insertionIndex`).

Example used:
- `FSharp/Clipper2Lib/Engine.fs` (`BinarySearchInt64`)

Template:
```fsharp
static member BinarySearchInt64(list: List<int64>, value: int64) : int =
    let mutable low = 0
    let mutable high = list.Count - 1
    let mutable found = -1
    while low <= high && found < 0 do
        let mid = low + ((high - low) / 2)
        let midValue = list.[mid]
        if midValue = value then
            found <- mid
        elif midValue < value then
            low <- mid + 1
        else
            high <- mid - 1
    if found >= 0 then found else ~~~low
```

## Verification Checklist

- `dotnet build FSharp/Clipper2Lib/Clipper2Lib.fsproj` succeeds.
- `fable FSharp/Clipper2Lib/Clipper2Lib.fsproj --noCache --outDir <temp-dir>` has no errors.
- Any remaining Fable warnings are reviewed and either accepted or tracked.
- Public API shape remains unchanged unless intentionally versioned.

## Guardrails

- Prefer minimal deltas and localized wrappers over broad rewrites.
- Keep conditional compilation narrow (`#if FABLE_COMPILER` around affected code only).
- Preserve algorithmic logic; only adapt unsupported plumbing/API calls.
- If a change could alter behavior (not just compatibility/perf), add tests before and after.

## Repo-Specific Reminder

- This repo already has a general F# translation skill:
  - `FSharp/FSharpPortingSkill.md`
- Use this file specifically for Fable-compatibility passes after the direct port compiles on .NET.

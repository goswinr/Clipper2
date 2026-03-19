# Clipper2 C# to F# Port Plan (Simplified)

## Prompts used
- continue porting missing function bodies with NotImplementedException following the plan @FSharp/PORT_PLAN.md and @FSharp/FSharpPortingSkill.md  do the files in the sequence as they appear in @FSharp/Clipper2Lib/Clipper2Lib.fsproj , use multiple agents.

The F# port is now fully functional:

Root cause: OutPt objects weren't initializing next/prev to self on first creation in PooledList.fs, breaking the circular doubly-linked list invariant that the entire clipping engine depends on.
Fix: Added poolPt.next <- poolPt; poolPt.prev <- poolPt after constructing new OutPt instances.
Result: All 10 tests pass across both test suites (Tests1: 5/5, TestsZ: 5/5). Both non-Z and USINGZ projects build clean.

c# code at `CSharp/Clipper2Lib` is being ported to F# in `FSharp/Clipper2Lib`.

This document outlines the porting plan, rules, architecture decisions, progress, and next steps.

## 1) Goal and Scope

Goal: deliver a direct F# translation of CSharp/Clipper2Lib with behavior parity.

In scope:
- Port only library code from CSharp/Clipper2Lib.
- Keep public API names and numeric behavior aligned with C#.
- Keep mutable/imperative flow where that preserves behavior.

Out of scope:
- Porting tests, benchmarks, and demo/examples.

## 2) Key Porting Rules

- Preserve C#-like shape (classes/structs/enums) over idiomatic F# rewrites.
- Preserve numeric and rounding semantics exactly.
- Preserve nullability behavior where required.
- Preserve API naming to minimize downstream edits.
- Keep USINGZ conditional compilation behavior.

## 3) F# Compile and Declaration Order

Project file order (required):
1. HashCode.fs
2. Core.fs
3. ClipperPrimitives.fs
4. EngineTypes.fs
5. PooledList.fs
6. Engine.fs
7. RectClip.fs
8. Offset.fs
9. Minkowski.fs
10. Triangulation.fs
11. Clipper.fs

Mandatory mutual type groups:
- Core.fs: Point64 and PointD
- EngineTypes.fs: Vertex and LocalMinima and Active and OutRec and OutPt and HorzSegment and HorzJoin
- Triangulation.fs: Vertex2 and Edge and Triangle

Everything else should remain linear declaration order.

## 4) Architecture Decisions

- Split low-level helpers out of facade into ClipperPrimitives.fs to avoid compile-order cycles.
- Keep PolyPathBase in EngineTypes.fs so OutRec can reference it safely.
- Keep facade Clipper.fs last.
- In Minkowski, avoid facade self-dependency patterns; use engine-level operations directly where needed.

## 5) USINGZ Status

Completed:
- Namespace switching directives added across all F# files.
- Point64/PointD Z members and constructors wired.
- Z callback delegates/properties wired in Engine and Offset.
- MakePathZ overloads added in facade.

Both non-Z and USINGZ projects currently compile clean.

## 6) Current Progress Snapshot

| File | Done % | Notes |
|---|---:|---|
| HashCode.fs | 100 | Complete |
| Core.fs | 100 | Complete |
| ClipperPrimitives.fs | 100 | Complete |
| EngineTypes.fs | 100 | Complete |
| PooledList.fs | 100 | Complete |
| Engine.fs | 100 | Complete |
| RectClip.fs | 100 | Complete |
| Offset.fs | 100 | Complete |
| Minkowski.fs | 100 | Complete |
| Triangulation.fs | 100 | Complete |
| Clipper.fs | 100 | Complete |
| Total | 100 | All files complete; all tests pass (Tests1 5/5, TestsZ 5/5) |

## Testing Progress
- `dotnet build FSharp/Clipper2Lib/Clipper2Lib.fsproj` succeeds.
- `dotnet build FSharp/USINGZ/Clipper2Lib_Z.fsproj` succeeds.
- `dotnet test FSharp/Tests/Tests1/Tests1.csproj` — 5/5 pass (TestClosedPaths, TestOpenPaths, TestPolytree2, TestPolytree3, TestOffsetEmpty).
- `dotnet test FSharp/Tests/Tests2/TestsZ.csproj` — 5/5 pass (TestSubjUnion64, TestSubjClipUnion64, TestSubjUnionD, TestSubjClipUnionD, TestMysteryD).

## 7) Critical Path

All milestones complete:
- Milestone A: Core.fs and ClipperPrimitives.fs — done.
- Milestone B: Engine.fs core algorithm pipeline — done.
- Milestone C: Parity validation — all tests pass.

## 8) Validation Strategy

- API parity: compare exported type/member names with C#.
- Numeric parity: area, orientation, intersections, scaling/rounding-sensitive operations.
- Structural parity: path counts, vertex counts, orientation/order.
- Conditional parity: validate both non-Z and USINGZ once algorithmic implementation is complete.

## 9) Known F# Porting Constraints Already Resolved

- Keyword conflict: renamed C# parameter rec to r.
- Added required attributes for custom equality/no comparison.
- Added AllowNullLiteral where C# null semantics are required.
- Resolved PointD and InternalClipper helper ordering.
- Resolved OutRec to PolyPathBase type-order dependency.
- Lifted nested C# types to namespace-level F# types where required.

## 10) Completion Definition

Done means:
- All stub methods removed.
- Non-Z and USINGZ projects compile clean.
- Representative tests pass for polygon, open-path, polytree, and USINGZ scenarios.

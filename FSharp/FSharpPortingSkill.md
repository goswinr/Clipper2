# Skill: C# to F# Direct Translation


## File Order and declaration order

Try to keep the file order in F# the same as C#, but within each file, ensure that types and functions are declared before they are used. This may require reordering some declarations or splitting types into multiple files to avoid circular dependencies.
In F# files, declaration order matters. Types and functions must be declared before they are used. This is different from C#, where the order of declarations does not matter. When porting from C# to F#, we need to ensure that the order of types and functions in the F# files allows for successful compilation without forward references. This may require reordering some declarations or splitting types into multiple files to avoid circular dependencies.


## Translation Rules


### Preserve C# Shape
- Keep classes, structs, enums as their F# equivalents (`type ... class`, `[<Struct>] type`, enum).
- Keep mutable fields mutable. Use `let mutable` or `val mutable`.
- Keep imperative control flow (`while`, `for`, `if/else`, early return via mutable + condition).
- Do NOT convert to idiomatic F# (no `|>` pipelines, `match` replacing if/else chains, `Option` replacing null, etc.) unless required for correctness.


### Numeric and Rounding Parity
- `(long)` cast = `int64` in F#.
- `(double)` cast = `float` in F#.
- `Math.Round(x, MidpointRounding.AwayFromZero)` stays exactly the same.
- Preserve all explicit casts; F# does not auto-widen.
- `long.MaxValue` = `Int64.MaxValue`; `long.MinValue` = `Int64.MinValue`.
- `double.MaxValue` = `Double.MaxValue`.

### Null Handling
- Reference types that are compared to `null` in C# need `[<AllowNullLiteral>]` on their type declaration.
- Use `isNull x` or `not (isNull x)` instead of `x == null` / `x != null`.
- Use `Unchecked.defaultof<_>` for `default(T)` on value types.

### Keep Conditional Compilation compiler directives
- Preserve `#if ...` blocks as-is, including any nested directives.

### Keyword Conflicts
append with `'` or use a more descriptive name:
- C# parameter named `rec` becomes `rec'` in F#.
- C# parameter named `val` becomes `value` or `v` in F#.
- C# parameter named `type` becomes `typ` in F#.
- C# parameter named `open` becomes `open'` or `isOpen` in F#.

### Properties and Methods
- C# `get`/`set` properties become `member this.Prop with get() = ... and set(v) = ...`.
- C# `static` methods become `static member`.
- C# `internal` becomes F# `internal`.
- C# `protected` becomes F# `member internal` (F# has no protected; use internal).
- C# `override` becomes F# `override`.
- C# `virtual` becomes F# `abstract` + `default` pair.
- C# `ref` / `out` parameters: use `byref<T>` or `outref<T>`.

### Collections and Types
- don't use F# lists ; keep `List<T>` as `ResizeArray<T>` (an alias for `System.Collections.Generic.List<T>`).
This minimizes downstream edits and preserves mutability semantics.

### Loops and Control Flow
- `for (int i = 0; i < n; i++)` = `for i = 0 to n - 1 do`.
- `for (int i = n; i >= 0; i--)` = `for i = n downto 0 do`.
- `while` loops stay as `while ... do`.
- C# `break` / `continue` / early `return`: use a mutable flag pattern since F# lacks these. Example:
  ```fsharp
  let mutable break_ = false
  let mutable i = 0
  while i < n && not break_ do
      if condition then break_ <- true
      else
          // body
          i <- i + 1
  ```
- C# `switch`: use `match ... with` or `if/elif/else`.
- C# ternary `a ? b : c` = `if a then b else c`.

### Operators
- C# `&&` = F# `&&`; C# `||` = F# `||`.
- C# `&` (bitwise) = F# `&&&`; C# `|` (bitwise) = F# `|||`.
- C# `<<` = F# `<<<`; C# `>>` = F# `>>>`.
- C# `~` (bitwise not) = F# `~~~`.
- Enum flag checks: `(flags & Flag) != 0` = `(flags &&& Flag) <> enum 0`.

### Nested Types
- C# nested classes/enums are lifted to namespace level in F#.
- If the C# code references `OuterClass.InnerType`, in F# it's just `InnerType`.

### String and Formatting
- `$"..."` interpolation = `$"..."` in F# (same syntax).
- `string.Format(...)` = `String.Format(...)`.
- `ToString()` stays `ToString()`.

### Exception Handling
- `try { ... } catch (Exception e) { ... }` = `try ... with | :? Exception as e -> ...`.
- `throw new ...` = `raise (...)` or `invalidArg` / `invalidOp`.

### Delegates and Events
- C# delegate types = F# simple lambda anonymous functions. Use `Func<...>` / `Action<...>` only if necessary.

### Optional Parameters
- C# optional parameters with default values = F# optional parameters with `[<Optional; DefaultParameterValue(...)>]` attributes.

## Inlining
 - Replace [MethodImpl(MethodImplOptions.AggressiveInlining)] with `inline` keyword on the method.

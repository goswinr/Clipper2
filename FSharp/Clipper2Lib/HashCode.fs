#if USINGZ
namespace Clipper2ZLib
#else
namespace Clipper2Lib
#endif

open System
open System.ComponentModel
open System.Runtime.CompilerServices
open System.Security.Cryptography

[<Struct; CustomEquality; NoComparison>]
type HashCode =

    static member private s_seed: uint32 = HashCode.GenerateGlobalSeed()

    static member private GenerateGlobalSeed() : uint32 =
        use rng = RandomNumberGenerator.Create()
        let data = Array.zeroCreate<byte> 4
        rng.GetBytes(data)
        BitConverter.ToUInt32(data, 0)

    static member Combine<'T1, 'T2>(value1: 'T1, value2: 'T2) : int =
        let hc1 = uint32 (match box value1 with null -> 0 | v -> v.GetHashCode())
        let hc2 = uint32 (match box value2 with null -> 0 | v -> v.GetHashCode())
        let mutable hash = HashCode.MixEmptyState()
        hash <- hash + 8u
        hash <- HashCode.QueueRound(hash, hc1)
        hash <- HashCode.QueueRound(hash, hc2)
        hash <- HashCode.MixFinal(hash)
        int hash

    // private members on a struct can't be inline when called from public members (F# compiler limitation).
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member private QueueRound(hash: uint32, queuedValue: uint32) : uint32 =
        HashCode.RotateLeft(hash + (queuedValue * 3266489917u), 17) * 668265263u

    static member private MixEmptyState() : uint32 =
        HashCode.s_seed + 374761393u

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member private MixFinal(hash: uint32) : uint32 =
        let mutable h = hash
        h <- h ^^^ (h >>> 15)
        h <- h * 2246822519u
        h <- h ^^^ (h >>> 13)
        h <- h * 3266489917u
        h <- h ^^^ (h >>> 16)
        h

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member RotateLeft(value: uint32, offset: int) : uint32 =
        (value <<< offset) ||| (value >>> (32 - offset))

    [<Obsolete("HashCode is a mutable struct and should not be compared with other HashCodes. Use ToHashCode to retrieve the computed hash code.", true)>]
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    override _.GetHashCode() : int =
        raise (NotSupportedException("HashCode.GetHashCode() is not supported"))

    [<Obsolete("HashCode is a mutable struct and should not be compared with other HashCodes.", true)>]
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    override _.Equals(_obj: obj) : bool =
        raise (NotSupportedException("HashCode.Equals() is not supported"))

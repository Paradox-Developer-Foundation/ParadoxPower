using System.Collections.Generic;
using System.Diagnostics.CodeAnalysis;
using ParadoxPower.Parser;
#if RELEASE
using System.Runtime.CompilerServices;
#endif

namespace ParadoxPower.CSharpExtensions;

public static class ValueExtensions
{
    /// <summary>
    /// 尝试获取<c>int</c>
    /// </summary>
    /// <param name="val"></param>
    /// <param name="value"></param>
    /// <returns>当 <see cref="Types.Value"/> 为 <c>int</c> 时返回<c>true</c>, 反之返回<c>false</c></returns>
    public static bool TryGetInt(this Types.Value val, out int value)
    {
        if (val.IsInt)
        {
#if DEBUG
            value = ((Types.Value.Int)val).Item;
#else
            value = Unsafe.As<Types.Value.Int>(val).Item;
#endif
            return true;
        }
        value = 0;
        return false;
    }

    /// <summary>
    /// 尝试获取<c>bool</c>
    /// </summary>
    /// <param name="val"></param>
    /// <param name="value"></param>
    /// <returns>当 <see cref="Types.Value"/> 为 <c>bool</c> 时返回<c>true</c>, 反之返回<c>false</c></returns>
    public static bool TryGetBool(this Types.Value val, out bool value)
    {
        if (val.IsBool)
        {
#if DEBUG

            value = ((Types.Value.Bool)val).Item;
#else
            value = Unsafe.As<Types.Value.Bool>(val).Item;
#endif
            return true;
        }

        value = false;
        return false;
    }

    /// <summary>
    /// 尝试获取<c>decimal</c>
    /// </summary>
    /// <param name="val"></param>
    /// <param name="value"></param>
    /// <returns>当 <see cref="Types.Value"/> 为 <c>decimal</c> 时返回<c>true</c>, 反之返回<c>false</c></returns>
    public static bool TryGetDecimal(this Types.Value val, out decimal value)
    {
        if (val.IsFloat)
        {
#if DEBUG
            value = ((Types.Value.Float)val).Item;
#else
            value = Unsafe.As<Types.Value.Float>(val).Item;
#endif
            return true;
        }

        value = 0;
        return false;
    }

    /// <summary>
    /// 尝试获取<c>string</c>
    /// </summary>
    /// <param name="val"></param>
    /// <param name="value"></param>
    /// <returns>当 <see cref="Types.Value"/> 为 <c>string</c> 时返回<c>true</c>, 反之返回<c>false</c></returns>
    public static bool TryGetString(this Types.Value val, [NotNullWhen(true)] out string? value)
    {
        if (val.IsString)
        {
#if DEBUG
            value = ((Types.Value.String)val).Item;
#else
            value = Unsafe.As<Types.Value.String>(val).Item;
#endif
            return true;
        }

        value = null;
        return false;
    }

    /// <summary>
    /// 尝试获取带引号类型的<c>string</c>
    /// </summary>
    /// <param name="val"></param>
    /// <param name="value"></param>
    /// <returns>当 <see cref="Types.Value"/> 为带引号类型的 <c>string</c> 时返回<c>true</c>, 反之返回<c>false</c></returns>
    public static bool TryGetQString(this Types.Value val, [NotNullWhen(true)] out string? value)
    {
        if (val.IsQString)
        {
#if DEBUG
            value = ((Types.Value.QString)val).Item;
#else
            value = Unsafe.As<Types.Value.QString>(val).Item;
#endif
            return true;
        }

        value = null;
        return false;
    }

    /// <summary>
    /// 尝试获取 <see cref="Types.Statement"/>
    /// </summary>
    /// <param name="val"></param>
    /// <param name="value"></param>
    /// <returns>当 <see cref="Types.Value"/> 为 <see cref="Types.Statement"/> 时返回<c>true</c>, 反之返回<c>false</c></returns>
    public static bool TryGetClause(
        this Types.Value val,
        [NotNullWhen(true)] out IReadOnlyCollection<Types.Statement>? value
    )
    {
        if (val.IsClause)
        {
#if DEBUG
            value = ((Types.Value.Clause)val).Item;
#else
            value = Unsafe.As<Types.Value.Clause>(val).Item;
#endif
            return true;
        }

        value = null;
        return false;
    }
}

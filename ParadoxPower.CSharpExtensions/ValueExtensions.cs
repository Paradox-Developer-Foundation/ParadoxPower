using System.Collections.Generic;
using System.Diagnostics.CodeAnalysis;
using ParadoxPower.Parser;

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
            value = ((Types.Value.Int)val).Item;
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
            value = ((Types.Value.Bool)val).Item;
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
            value = ((Types.Value.Float)val).Item;
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
            value = ((Types.Value.String)val).Item;
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
            value = ((Types.Value.QString)val).Item;
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
            value = ((Types.Value.Clause)val).Item;
            return true;
        }

        value = null;
        return false;
    }
}

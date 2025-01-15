using System.Collections.Generic;
using System.Diagnostics.CodeAnalysis;
using ParadoxPower.Parser;

namespace ParadoxPower.CSharpExtensions;

public static class ValueExtensions
{
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

    public static bool TryGetClause(
        this Types.Value val,
        [NotNullWhen(true)] out IReadOnlyCollection<Types.Statement>? clause
    )
    {
        if (val.IsClause)
        {
            clause = ((Types.Value.Clause)val).Item;
            return true;
        }

        clause = null;
        return false;
    }
}

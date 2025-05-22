# ParadoxPower

A Paradox parser, used for parsing Paradox script files

一个解析器, 用来解析 Paradox 脚本文件

## Usage

Install `ParadoxPower` and  `ParadoxPower.CSharpExtensions`

## Example

```c#
var parser = new TextParser(filePath);
if (parser.IsFailure)
{
    return;
}
var rootNode = result.GetResult();

// 或者
if (!TextParser.TryParse(filePath, out var rootNode, out _))
{
    return;
}
```



```c#
var text = """
    state={
    	id=1
        name="STATE_1" # Corsica

        provinces={
        	3838 9851 11804 
    	}
	}
"""

var rootNode = new TextParser("", text).GetResult();
if (!rootNode.TryGetNode("state", out var stateNode))
{
    return;
}

// 获取 provinces 下所有元素
var provinces = new List<int>();
if (stateNode.TryGetNode("provinces", out var provincesNode))
{
    foreach (var item in provincesNode.LeafValues)
    {
        if (int.TryParse(item.ValueText, out int provinceId))
        {
            provinces.Add(provinceId);
        }
    }
}

// 获取 Id
if (stateNode.TryGetLeaf("id", out var leaf))
{
    string id = leaf.ValueText;
}
// 或者 (不推荐)
// string id = stateNode.GetLeaf("id")!.Value.ValueText;

// 输出 State 下所有元素的 Key
foreach (var child in stateNode.AllArray)
{
    if (child.TryGetNode(out var node))
    {
        Console.WriteLine(node.Key);
    }
    else if (child.TryGetLeaf(out var leafChild))
    {
        Console.WriteLine(leafChild.Key);
    }
}

// 修改id为2
leaf.Value = Types.Value.NewInt(2);

// 修改 name 为 "STATE_2"
if (stateNode.TryGetLeaf("name", out var nameLeaf))
{
	nameLeaf.Value = Types.Value.NewQString("STATE_2");
}

// rootNode转化为脚本字符串
string output = rootNode.ToScript();
Console.WriteLine(output);
```



About Color

```c#
// 关于颜色, 其中 "rgb" 被划分为了 color 节点下的 Leaf, 而在 CWTools 中 rgb 在 AST 中是不存在的
var color = "color = rgb { 255 1 1 }";
```

## Used Project

- [VModer - HOI4 vscode extension]([textGamex/VModer: HOI4 vscode extension](https://github.com/textGamex/VModer))

## Supported Games

- Europa Universalis IV (欧陆风云4)
- Hearts of Iron IV (钢铁雄心4)
- Victoria (维多利亚)
- Stellaris (群星)

## Thanks

This project is built upon [CWTools](https://github.com/cwtools/cwtools), and it would not have been possible without the foundation provided by [CWTools](https://github.com/cwtools/cwtools).

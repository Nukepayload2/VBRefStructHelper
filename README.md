# RefStructHelper

一个用于Visual Basic的Roslyn分析器，专门处理受限类型（ref struct）的装箱检测和相关规则。

## 项目概述

RefStructHelper是一个.NET分析器项目，旨在扩展Visual Basic中受限类型（ref struct）的使用规则。当项目中启用`<OptionRestrict>On</OptionRestrict>`选项时，该分析器会激活并检测各种可能导致受限类型装箱的操作。

### 什么是受限类型？

受限类型（在 C# 叫 `ref struct`）是具有`System.Runtime.CompilerServices.IsByRefLikeAttribute`特性的结构体。这些类型：
- 只能在栈上分配（无法装箱）
- 不能嵌套 ByRef
- 对出栈有使用限制(unscoped ref)以确保内存安全

## 当前实现

### 已完成的功能

- **BCX31394**: 检测受限类型转换为`Object`或`ValueType`的违规操作

### 待实现的功能

- BCX31396: 受限类型不能转换为`Nullable(Of T)`及相关限制
- BCX32061: 受限类型作为泛型参数的限制
- BCX36598: 防止LINQ装箱受限类型
- BCX36640: 防止Lambda闭包装箱受限类型
- BCX37052: Async/Iterator状态机中受限类型变量的限制
- BCX31393: 防止调用受限类型继承的实例方法装箱
- 完善OptionRestrict选项检查逻辑
- 添加完整的单元测试
- 诊断消息的本地化支持

## 项目结构

```
RefStructHelper/                 # 主分析器项目
├── RefStructConvertToBoxedTypeAnalyzer.vb  # 核心分析器实现
├── AnalyzerReleases.Shipped.md            # 已发布的分析器规则
├── AnalyzerReleases.Unshipped.md          # 未发布的分析器规则
└── Tasks.md                              # 项目任务和进度跟踪

RefStructHelper.Demo/            # 演示项目
└── Class1.vb                           # 示例代码

RefStructHelper.Tests/           # 测试项目
├── Test1.vb                           # 测试用例
└── MSTestSettings.vb                  # 测试配置
```

## 安装和使用

### 要求

- .NET Standard 2.0
- Visual Basic项目
- Roslyn编译器

### 配置

在目标项目的`.vbproj`文件中添加以下配置来启用分析器：

```xml
<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <OptionRestrict>On</OptionRestrict>
  </PropertyGroup>
  
  <ItemGroup>
    <ProjectReference Include="..\RefStructHelper\RefStructHelper.vbproj" 
                      OutputItemType="Analyzer" 
                      ReferenceOutputAssembly="false" />
  </ItemGroup>
</Project>
```

## 诊断规则

### BCX31394 - 受限类型装箱检测

**严重程度**: 错误 (Error)

**描述**: 检测受限类型被转换为`Object`或`ValueType`的违规操作。

**示例违规代码**:
```vb
' 假设SomeRefStruct是受限类型
Dim refStruct As SomeRefStruct = GetRefStruct()
Dim obj As Object = refStruct  ' 这将触发BCX31394错误
```

## 开发

### 构建项目

```bash
dotnet build
```

### 运行测试

```bash
dotnet test
```

### 项目依赖

- Microsoft.CodeAnalysis.VisualBasic (4.14.0)

## 贡献

请查看[Tasks.md](RefStructHelper/Tasks.md)文件了解当前的开发任务和进度。

## 许可证

请查看[LICENSE](LICENSE)文件了解许可信息。

## 相关资源

- [Roslyn Analyzer Documentation](https://github.com/dotnet/roslyn-analyzers)
- [Visual Basic Language Specification](https://learn.microsoft.com/en-us/dotnet/visual-basic/)

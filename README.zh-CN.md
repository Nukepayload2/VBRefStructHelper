# RefStructHelper

[English Introduction](https://github.com/Nukepayload2/VBRefStructHelper/blob/master/README.md)

这是一个 Visual Basic .NET 分析器项目，旨在将受限类型的概念扩展到所有 `ref struct`。
此分析器检测在使用受限类型时可能导致 [InvalidProgramException](https://learn.microsoft.com/en-us/dotnet/api/system.invalidprogramexception) 的各种操作。

## 什么是受限类型？

受限类型（在 C# 中称为 `ref struct`）是具有 `System.Runtime.CompilerServices.IsByRefLikeAttribute` 特性的结构体。这些类型：
- 只能在栈上分配（无法装箱）
- 不能嵌套 `ByRef` 或其他受限类型
- 有关于栈引用的使用限制（`scoped`/`unscoped` ref）以确保内存安全

## 当前实现

### 已完成的功能

- BCX31394: 检测受限类型转换为 `Object` 或 `ValueType` 的违规操作
- BCX31396: 受限类型转换为 `Nullable(Of T)` 及相关限制
- BCX32061: 受限类型作为泛型参数的限制
- BCX36598: 防止 LINQ 中受限类型的装箱
- BCX36640: 防止 Lambda 闭包中受限类型的装箱
- BCX37052: 防止 Async/Iterator 状态机中受限类型的装箱
- BCX31393: 防止调用受限类型继承的实例方法时装箱
- `en-US` 和 `zh-CN` 诊断消息的本地化支持

### 未来计划
- 对于返回值进行受限类型的流分析（`scoped` 和 `unscoped`）
- 在 `Async` 或 `Iterator` 方法中声明受限类型变量的流分析
- 识别 `allows ref struct` 约束
- 更多资源翻译（P1: 日语和欧洲语言）

## 项目结构

```
RefStructHelper/                 # 主分析器项目
RefStructHelper.Demo/            # 演示项目
RefStructHelper.Tests/           # 测试项目
```

## 安装和使用

### 要求
- 支持 Visual Basic 17.13 或更高版本的 .NET SDK

### 通过 NuGet 包管理器安装
[![NuGet](https://img.shields.io/nuget/v/Nukepayload2.CodeAnalysis.ExtendRestrictedTypes.svg)](https://www.nuget.org/packages/Nukepayload2.CodeAnalysis.ExtendRestrictedTypes/)

### 通过源码引用

在目标项目的 `.vbproj` 文件中添加以下配置来启用分析器：

```xml
<Project Sdk="Microsoft.NET.Sdk">
  <ItemGroup>
    <ProjectReference Include="Path to RefStructHelper.vbproj" 
                      OutputItemType="Analyzer" 
                      ReferenceOutputAssembly="false" />
  </ItemGroup>
</Project>
```

## 许可证

请查看 [LICENSE](LICENSE) 文件了解许可信息。

## 相关资源

- [Roslyn](https://github.com/dotnet/roslyn)
- [Visual Basic Language Specification](https://learn.microsoft.com/en-us/dotnet/visual-basic/)

# RefStructHelper

[中文介绍](https://github.com/Nukepayload2/VBRefStructHelper/blob/master/README.zh-CN.md)

This is a .NET analyzer project designed to extend the concept of restricted types to all `ref struct`s in Visual Basic. 
This analyzer detects various operations that could throw [InvalidProgramException](https://learn.microsoft.com/en-us/dotnet/api/system.invalidprogramexception) when using restricted types.

## What are Restricted Types?

Restricted types (called `ref struct` in C#) are structures with the `System.Runtime.CompilerServices.IsByRefLikeAttribute` attribute. These types:
- Can only be allocated on the stack (cannot be boxed)
- Cannot nest `ByRef`
- Have usage restrictions related to stack references (scoped/unscoped ref) to ensure memory safety

## Current Implementation

### Completed Features

- BCX31394: Detects violations of restricted types being converted to `Object` or `ValueType`
- BCX31396: Restrictions on converting restricted types to `Nullable(Of T)` and related limitations
- BCX32061: Restrictions on using restricted types as generic parameters
- BCX36598: Prevent boxing of restricted types in LINQ
- BCX36640: Prevent boxing of restricted types in Lambda closures
- BCX37052: Restrictions on restricted type variables in Async/Iterator state machines
- BCX31393: Prevent boxing when calling instance methods inherited by restricted types
- `en-US` and `zh-CN` Localization support for diagnostic messages

### Future Plans
- Flow analysis for returning restricted types (`scoped` and `unscoped`)
- Flow analysis for declaring variables of restricted types in `Async` or `Iterator` methods
- Respect the `allows ref struct` constraint
- More resource translations

## Project Structure

```
RefStructHelper/                 # Main analyzer project
RefStructHelper.Demo/            # Demo project
RefStructHelper.Tests/           # Test project
```

## Installation and Usage

### Requirements
- .NET SDK that supports Visual Basic 17.13 or later

### Via NuGet Package Manager
[![NuGet](https://img.shields.io/nuget/v/Nukepayload2.CodeAnalysis.ExtendRestrictedTypes.svg)](https://www.nuget.org/packages/Nukepayload2.CodeAnalysis.ExtendRestrictedTypes/)

### Via Project Reference

Add the following configuration to the target project's `.vbproj` file to enable the analyzer:

```xml
<Project Sdk="Microsoft.NET.Sdk">
  <ItemGroup>
    <ProjectReference Include="..\RefStructHelper\RefStructHelper.vbproj" 
                      OutputItemType="Analyzer" 
                      ReferenceOutputAssembly="false" />
  </ItemGroup>
</Project>
```

## License

Please see the [LICENSE](LICENSE) file for licensing information.

## Related Resources

- [Roslyn](https://github.com/dotnet/roslyn)
- [Visual Basic Language Specification](https://learn.microsoft.com/en-us/dotnet/visual-basic/)

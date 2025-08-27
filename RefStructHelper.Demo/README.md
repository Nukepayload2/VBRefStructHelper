# RefStructHelper Demo

这个目录包含了RefStructHelper分析器的演示代码，展示了各种受限类型（ref struct）使用场景的正确和错误用法。

## Demo文件说明

### 1. RestrictedTypeMemberAccessDemo.vb
演示调用受限类型继承的Object/ValueType方法的错误使用（BCX31393）
- `ToString()`, `Equals()`, `GetHashCode()`, `GetType()` 等方法调用
- `ReferenceEquals()` 静态方法调用
- 字符串插值中的受限类型使用

### 2. RestrictedTypeUsageDemo.vb
演示受限类型的使用限制（BCX31396）
- Nullable类型声明：`Dim x As Span(Of Integer)?`
- 数组类型声明：`Dim x As Span(Of Integer)()`
- 集合初始化表达式中的受限类型
- 类字段和属性声明为受限类型
- 匿名类型成员为受限类型
- ByRef参数为受限类型
- 返回值为受限类型

### 3. RestrictedTypeGenericDemo.vb
演示受限类型作为泛型参数的限制（BCX32061）
- 泛型类型参数：`List(Of Span(Of Integer))`
- 泛型约束：`Of T As Span(Of Integer)`
- Inherits和Implements声明
- Event声明
- 方法参数和返回值为泛型包含受限类型

### 4. RestrictedTypeLinqDemo.vb
演示LINQ中受限类型的使用限制（BCX36598）
- LINQ查询中使用受限类型作为条件
- LINQ范围变量声明为受限类型
- Group By子句中使用受限类型

### 5. RestrictedTypeLambdaDemo.vb
演示Lambda表达式中受限类型的使用限制（BCX36640）
- Lambda表达式中捕获受限类型变量
- 多行Lambda表达式中的受限类型使用
- 嵌套Lambda表达式中的受限类型使用

### 6. RestrictedTypeAsyncDemo.vb
演示Async/Iterator方法中受限类型的使用限制（BCX37052）
- Async方法中声明受限类型局部变量
- Iterator方法中声明受限类型局部变量
- 通过参数传递受限类型是允许的正确用法

## 使用方法

1. 确保在主项目的`.vbproj`文件中启用了分析器：
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

2. 构建项目时，分析器会检测到错误使用并报告相应的诊断信息。

3. 注释中标识了每个应该触发的诊断代码，便于验证分析器的正确性。

## 注意事项

- 所有demo类都使用了`<Obsolete("Suppress default ref struct obsolete errors")>`属性来抑制默认的ref struct过时警告
- 错误用法会在编译时触发相应的BCX诊断错误
- 正确用法展示了如何安全地使用受限类型

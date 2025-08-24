Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.Diagnostics
Imports Microsoft.CodeAnalysis.VisualBasic
Imports Microsoft.VisualStudio.TestTools.UnitTesting
Imports System.Collections.Immutable

<TestClass>
<Obsolete("Suppress default ref struct obsolete errors")>
Public Class RefStructConvertToBoxedTypeAnalyzerTests

    ' 工具方法：编译代码并返回诊断结果

    Private Function GetDiagnostics(source As String) As ImmutableArray(Of Diagnostic)
        ' 创建解析选项
        Dim parseOptions = New VisualBasicParseOptions()

        ' 创建语法树
        Dim syntaxTree = VisualBasicSyntaxTree.ParseText(source, parseOptions)

        ' 创建引用（包括系统引用和我们的分析器）
        Dim references As MetadataReference() = {
            MetadataReference.CreateFromFile(GetType(Object).Assembly.Location),
            MetadataReference.CreateFromFile(GetType(Span(Of )).Assembly.Location),
            MetadataReference.CreateFromFile(GetType(ValueType).Assembly.Location)
        }.DistinctBy(Function(it) it.FilePath).ToArray

        ' 创建编译选项，启用 OptionRestrict
        Dim compilationOptions = New VisualBasicCompilationOptions(OutputKind.DynamicallyLinkedLibrary)

        ' 创建编译
        Dim compilation = VisualBasicCompilation.Create(
            "TestAssembly",
            {syntaxTree},
            references,
            compilationOptions
        )

        ' 运行我们的分析器
        Dim analyzer As New RefStructConvertToBoxedTypeAnalyzer()
        Dim analyzers As ImmutableArray(Of DiagnosticAnalyzer) = ImmutableArray.Create(Of DiagnosticAnalyzer)({analyzer})
        Dim compilationWithAnalyzers = compilation.WithAnalyzers(analyzers)
        Dim diagnostics = compilationWithAnalyzers.GetAnalyzerDiagnosticsAsync().Result

        Return diagnostics
    End Function

    ' 工具方法：检查是否包含指定ID的诊断
    Private Function ContainsDiagnostic(diagnostics As ImmutableArray(Of Diagnostic), diagnosticId As String) As Boolean
        Return diagnostics.Any(Function(d) d.Id = diagnosticId)
    End Function

    ' 测试1: 直接将 Span 赋值给 Object 变量
    <TestMethod>
    Public Sub TestSpanToObjectAssignment()
        Dim source As String = "
Imports System
Imports System.Runtime.InteropServices

<Obsolete(""Suppress default ref struct obsolete errors"")>
Class TestClass
    Sub TestMethod()
        Dim arr As Integer() = {1, 2, 3, 4, 5}
        Dim span As Span(Of Integer) = arr.AsSpan()
        Dim obj As Object = span  ' 这应该触发 BCX31394
    End Sub
End Class
"
        Dim diagnostics = GetDiagnostics(source)
        Assert.IsTrue(ContainsDiagnostic(diagnostics, "BCX31394"), "应该检测到 BCX31394 诊断")
    End Sub

    ' 测试2: 使用 CType 转换 Span 到 Object
    <TestMethod>
    Public Sub TestCTypeSpanToObject()
        Dim source As String = "
Imports System
Imports System.Runtime.InteropServices

<Obsolete(""Suppress default ref struct obsolete errors"")>
Class TestClass
    Sub TestMethod()
        Dim arr As Integer() = {1, 2, 3, 4, 5}
        Dim span As Span(Of Integer) = arr.AsSpan()
        Dim obj As Object = CType(span, Object)  ' 这应该触发 BCX31394
    End Sub
End Class
"
        Dim diagnostics = GetDiagnostics(source)
        Assert.IsTrue(ContainsDiagnostic(diagnostics, "BCX31394"), "应该检测到 BCX31394 诊断")
    End Sub

    ' 测试3: 使用 DirectCast 转换 Span 到 Object
    <TestMethod>
    Public Sub TestDirectCastSpanToObject()
        Dim source As String = "
Imports System
Imports System.Runtime.InteropServices

<Obsolete(""Suppress default ref struct obsolete errors"")>
Class TestClass
    Sub TestMethod()
        Dim arr As Integer() = {1, 2, 3, 4, 5}
        Dim span As Span(Of Integer) = arr.AsSpan()
        Dim obj As Object = DirectCast(span, Object)  ' 这应该触发 BCX31394
    End Sub
End Class
"
        Dim diagnostics = GetDiagnostics(source)
        Assert.IsTrue(ContainsDiagnostic(diagnostics, "BCX31394"), "应该检测到 BCX31394 诊断")
    End Sub

    ' 测试4: 将 Span 作为 Object 参数传递
    <TestMethod>
    Public Sub TestSpanAsObjectParameter()
        Dim source As String = "
Imports System
Imports System.Runtime.InteropServices

<Obsolete(""Suppress default ref struct obsolete errors"")>
Class TestClass
    Sub TestMethod()
        Dim arr As Integer() = {1, 2, 3, 4, 5}
        Dim span As Span(Of Integer) = arr.AsSpan()
        TestMethodTakingObject(span)  ' 这应该触发 BCX31394
    End Sub

    Sub TestMethodTakingObject(obj As Object)
    End Sub
End Class
"
        Dim diagnostics = GetDiagnostics(source)
        Assert.IsTrue(ContainsDiagnostic(diagnostics, "BCX31394"), "应该检测到 BCX31394 诊断")
    End Sub

    ' 测试5: 将 Span 添加到 Object 数组
    <TestMethod>
    Public Sub TestSpanInObjectArray()
        Dim source As String = "
Imports System
Imports System.Runtime.InteropServices

<Obsolete(""Suppress default ref struct obsolete errors"")>
Class TestClass
    Sub TestMethod()
        Dim arr As Integer() = {1, 2, 3, 4, 5}
        Dim span As Span(Of Integer) = arr.AsSpan()
        Dim objArray As Object() = {span, ""hello"", 42}  ' 这应该触发 BCX31394
    End Sub
End Class
"
        Dim diagnostics = GetDiagnostics(source)
        Assert.IsTrue(ContainsDiagnostic(diagnostics, "BCX31394"), "应该检测到 BCX31394 诊断")
    End Sub

    <TestMethod>
    Public Sub TestSpanInObjectArray2()
        Dim source As String = "
Imports System
Imports System.Runtime.InteropServices

<Obsolete(""Suppress default ref struct obsolete errors"")>
Class TestClass
    Sub TestMethod()
        Dim arr As Integer() = {1, 2, 3, 4, 5}
        Dim span As Span(Of Integer) = arr.AsSpan()
        Dim objArray = New Object() {span, ""hello"", 42}  ' 这应该触发 BCX31394
    End Sub
End Class
"
        Dim diagnostics = GetDiagnostics(source)
        Assert.IsTrue(ContainsDiagnostic(diagnostics, "BCX31394"), "应该检测到 BCX31394 诊断")
    End Sub

    <TestMethod>
    Public Sub TestSpanInObjectArray3()
        Dim source As String = "
Imports System
Imports System.Runtime.InteropServices

<Obsolete(""Suppress default ref struct obsolete errors"")>
Class TestClass
    Sub TestMethod()
        Dim arr As Integer() = {1, 2, 3, 4, 5}
        Dim span As Span(Of Integer) = arr.AsSpan()
        Dim objArray = {span, ""hello"", 42}  ' 这应该触发 BCX31394
    End Sub
End Class
"
        Dim diagnostics = GetDiagnostics(source)
        Assert.IsTrue(ContainsDiagnostic(diagnostics, "BCX31394"), "应该检测到 BCX31394 诊断")
    End Sub

    <TestMethod>
    Public Sub TestSpanInObjectList()
        Dim source As String = "
Imports System
Imports System.Runtime.InteropServices

<Obsolete(""Suppress default ref struct obsolete errors"")>
Class TestClass
    Sub TestMethod()
        Dim arr As Integer() = {1, 2, 3, 4, 5}
        Dim span As Span(Of Integer) = arr.AsSpan()
        Dim objList4 = New List(Of Object) From {span, ""hello"", 42}  ' 这应该触发 BCX31394
    End Sub
End Class
"
        Dim diagnostics = GetDiagnostics(source)
        Assert.IsTrue(ContainsDiagnostic(diagnostics, "BCX31394"), "应该检测到 BCX31394 诊断")
    End Sub

    <TestMethod>
    Public Sub TestSpanInObjectList2()
        Dim source As String = "
Imports System
Imports System.Runtime.InteropServices

<Obsolete(""Suppress default ref struct obsolete errors"")>
Class TestClass
    Sub TestMethod()
        Dim arr As Integer() = {1, 2, 3, 4, 5}
        Dim span As Span(Of Integer) = arr.AsSpan()
        Dim objList4 As New List(Of Object) From {span, ""hello"", 42}  ' 这应该触发 BCX31394
    End Sub
End Class
"
        Dim diagnostics = GetDiagnostics(source)
        Assert.IsTrue(ContainsDiagnostic(diagnostics, "BCX31394"), "应该检测到 BCX31394 诊断")
    End Sub

    ' 测试6: 从函数返回 Span 作为 Object
    <TestMethod>
    Public Sub TestReturnSpanAsObject()
        Dim source As String = "
Imports System
Imports System.Runtime.InteropServices

<Obsolete(""Suppress default ref struct obsolete errors"")>
Class TestClass
    Function TestReturnSpanAsObject() As Object
        Dim arr As Integer() = {1, 2, 3, 4, 5}
        Dim span As Span(Of Integer) = arr.AsSpan()
        Return span  ' 这应该触发 BCX31394
    End Function
End Class
"
        Dim diagnostics = GetDiagnostics(source)
        Assert.IsTrue(ContainsDiagnostic(diagnostics, "BCX31394"), "应该检测到 BCX31394 诊断")
    End Sub

    ' 测试7: 将 Span 赋值给 ValueType 变量
    <TestMethod>
    Public Sub TestSpanToValueTypeAssignment()
        Dim source As String = "
Imports System
Imports System.Runtime.InteropServices

<Obsolete(""Suppress default ref struct obsolete errors"")>
Class TestClass
    Sub TestMethod()
        Dim arr As Integer() = {1, 2, 3, 4, 5}
        Dim span As Span(Of Integer) = arr.AsSpan()
        Dim valueType As ValueType = span  ' 这应该触发 BCX31394
    End Sub
End Class
"
        Dim diagnostics = GetDiagnostics(source)
        Assert.IsTrue(ContainsDiagnostic(diagnostics, "BCX31394"), "应该检测到 BCX31394 诊断")
    End Sub

    ' 测试8: 使用 CType 转换 Span 到 ValueType
    <TestMethod>
    Public Sub TestCTypeSpanToValueType()
        Dim source As String = "
Imports System
Imports System.Runtime.InteropServices

<Obsolete(""Suppress default ref struct obsolete errors"")>
Class TestClass
    Sub TestMethod()
        Dim arr As Integer() = {1, 2, 3, 4, 5}
        Dim span As Span(Of Integer) = arr.AsSpan()
        Dim valueType As ValueType = CType(span, ValueType)  ' 这应该触发 BCX31394
    End Sub
End Class
"
        Dim diagnostics = GetDiagnostics(source)
        Assert.IsTrue(ContainsDiagnostic(diagnostics, "BCX31394"), "应该检测到 BCX31394 诊断")
    End Sub

    <TestMethod>
    Public Sub TestCObjSpan()
        Dim source As String = "
Imports System
Imports System.Runtime.InteropServices

<Obsolete(""Suppress default ref struct obsolete errors"")>
Class TestClass
    Sub TestMethod()
        Dim arr As Integer() = {1, 2, 3, 4, 5}
        Dim span As Span(Of Integer) = arr.AsSpan()
        Dim valueType = CObj(span)  ' 这应该触发 BCX31394
    End Sub
End Class
"
        Dim diagnostics = GetDiagnostics(source)
        Assert.IsTrue(ContainsDiagnostic(diagnostics, "BCX31394"), "应该检测到 BCX31394 诊断")
    End Sub

    ' 测试9: ReadOnlySpan 的类似测试
    <TestMethod>
    Public Sub TestReadOnlySpanToObject()
        Dim source As String = "
Imports System
Imports System.Runtime.InteropServices

<Obsolete(""Suppress default ref struct obsolete errors"")>
Class TestClass
    Sub TestMethod()
        Dim readOnlySpan As ReadOnlySpan(Of Integer) = {1, 2, 3, 4, 5}
        Dim obj As Object = readOnlySpan  ' 这也应该触发 BCX31394
    End Sub
End Class
"
        Dim diagnostics = GetDiagnostics(source)
        Assert.IsTrue(ContainsDiagnostic(diagnostics, "BCX31394"), "应该检测到 BCX31394 诊断")
    End Sub

    ' 测试10: 正确的用法不应该触发
    <TestMethod>
    Public Sub TestCorrectUsage()
        Dim source As String = "
Imports System
Imports System.Runtime.InteropServices

<Obsolete(""Suppress default ref struct obsolete errors"")>
Class TestClass
    Sub TestMethod()
        ' 直接使用 Span，不进行装箱转换
        Dim arr As Integer() = {1, 2, 3, 4, 5}
        Dim span As Span(Of Integer) = arr.AsSpan()
        span(0) = 10
        Dim length As Integer = span.Length
        ' 这些都是正确的用法，不应该触发
    End Sub
End Class
"
        Dim diagnostics = GetDiagnostics(source)
        Assert.IsFalse(ContainsDiagnostic(diagnostics, "BCX31394"), "不应该检测到 BCX31394 诊断")
    End Sub

    ' 测试11: 没有使用 Span 的正常代码不应该触发
    <TestMethod>
    Public Sub TestNormalCode()
        Dim source As String = "
Imports System

Class TestClass
    Sub TestMethod()
        Dim obj As Object = ""hello""
        Dim arr As Object() = {""hello"", 42}
        Dim valueType As ValueType = 123
    End Sub
End Class
"
        Dim diagnostics = GetDiagnostics(source)
        Assert.IsFalse(ContainsDiagnostic(diagnostics, "BCX31394"), "不应该检测到 BCX31394 诊断")
    End Sub

End Class

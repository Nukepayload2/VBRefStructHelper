Imports Microsoft.VisualStudio.TestTools.UnitTesting

<TestClass>
Public Class RefStructConvertToBoxedTypeAnalyzerTests

    Private Sub AssertThatDiagTriggeredInSub(snippetContent As String)
        Dim source As String = $"
Imports System
Imports System.Runtime.InteropServices

<Obsolete(""Suppress default ref struct obsolete errors"")>
Class TestClass

    ' 辅助方法
    Sub TestMethodTakingObject(obj As Object)
        ' 这个方法用于测试参数传递场景
    End Sub

    ' 辅助方法
    Sub TestMethodTakingValueType(obj As ValueType)
        ' 这个方法用于测试参数传递场景
    End Sub

    Private Class Something
        Sub New(arg As Span(Of Integer))

        End Sub
    End Class

    Private Event SomeEvent(arg As Span(Of Integer))

    Sub TestMethod()
        Dim arr As Integer() = {{1, 2, 3, 4, 5}}
        Dim span As Span(Of Integer) = arr.AsSpan()
        {snippetContent}
    End Sub
End Class
"
        With GetSyntaxTreeTextAndDiagnostics(source, snippetContent)
            Assert.IsTrue(ContainsDiagnostic(.diagnostics, "BCX31394"), $"应该检测到 BCX31394 诊断。语法树内容: {vbCrLf}{ .syntaxTreeText}")
        End With
    End Sub

    <TestMethod>
    Public Sub TestSpanToObjectAssignment()
        Dim snippetContent = "Dim obj As Object = span ' 这应该触发 BCX31394"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    ' 使用 CType 转换 Span 到 Object
    <TestMethod>
    Public Sub TestCTypeSpanToObject()
        Dim snippetContent = "Dim obj As Object = CType(span, Object)  ' 这应该触发 BCX31394"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    ' 使用 DirectCast 转换 Span 到 Object
    <TestMethod>
    Public Sub TestDirectCastSpanToObject()
        Dim snippetContent = "Dim obj As Object = DirectCast(span, Object)  ' 这应该触发 BCX31394"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    ' 将 Span 赋值给 ValueType 变量
    <TestMethod>
    Public Sub TestSpanToValueTypeAssignment()
        Dim snippetContent = "Dim valueType As ValueType = span  ' 这应该触发 BCX31394"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    ' 使用 CType 转换 Span 到 ValueType
    <TestMethod>
    Public Sub TestCTypeSpanToValueType()
        Dim snippetContent = "Dim valueType As ValueType = CType(span, ValueType)  ' 这应该触发 BCX31394"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    ' 使用 CObj 转换 Span 到 Object
    <TestMethod>
    Public Sub TestCObjSpanToObject()
        Dim snippetContent = "Dim obj = CObj(span)  ' 这应该触发 BCX31394"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    ' 使用 TryCast 转换 Span 到 Object
    <TestMethod>
    Public Sub TestTryCastSpanToObject()
        Dim snippetContent = "Dim obj As Object = TryCast(span, Object)  ' 这应该触发 BCX31394"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    ' 使用 TryCast 转换 Span 到 ValueType
    <TestMethod>
    Public Sub TestTryCastSpanToValueType()
        Dim snippetContent = "Dim valueType As ValueType = TryCast(span, ValueType)  ' 这应该触发 BCX31394"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    ' 使用 DirectCast 转换 Span 到 ValueType
    <TestMethod>
    Public Sub TestDirectCastSpanToValueType()
        Dim snippetContent = "Dim valueType As ValueType = DirectCast(span, ValueType)  ' 这应该触发 BCX31394"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    ' 将 Span 赋值给 ValueType 变量 (直接赋值)
    <TestMethod>
    Public Sub TestSpanToValueTypeAssignmentDirect()
        Dim snippetContent = "Dim valueType As ValueType = span  ' 这应该触发 BCX31394"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    ' 将 Span 作为 Object 参数传递
    <TestMethod>
    Public Sub TestSpanAsObjectParameter()
        Dim snippetContent = "TestMethodTakingObject(span)"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    ' 将 Span 作为 ValueType 参数传递
    <TestMethod>
    Public Sub TestSpanAsValueTypeParameter()
        Dim snippetContent = "TestMethodTakingValueType(span)"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    <TestMethod>
    Public Sub TestEventRaiseWithSpan()
        Dim snippetContent = "RaiseEvent SomeEvent(span)  ' 这应该触发 BCX31394"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    <TestMethod>
    Public Sub TestNewSomethingWithSpan()
        Dim snippetContent = "Dim x As New Something(span)  ' 这应该触发 BCX31394"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    <TestMethod>
    Public Sub TestNewSomethingWithSpanImplicit()
        Dim snippetContent = "Dim y = New Something(span)  ' 这应该触发 BCX31394"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    ' 从函数返回 Span 作为 Object
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
        With GetSyntaxTreeTextAndDiagnostics(source)
            Assert.IsTrue(ContainsDiagnostic(.diagnostics, "BCX31394"), $"应该检测到 BCX31394 诊断。语法树内容: { .syntaxTreeText}")
        End With
    End Sub

    ' 从函数返回 Span 作为 ValueType
    <TestMethod>
    Public Sub TestReturnSpanAsValueType()
        Dim source As String = "
Imports System
Imports System.Runtime.InteropServices

<Obsolete(""Suppress default ref struct obsolete errors"")>
Class TestClass
    Function TestReturnSpanAsValueType() As ValueType
        Dim arr As Integer() = {1, 2, 3, 4, 5}
        Dim span As Span(Of Integer) = arr.AsSpan()
        Return span  ' 这应该触发 BCX31394
    End Function
End Class
"
        With GetSyntaxTreeTextAndDiagnostics(source)
            Assert.IsTrue(ContainsDiagnostic(.diagnostics, "BCX31394"), $"应该检测到 BCX31394 诊断。语法树内容: { .syntaxTreeText}")
        End With
    End Sub

    ' ReadOnlySpan 的类似测试
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
        With GetSyntaxTreeTextAndDiagnostics(source)
            Assert.IsTrue(ContainsDiagnostic(.diagnostics, "BCX31394"), $"应该检测到 BCX31394 诊断。语法树内容: { .syntaxTreeText}")
        End With
    End Sub

    ' 正确的用法不应该触发
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
        With GetSyntaxTreeTextAndDiagnostics(source)
            Assert.IsFalse(ContainsDiagnostic(.diagnostics, "BCX31394"), $"不应该检测到 BCX31394 诊断。语法树内容: { .syntaxTreeText}")
        End With
    End Sub

    ' 没有使用 Span 的正常代码不应该触发
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
        With GetSyntaxTreeTextAndDiagnostics(source)
            Assert.IsFalse(ContainsDiagnostic(.diagnostics, "BCX31394"), $"不应该检测到 BCX31394 诊断。语法树内容: { .syntaxTreeText}")
        End With
    End Sub

End Class

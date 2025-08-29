Imports Microsoft.VisualStudio.TestTools.UnitTesting

<TestClass>
Public Class RefStructBCX31393AnalyzerTests

    Private Shared Sub AssertThatShouldHaveError(source As FormattableString)
        With GetSyntaxTreeTextAndDiagnostics(source)
            Assert.IsTrue(ContainsDiagnostic(.diagnostics, "BCX31393"), $"应该检测到 BCX31393 诊断。语法树内容:  {vbCrLf}{ .syntaxTreeText}")
        End With
    End Sub

    Private Shared Sub AssertThatShouldNotHaveError(source As FormattableString)
        With GetSyntaxTreeTextAndDiagnostics(source)
            Assert.IsFalse(ContainsDiagnostic(.diagnostics, "BCX31393"), $"不应该检测到 BCX31393 诊断。语法树内容:  {vbCrLf}{ .syntaxTreeText}")
        End With
    End Sub

    Private Shared Sub AssertThatDiagTriggeredInSub(snippetContent As String)
        Dim source As FormattableString = $"
Imports System
Imports System.Runtime.InteropServices

<Obsolete(""Suppress default ref struct obsolete errors"")>
Class TestClass

    <System.Runtime.CompilerServices.IsByRefLike>
    Private Structure DemoType

    End Structure

    Sub TestMethod()
        Dim span As DemoType
        {snippetContent}
    End Sub
End Class
"
        AssertThatShouldHaveError(source)
    End Sub

    ' ====================
    ' 调用受限类型继承的 Object 方法测试
    ' ====================

    <TestMethod>
    Public Sub TestRestrictedTypeToString()
        Dim snippetContent = "span.ToString()"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    <TestMethod>
    Public Sub TestRestrictedTypeEquals()
        Dim snippetContent = "span.Equals(span)"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    <TestMethod>
    Public Sub TestRestrictedTypeGetHashCode()
        Dim snippetContent = "span.GetHashCode()"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    <TestMethod>
    Public Sub TestRestrictedTypeGetType()
        Dim snippetContent = "span.GetType()"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    ' ====================
    ' 调用受限类型继承的 ValueType 方法测试
    ' ====================

    <TestMethod>
    Public Sub TestRestrictedTypeValueTypeEquals()
        Dim snippetContent = "Dim result As Boolean = span.Equals(CType(Nothing, ValueType))"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    ' ====================
    ' 字符串插值测试
    ' ====================

    <TestMethod>
    Public Sub TestRestrictedTypeInStringInterpolation()
        Dim snippetContent = "Dim str As String = $""Span: {span}"""
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    ' ====================
    ' 正确用法测试
    ' ====================

    <TestMethod>
    Public Sub TestRegularMembers()
        Dim source As FormattableString = $"
Imports System
Imports System.Runtime.InteropServices

<Obsolete(""Suppress default ref struct obsolete errors"")>
Class TestClass
    Sub TestMethod()
        Dim arr As Integer() = {{1, 2, 3, 4, 5}}
        Dim span As Span(Of Integer) = arr.AsSpan()
        Dim allowString = span.ToString()
        arr(0) = span.Length
        Dim sliced = span.Slice(0,2)
    End Sub
End Class
"
        AssertThatShouldNotHaveError(source)
    End Sub

    <TestMethod>
    Public Sub TestNormalObjectUsage()
        Dim snippet = "Dim obj As Object = ""hello""
Dim str As String = obj.ToString()
Dim hash As Integer = obj.GetHashCode()
Dim type As Type = obj.GetType()"
        AssertThatCorrectInMethod(snippet)
    End Sub

    Private Shared Sub AssertThatCorrectInMethod(snippet As String)
        Dim source As FormattableString = $"
Imports System

Class TestClass
    Sub TestMethod()
{snippet}
    End Sub
End Class
"
        AssertThatShouldNotHaveError(source)
    End Sub

    <TestMethod>
    Public Sub TestNormalValueTypeUsage()
        Dim source As FormattableString = $"
Imports System

Class TestClass
    Sub TestMethod()
        Dim value As Integer = 42
        Dim str As String = value.ToString()
        Dim hash As Integer = value.GetHashCode()
    End Sub
End Class
"
        AssertThatShouldNotHaveError(source)
    End Sub

End Class

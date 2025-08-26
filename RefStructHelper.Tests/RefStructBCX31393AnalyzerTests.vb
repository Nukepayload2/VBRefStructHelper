Imports Microsoft.VisualStudio.TestTools.UnitTesting

<TestClass, Ignore("没做")>
Public Class RefStructBCX31393AnalyzerTests

    Private Shared Sub AssertThatShouldHaveError(snippetContent As String, source As String)
        With GetSyntaxTreeTextAndDiagnostics(source, snippetContent)
            Assert.IsTrue(ContainsDiagnostic(.diagnostics, "BCX31393"), $"应该检测到 BCX31393 诊断。语法树内容:  {vbCrLf}{ .syntaxTreeText}")
        End With
    End Sub

    Private Shared Sub AssertThatShouldNotHaveError(snippetContent As String, source As String)
        With GetSyntaxTreeTextAndDiagnostics(source, snippetContent)
            Assert.IsFalse(ContainsDiagnostic(.diagnostics, "BCX31393"), $"不应该检测到 BCX31393 诊断。语法树内容:  {vbCrLf}{ .syntaxTreeText}")
        End With
    End Sub

    Private Shared Sub AssertThatDiagTriggeredInSub(snippetContent As String)
        Dim source As String = $"
Imports System
Imports System.Runtime.InteropServices

<Obsolete(""Suppress default ref struct obsolete errors"")>
Class TestClass
    Sub TestMethod()
        Dim arr As Integer() = {{1, 2, 3, 4, 5}}
        Dim span As Span(Of Integer) = arr.AsSpan()
        {snippetContent}
    End Sub
End Class
"
        AssertThatShouldHaveError(snippetContent, source)
    End Sub

    ' ====================
    ' 调用受限类型继承的 Object 方法测试
    ' ====================

    <TestMethod>
    Public Sub TestRestrictedTypeToString()
        Dim snippetContent = "Dim str As String = span.ToString()"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    <TestMethod>
    Public Sub TestRestrictedTypeEquals()
        Dim snippetContent = "Dim result As Boolean = span.Equals(span)"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    <TestMethod>
    Public Sub TestRestrictedTypeGetHashCode()
        Dim snippetContent = "Dim hash As Integer = span.GetHashCode()"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    <TestMethod>
    Public Sub TestRestrictedTypeGetType()
        Dim snippetContent = "Dim type As Type = span.GetType()"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    <TestMethod>
    Public Sub TestRestrictedTypeReferenceEquals()
        Dim snippetContent = "Dim result As Boolean = ReferenceEquals(span, span)"
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
        Dim source As String = "
Imports System
Imports System.Runtime.InteropServices

<Obsolete(""Suppress default ref struct obsolete errors"")>
Class TestClass
    Sub TestMethod()
        Dim arr As Integer() = {{1, 2, 3, 4, 5}}
        Dim span As Span(Of Integer) = arr.AsSpan()
        arr(0) = span.Length
        Dim sliced = span.Slice(0,2)
    End Sub
End Class
"
        AssertThatShouldNotHaveError(source, source)
    End Sub

    <TestMethod>
    Public Sub TestNormalObjectUsage()
        Dim source As String = "
Imports System

Class TestClass
    Sub TestMethod()
        Dim obj As Object = ""hello""
        Dim str As String = obj.ToString()
        Dim hash As Integer = obj.GetHashCode()
        Dim type As Type = obj.GetType()
    End Sub
End Class
"
        AssertThatShouldNotHaveError(source, source)
    End Sub

    <TestMethod>
    Public Sub TestNormalValueTypeUsage()
        Dim source As String = "
Imports System

Class TestClass
    Sub TestMethod()
        Dim value As Integer = 42
        Dim str As String = value.ToString()
        Dim hash As Integer = value.GetHashCode()
    End Sub
End Class
"
        AssertThatShouldNotHaveError(source, source)
    End Sub

End Class

Imports Microsoft.VisualStudio.TestTools.UnitTesting

<TestClass>
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
        Dim snippetContent = "Dim result As Boolean = span.Equals(CType(span, Object))"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    <TestMethod>
    Public Sub TestRestrictedTypeValueTypeGetHashCode()
        Dim snippetContent = "Dim hash As Integer = span.GetHashCode()"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    ' ====================
    ' 成员访问测试
    ' ====================

    <TestMethod>
    Public Sub TestRestrictedTypeMemberAccess()
        Dim snippetContent = "Dim method As MethodInfo = span.ToString().GetType().GetMethod(""Length"")"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    <TestMethod>
    Public Sub TestRestrictedTypePropertyChain()
        Dim snippetContent = "Dim name As String = span.GetType().Name"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    <TestMethod>
    Public Sub TestRestrictedTypeMethodChain()
        Dim snippetContent = "Dim result As String = span.ToString().ToUpper()"
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

    <TestMethod>
    Public Sub TestRestrictedTypeInStringConcat()
        Dim snippetContent = "Dim str As String = ""Span: "" & span.ToString()"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    ' ====================
    ' 集合操作测试
    ' ====================

    <TestMethod>
    Public Sub TestRestrictedTypeInCollection()
        Dim snippetContent = "Dim list As New List(Of Object) From {span}"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    <TestMethod>
    Public Sub TestRestrictedTypeInArray()
        Dim snippetContent = "Dim arr As Object() = {span}"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    ' ====================
    ' 正确用法测试
    ' ====================

    <TestMethod>
    Public Sub TestNormalRestrictedTypeUsage()
        Dim source As String = "
Imports System
Imports System.Runtime.InteropServices

<Obsolete(""Suppress default ref struct obsolete errors"")>
Class TestClass
    Sub TestMethod()
        Dim arr As Integer() = {1, 2, 3, 4, 5}
        Dim span As Span(Of Integer) = arr.AsSpan()
        ' 直接使用 Span 的方法和属性
        Dim length As Integer = span.Length
        Dim first As Integer = span(0)
        span(0) = 10
        ' 这些都是正确的用法，不应该触发 BCX31393
    End Sub
End Class
"
        AssertThatShouldNotHaveError(source, source)
    End Sub

    <TestMethod>
    Public Sub TestRestrictedTypeInRestrictedStruct()
        Dim source As String = "
Imports System
Imports System.Runtime.InteropServices

<System.Runtime.CompilerServices.IsByRefLike>
Structure RestrictedStruct
    Public SpanField As Span(Of Integer)
    
    Sub TestMethod()
        ' 在受限结构体内部使用受限类型是允许的
        Dim length As Integer = SpanField.Length
        SpanField(0) = 42
    End Sub
End Structure
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

Imports Microsoft.VisualStudio.TestTools.UnitTesting

<TestClass>
Public Class RefStructBCX36598AnalyzerTests

    Private Shared Sub AssertThatShouldHaveError(snippetContent As String, source As String)
        With GetSyntaxTreeTextAndDiagnostics(source, snippetContent)
            Assert.IsTrue(ContainsDiagnostic(.diagnostics, "BCX36598"), $"应该检测到 BCX36598 诊断。语法树内容:  {vbCrLf}{ .syntaxTreeText}")
        End With
    End Sub

    Private Shared Sub AssertThatShouldNotHaveError(snippetContent As String, source As String)
        With GetSyntaxTreeTextAndDiagnostics(source, snippetContent)
            Assert.IsFalse(ContainsDiagnostic(.diagnostics, "BCX36598"), $"不应该检测到 BCX36598 诊断。语法树内容:  {vbCrLf}{ .syntaxTreeText}")
        End With
    End Sub

    Private Shared Sub AssertThatDiagTriggeredInSub(snippetContent As String)
        Dim source As String = $"
Imports System
Imports System.Runtime.InteropServices
Imports System.Linq

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
    ' LINQ 查询中使用受限类型测试
    ' ====================

    <TestMethod>
    Public Sub TestRestrictedTypeInLinqFromClause()
        Dim snippetContent = "Dim query = From item In arr Select item"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    <TestMethod>
    Public Sub TestRestrictedTypeInLinqWhereClause()
        Dim snippetContent = "Dim query = From x In arr Where x > 0 Select x"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    <TestMethod>
    Public Sub TestRestrictedTypeInLinqSelectClause()
        Dim snippetContent = "Dim query = From x In arr Select span"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    <TestMethod>
    Public Sub TestRestrictedTypeInLinqLetClause()
        Dim snippetContent = "Dim query = From x In arr Let y = span Select x, y"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    <TestMethod>
    Public Sub TestRestrictedTypeInLinqGroupByClause()
        Dim snippetContent = "Dim query = From x In arr Group x By Key = span Into g"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    <TestMethod>
    Public Sub TestRestrictedTypeInLinqJoinClause()
        Dim snippetContent = "Dim query = From x In arr Join y In arr On x Equals y Where span.Length > 0 Select x"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    <TestMethod>
    Public Sub TestRestrictedTypeInLinqOrderByClause()
        Dim snippetContent = "Dim query = From x In arr Order By span.Length Select x"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    ' ====================
    ' LINQ 范围变量声明测试
    ' ====================

    <TestMethod>
    Public Sub TestRestrictedTypeAsRangeVariable()
        Dim snippetContent = "Dim query = From span In arr Select span"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    <TestMethod>
    Public Sub TestRestrictedTypeInAggregateQuery()
        Dim snippetContent = "Dim result = arr.Aggregate(Function(acc, x) span)"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    <TestMethod>
    Public Sub TestRestrictedTypeInSelectMany()
        Dim snippetContent = "Dim query = From x In arr From y In span Select x, y"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    ' ====================
    ' 正确用法测试
    ' ====================

    <TestMethod>
    Public Sub TestNormalLinqUsage()
        Dim source As String = "
Imports System
Imports System.Linq

Class TestClass
    Sub TestMethod()
        Dim arr As Integer() = {1, 2, 3, 4, 5}
        Dim query = From item In arr Where item > 0 Select item
        Dim result = query.ToList()
    End Sub
End Class
"
        AssertThatShouldNotHaveError(source, source)
    End Sub

    <TestMethod>
    Public Sub TestNormalLinqWithNormalTypes()
        Dim source As String = "
Imports System
Imports System.Linq

Class TestClass
    Sub TestMethod()
        Dim numbers = {1, 2, 3, 4, 5}
        Dim strings = {""hello"", ""world""}
        Dim query = From n In numbers, s In strings Select New With {.Number = n, .Text = s}
        Dim result = query.ToList()
    End Sub
End Class
"
        AssertThatShouldNotHaveError(source, source)
    End Sub

End Class

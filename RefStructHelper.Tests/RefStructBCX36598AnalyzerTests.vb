Imports Microsoft.VisualStudio.TestTools.UnitTesting

<TestClass>
Public Class RefStructBCX36598AnalyzerTests

    Private Shared Sub AssertThatShouldHaveError(source As FormattableString)
        With GetSyntaxTreeTextAndDiagnostics(source)
            Assert.IsTrue(ContainsDiagnostic(.diagnostics, "BCX36598"), $"应该检测到 BCX36598 诊断。语法树内容:  {vbCrLf}{ .syntaxTreeText}")
        End With
    End Sub

    Private Shared Sub AssertThatShouldNotHaveError(source As FormattableString)
        With GetSyntaxTreeTextAndDiagnostics(source)
            Assert.IsFalse(ContainsDiagnostic(.diagnostics, "BCX36598"), $"不应该检测到 BCX36598 诊断。语法树内容:  {vbCrLf}{ .syntaxTreeText}")
        End With
    End Sub

    Private Shared Sub AssertThatDiagTriggeredInSub(snippetContent As String)
        Dim source As FormattableString = $"
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
        AssertThatShouldHaveError(source)
    End Sub

    ' ====================
    ' LINQ 查询中使用受限类型测试: 把 span 捕获到闭包里都算
    ' ====================

    <TestMethod>
    Public Sub TestRestrictedTypeInWhereClause()
        Dim snippetContent = "Dim q = From a In arr Where a = span.Length"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    <TestMethod>
    Public Sub TestRestrictedTypeInLetClause()
        Dim snippetContent = "Dim q = From a In arr Let b = span.Length Select a + b"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    <TestMethod>
    Public Sub TestRestrictedTypeInSelectClause()
        Dim snippetContent = "Dim q = From a In arr Select a, g = span.Length"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    <TestMethod>
    Public Sub TestRestrictedTypeInGroupByClause()
        Dim snippetContent = "Dim q = From a In arr Group By span.Length Into Group"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    ' ====================
    ' LINQ 范围变量声明测试: 把 span 声明成范围变量都算
    ' ====================

    <TestMethod>
    Public Sub TestRestrictedTypeInSelectRangeVariable()
        Dim snippetContent = "Dim q = From a In arr Select a, g = span"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    <TestMethod>
    Public Sub TestRestrictedTypeInLetRangeVariable()
        Dim snippetContent = "Dim q = From a In arr Let b = span Select a, b"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    <TestMethod>
    Public Sub TestRestrictedTypeAsSpanInSelectRangeVariable()
        Dim snippetContent = "Dim q = From a In arr Select a, g = arr.AsSpan"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    <TestMethod>
    Public Sub TestRestrictedTypeAsSpanInLetRangeVariable()
        Dim snippetContent = "Dim q = From a In arr Let b = arr.AsSpan Select a, b"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    <TestMethod>
    Public Sub TestRestrictedTypeInGroupByRangeVariable()
        Dim snippetContent = "Dim q = From a In arr Group By span Into Group"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    ' ====================
    ' 正确用法测试
    ' ====================

    <TestMethod>
    Public Sub TestNormalLinqUsage()
        Dim snippet = "Dim arr As Integer() = {1, 2, 3, 4, 5}
Dim query = From item In arr Where item > 0 Select item
Dim result = query.ToList()"
        AssertThatCorrectInMethod(snippet)
    End Sub

    Private Shared Sub AssertThatCorrectInMethod(snippet As String)
        Dim source As FormattableString = $"
Imports System
Imports System.Linq

Class TestClass
    Sub TestMethod()
{snippet}
    End Sub
End Class
"
        AssertThatShouldNotHaveError(source)
    End Sub

    <TestMethod>
    Public Sub TestComboLinqUsageInFromIn()
        Dim snippet = "Dim arr As Integer() = {1, 2, 3, 4, 5}
Dim query = From item In arr.AsSpan.ToArray Where item > 0 Select item
Dim result = query.ToList()"
        AssertThatCorrectInMethod(snippet)
    End Sub

    <TestMethod>
    Public Sub TestNormalLinqWithNormalTypes()
        Dim snippet = "Dim numbers = {1, 2, 3, 4, 5}
Dim strings = {""hello"", ""world""}
Dim query = From n In numbers, s In strings Select New With {.Number = n, .Text = s}
Dim result = query.ToList()"
        AssertThatCorrectInMethod(snippet)
    End Sub

End Class

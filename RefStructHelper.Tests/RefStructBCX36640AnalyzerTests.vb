Imports Microsoft.VisualStudio.TestTools.UnitTesting

<TestClass>
Public Class RefStructBCX36640AnalyzerTests

    Private Shared Sub AssertThatShouldHaveError(snippetContent As String, source As String)
        With GetSyntaxTreeTextAndDiagnostics(source, snippetContent)
            Assert.IsTrue(ContainsDiagnostic(.diagnostics, "BCX36640"), $"应该检测到 BCX36640 诊断。语法树内容:  {vbCrLf}{ .syntaxTreeText}")
        End With
    End Sub

    Private Shared Sub AssertThatShouldNotHaveError(snippetContent As String, source As String)
        With GetSyntaxTreeTextAndDiagnostics(source, snippetContent)
            Assert.IsFalse(ContainsDiagnostic(.diagnostics, "BCX36640"), $"不应该检测到 BCX36640 诊断。语法树内容:  {vbCrLf}{ .syntaxTreeText}")
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
    ' Lambda 表达式中捕获受限类型测试
    ' ====================

    <TestMethod>
    Public Sub TestRestrictedTypeInLambdaExpression()
        Dim snippetContent = "Dim action As Action = Sub() Console.WriteLine(span.Length)"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    <TestMethod>
    Public Sub TestRestrictedTypeInLambdaFunction()
        Dim snippetContent = "Dim func As Func(Of Integer) = Function() span.Length"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    <TestMethod>
    Public Sub TestRestrictedTypeInLambdaWithClosure()
        Dim snippetContent = "Dim items = arr.Select(Function(x) span)"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    <TestMethod>
    Public Sub TestRestrictedTypeInForEachLambda()
        Dim snippetContent = "arr.ToList().ForEach(Sub(item) Console.WriteLine(span.Length))"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    <TestMethod>
    Public Sub TestRestrictedTypeInWhereLambda()
        Dim snippetContent = "Dim result = arr.Where(Function(x) span.Length > 0).ToArray()"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    <TestMethod>
    Public Sub TestRestrictedTypeInSelectLambda()
        Dim snippetContent = "Dim result = arr.Select(Function(x) span).ToArray()"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    <TestMethod>
    Public Sub TestRestrictedTypeInAggregateLambda()
        Dim snippetContent = "Dim result = arr.Aggregate(Function(acc, x) span)"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    <TestMethod>
    Public Sub TestRestrictedTypeInOrderByLambda()
        Dim snippetContent = "Dim result = arr.OrderBy(Function(x) span.Length).ToArray()"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    <TestMethod>
    Public Sub TestRestrictedTypeInGroupByLambda()
        Dim snippetContent = "Dim result = arr.GroupBy(Function(x) span.Length).ToArray()"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    <TestMethod>
    Public Sub TestRestrictedTypeInJoinLambda()
        Dim snippetContent = "Dim result = arr.Join(arr, Function(x) span.Length, Function(y) y, Function(x, y) x).ToArray()"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    ' ====================
    ' 多层 Lambda 嵌套测试
    ' ====================

    <TestMethod>
    Public Sub TestRestrictedTypeInNestedLambda()
        Dim snippetContent = "Dim func As Func(Of Func(Of Integer)) = Function() Function() span.Length"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    <TestMethod>
    Public Sub TestRestrictedTypeInLambdaChain()
        Dim snippetContent = "Dim result = arr.Select(Function(x) x).Where(Function(x) span.Length > 0).ToArray()"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    ' ====================
    ' Async Lambda 测试
    ' ====================

    <TestMethod>
    Public Sub TestRestrictedTypeInAsyncLambda()
        Dim snippetContent = "Dim task = Task.Run(Function() span.Length)"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    ' ====================
    ' 正确用法测试
    ' ====================

    <TestMethod>
    Public Sub TestNormalLambdaUsage()
        Dim source As String = "
Imports System
Imports System.Linq

Class TestClass
    Sub TestMethod()
        Dim arr As Integer() = {1, 2, 3, 4, 5}
        Dim action As Action = Sub() Console.WriteLine(""Hello"")
        Dim func As Func(Of Integer, Integer) = Function(x) x * 2
        Dim result = arr.Select(Function(x) x * 2).ToArray()
    End Sub
End Class
"
        AssertThatShouldNotHaveError(source, source)
    End Sub

    <TestMethod>
    Public Sub TestLambdaWithNormalTypes()
        Dim source As String = "
Imports System
Imports System.Linq

Class TestClass
    Sub TestMethod()
        Dim numbers = {1, 2, 3, 4, 5}
        Dim strings = {""hello"", ""world""}
        Dim result = numbers.Select(Function(n) n.ToString()).ToArray()
        Dim query = strings.Where(Function(s) s.Length > 3).ToArray()
    End Sub
End Class
"
        AssertThatShouldNotHaveError(source, source)
    End Sub

End Class

Imports Microsoft.VisualStudio.TestTools.UnitTesting

<TestClass>
Public Class RefStructBCX36640AnalyzerTests

    Private Shared Sub AssertThatShouldHaveError(source As FormattableString)
        With GetSyntaxTreeTextAndDiagnostics(source)
            Assert.IsTrue(ContainsDiagnostic(.diagnostics, "BCX36640"), $"Should detect BCX36640 diagnostic. Syntax tree content:  {vbCrLf}{ .syntaxTreeText}")
        End With
    End Sub

    Private Shared Sub AssertThatShouldNotHaveError(source As FormattableString)
        With GetSyntaxTreeTextAndDiagnostics(source)
            Assert.IsFalse(ContainsDiagnostic(.diagnostics, "BCX36640"), $"Should not detect BCX36640 diagnostic. Syntax tree content:  {vbCrLf}{ .syntaxTreeText}")
        End With
    End Sub

    Private Shared Sub AssertThatDiagTriggeredInSub(snippetContent As String)
        Dim source As FormattableString = $"
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
        AssertThatShouldHaveError(source)
    End Sub

    ' ====================
    ' Capture restricted types in Lambda expressions test
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
    ' Capture restricted types in multi-line Lambda expressions test
    ' ====================

    <TestMethod>
    Public Sub TestRestrictedTypeInMultiLineLambdaExpression()
        Dim snippetContent = "Dim action As Action = Sub()
        Console.WriteLine(span.Length)
    End Sub"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    <TestMethod>
    Public Sub TestRestrictedTypeInMultiLineLambdaFunction()
        Dim snippetContent = "Dim func As Func(Of Integer) = Function()
        Return span.Length
    End Function"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    <TestMethod>
    Public Sub TestRestrictedTypeInMultiLineLambdaWithMultipleStatements()
        Dim snippetContent = "Dim action As Action = Sub()
        Console.WriteLine()
        Dim x = span.Length
    End Sub"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    <TestMethod>
    Public Sub TestRestrictedTypeInMultiLineLambdaChain()
        Dim snippetContent = "Dim result = arr.Select(Function(x)
        Return x * span.Length
    End Function).ToArray()"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    <TestMethod>
    Public Sub TestRestrictedTypeInMultiLineNestedLambda()
        Dim snippetContent = "Dim func As Func(Of Func(Of Integer)) = Function()
        Return Function()
            Return span.Length
        End Function
    End Function"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    ' ====================
    ' Normal local Span variable declaration inside multi-line Lambda test
    ' ====================

    <TestMethod>
    Public Sub TestNormalLocalSpanInMultiLineLambda()
        Dim source As FormattableString = $"
Imports System
Imports System.Runtime.InteropServices
Imports System.Linq

Class TestClass
    Sub TestMethod()
        Dim arr As Integer() = {{1, 2, 3, 4, 5}}
        Dim action As Action = Sub()
            Dim localArr As Integer() = {{6, 7, 8, 9, 10}}
            Dim localSpan As Span(Of Integer) = localArr.AsSpan()
            Console.WriteLine(localSpan.Length)
        End Sub
    End Sub
End Class
"
        AssertThatShouldNotHaveError(source)
    End Sub

    <TestMethod>
    Public Sub TestNormalLocalSpanInMultiLineLambdaFunction()
        Dim source As FormattableString = $"
Imports System
Imports System.Runtime.InteropServices
Imports System.Linq

Class TestClass
    Sub TestMethod()
        Dim arr As Integer() = {{1, 2, 3, 4, 5}}
        Dim func As Func(Of Integer) = Function()
            Dim localArr As Integer() = {{6, 7, 8, 9, 10}}
            Dim localSpan As Span(Of Integer) = localArr.AsSpan()
            Return localSpan.Length
        End Function
    End Sub
End Class
"
        AssertThatShouldNotHaveError(source)
    End Sub

    <TestMethod>
    Public Sub TestNormalLocalSpanWithMultipleOperationsInLambda()
        Dim source As FormattableString = $"
Imports System
Imports System.Runtime.InteropServices
Imports System.Linq

Class TestClass
    Sub TestMethod()
        Dim arr As Integer() = {{1, 2, 3, 4, 5}}
        Dim action As Action = Sub()
            Dim localArr As Integer() = {{6, 7, 8, 9, 10}}
            Dim localSpan As Span(Of Integer) = localArr.AsSpan()
            Dim slice = localSpan.Slice(1, 2)
            Dim copy = localSpan.ToArray()
            Console.WriteLine(slice.Length)
        End Sub
    End Sub
End Class
"
        AssertThatShouldNotHaveError(source)
    End Sub

    <TestMethod>
    Public Sub TestNormalLocalSpanInLinqMultiLineLambda()
        Dim source As FormattableString = $"
Imports System
Imports System.Runtime.InteropServices
Imports System.Linq

Class TestClass
    Sub TestMethod()
        Dim arr As Integer() = {{1, 2, 3, 4, 5}}
        Dim result = arr.Select(Function(x)
            Dim localSpan As Span(Of Integer) = arr.AsSpan()
            Return localSpan.Length
        End Function).ToArray()
    End Sub
End Class
"
        AssertThatShouldNotHaveError(source)
    End Sub

    ' ====================
    ' Multi-layer Lambda nesting test
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
    ' Async Lambda test
    ' ====================

    <TestMethod>
    Public Sub TestRestrictedTypeInAsyncLambda()
        Dim snippetContent = "Dim task = Task.Run(Function() span.Length)"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    ' ====================
    ' Correct usage test
    ' ====================

    <TestMethod>
    Public Sub TestNormalLambdaUsage()
        Dim snippet = "Dim arr As Integer() = {1, 2, 3, 4, 5}
Dim action As Action = Sub() Console.WriteLine(""Hello"")
Dim func As Func(Of Integer, Integer) = Function(x) x * 2
Dim result = arr.Select(Function(x) x * 2).ToArray()"
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
    Public Sub TestLambdaWithNormalTypes()
        Dim snippet = "Dim numbers = {1, 2, 3, 4, 5}
Dim strings = {""hello"", ""world""}
Dim result = numbers.Select(Function(n) n.ToString()).ToArray()
Dim query = strings.Where(Function(s) s.Length > 3).ToArray()"
        AssertThatCorrectInMethod(snippet)
    End Sub

End Class

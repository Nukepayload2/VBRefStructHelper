Imports Microsoft.VisualStudio.TestTools.UnitTesting

<TestClass>
Public Class RefStructBCX37052AnalyzerTests

    Private Shared Sub AssertThatShouldHaveError(source As FormattableString)
        With GetSyntaxTreeTextAndDiagnostics(source)
            Assert.IsTrue(ContainsDiagnostic(.diagnostics, "BCX37052"), $"应该检测到 BCX37052 诊断。语法树内容:  {vbCrLf}{ .syntaxTreeText}")
        End With
    End Sub

    Private Shared Sub AssertThatShouldNotHaveError(source As FormattableString)
        With GetSyntaxTreeTextAndDiagnostics(source)
            Assert.IsFalse(ContainsDiagnostic(.diagnostics, "BCX37052"), $"不应该检测到 BCX37052 诊断。语法树内容:  {vbCrLf}{ .syntaxTreeText}")
        End With
    End Sub

    ' ====================
    ' Async 方法中声明受限类型变量测试
    ' ====================

    <TestMethod>
    Public Sub TestRestrictedTypeInAsyncMethod()
        Dim source As FormattableString = $"
Imports System
Imports System.Runtime.InteropServices
Imports System.Threading.Tasks

<Obsolete(""Suppress default ref struct obsolete errors"")>
Class TestClass
    Async Function TestMethod() As Task
        Dim arr As Integer() = {{1, 2, 3, 4, 5}}
        Dim span As Span(Of Integer) = arr.AsSpan()  ' 这应该触发 BCX37052
        Await Task.Delay(100)
    End Function
End Class
"
        AssertThatShouldHaveError(source)
    End Sub

    <TestMethod>
    Public Sub TestRestrictedTypeInAsyncMethod2()
        Dim source As FormattableString = $"
Imports System
Imports System.Runtime.InteropServices
Imports System.Threading.Tasks

<Obsolete(""Suppress default ref struct obsolete errors"")>
Class TestClass
    Async Function TestMethod() As Task
        Dim arr As Integer() = {{1, 2, 3, 4, 5}}
        Dim span = arr.AsSpan()  ' 这应该触发 BCX37052
        Await Task.Delay(100)
    End Function
End Class
"
        AssertThatShouldHaveError(source)
    End Sub

    <TestMethod>
    Public Sub TestRestrictedTypeInAsyncMethod3()
        Dim source As FormattableString = $"
Imports System
Imports System.Runtime.InteropServices
Imports System.Threading.Tasks

<Obsolete(""Suppress default ref struct obsolete errors"")>
Class TestClass
    Async Sub TestMethod()
        Dim arr As Integer() = {{1, 2, 3, 4, 5}}
        Dim span = arr.AsSpan()  ' 这应该触发 BCX37052
        Await Task.Delay(100)
    End Sub
End Class
"
        AssertThatShouldHaveError(source)
    End Sub

    ' ====================
    ' Iterator 方法中声明受限类型变量测试
    ' ====================

    <TestMethod>
    Public Sub TestRestrictedTypeInIteratorMethod()
        Dim source As FormattableString = $"
Imports System
Imports System.Runtime.InteropServices
Imports System.Collections.Generic

<Obsolete(""Suppress default ref struct obsolete errors"")>
Class TestClass
    Iterator Function TestMethod() As IEnumerable(Of Integer)
        Dim arr As Integer() = {{1, 2, 3, 4, 5}}
        Dim span As Span(Of Integer) = arr.AsSpan()  ' 这应该触发 BCX37052
        Yield span.Length
    End Function
End Class
"
        AssertThatShouldHaveError(source)
    End Sub

    <TestMethod>
    Public Sub TestRestrictedTypeInIteratorMethod2()
        Dim source As FormattableString = $"
Imports System
Imports System.Runtime.InteropServices
Imports System.Collections.Generic

<Obsolete(""Suppress default ref struct obsolete errors"")>
Class TestClass
    Iterator Function TestMethod() As IEnumerable(Of Integer)
        Dim arr As Integer() = {{1, 2, 3, 4, 5}}
        Dim span = arr.AsSpan()  ' 这应该触发 BCX37052
        Yield span.Length
    End Function
End Class
"
        AssertThatShouldHaveError(source)
    End Sub

    ' ====================
    ' 参数/连续调用中的受限类型测试
    ' ====================

    <TestMethod>
    Public Sub TestRestrictedTypeAsParameterInAsyncMethod()
        Dim source As FormattableString = $"
Imports System
Imports System.Runtime.InteropServices
Imports System.Threading.Tasks

<Obsolete(""Suppress default ref struct obsolete errors"")>
Class TestClass
    Async Function TestMethod(span As Span(Of Integer)) As Task(Of Integer)
        Await Task.Delay(100)
        Return span.Length
    End Function
End Class
"
        ' 参数进入闭包，并非内存安全
        AssertThatShouldHaveError(source)
    End Sub

    <TestMethod>
    Public Sub TestRestrictedTypeAsCombinedCallInAsyncMethod()
        Dim source As FormattableString = $"
Imports System
Imports System.Runtime.InteropServices
Imports System.Threading.Tasks

<Obsolete(""Suppress default ref struct obsolete errors"")>
Class TestClass
    Async Function TestMethod() As Task(Of Integer)
        Dim x = {{1,2,3}}
        Await Task.Delay(100)
        Return x.AsSpan().Length
    End Function
End Class
"
        ' 连续调用允许，因为没有产生 Span 闭包
        AssertThatShouldNotHaveError(source)
    End Sub

    <TestMethod>
    Public Sub TestRestrictedTypeAsParameterInIteratorMethod()
        Dim source As FormattableString = $"
Imports System
Imports System.Runtime.InteropServices
Imports System.Collections.Generic

<Obsolete(""Suppress default ref struct obsolete errors"")>
Class TestClass
    Iterator Function TestMethod(span As Span(Of Integer)) As IEnumerable(Of Integer)
        Yield span.Length
    End Function
End Class
"
        ' 参数进入闭包，并非内存安全
        AssertThatShouldHaveError(source)
    End Sub

    <TestMethod>
    Public Sub TestRestrictedTypeAsCombinedCallInIteratorMethod()
        Dim source As FormattableString = $"
Imports System
Imports System.Runtime.InteropServices
Imports System.Collections.Generic

<Obsolete(""Suppress default ref struct obsolete errors"")>
Class TestClass
    Iterator Function TestMethod() As IEnumerable(Of Integer)
        Dim x = {{1,2,3}}
        Await Task.Delay(100)
        Yield x.AsSpan().Length
    End Function
End Class
"
        ' 连续调用允许，因为没有产生 Span 闭包
        AssertThatShouldNotHaveError(source)
    End Sub

    ' ====================
    ' 误伤测试
    ' ====================

    <TestMethod>
    Public Sub TestNormalAsyncMethod()
        Dim snippet = "Dim x As Integer = 42
Await Task.Delay(100)"
        AssertThatCorrectInAsyncMethod(snippet)
    End Sub

    Private Shared Sub AssertThatCorrectInAsyncMethod(snippet As String)
        Dim source As FormattableString = $"
Imports System
Imports System.Threading.Tasks

Class TestClass
    Async Function TestMethod() As Task
{snippet}
    End Function
End Class
"
        AssertThatShouldNotHaveError(source)
    End Sub

    <TestMethod>
    Public Sub TestNormalIteratorMethod()
        Dim source As FormattableString = $"
Imports System
Imports System.Collections.Generic

Class TestClass
    Iterator Function TestMethod() As IEnumerable(Of Integer)
        Dim x As Integer = 42
        Yield x
    End Function
End Class
"
        AssertThatShouldNotHaveError(source)
    End Sub

    <TestMethod>
    Public Sub TestNormalAsyncIteratorMethod()
        Dim source As FormattableString = $"
Imports System
Imports System.Collections.Generic
Imports System.Threading.Tasks

Class TestClass
    Async Iterator Function TestMethod() As IAsyncEnumerable(Of Integer)
        Dim x As Integer = 42
        Yield x
        Await Task.Delay(100)
    End Function
End Class
"
        AssertThatShouldNotHaveError(source)
    End Sub

End Class

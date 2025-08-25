Imports Microsoft.VisualStudio.TestTools.UnitTesting

<TestClass>
Public Class RefStructBCX37052AnalyzerTests

    Private Shared Sub AssertThatShouldHaveError(snippetContent As String, source As String)
        With GetSyntaxTreeTextAndDiagnostics(source, snippetContent)
            Assert.IsTrue(ContainsDiagnostic(.diagnostics, "BCX37052"), $"应该检测到 BCX37052 诊断。语法树内容:  {vbCrLf}{ .syntaxTreeText}")
        End With
    End Sub

    Private Shared Sub AssertThatShouldNotHaveError(snippetContent As String, source As String)
        With GetSyntaxTreeTextAndDiagnostics(source, snippetContent)
            Assert.IsFalse(ContainsDiagnostic(.diagnostics, "BCX37052"), $"不应该检测到 BCX37052 诊断。语法树内容:  {vbCrLf}{ .syntaxTreeText}")
        End With
    End Sub

    ' ====================
    ' Async 方法中声明受限类型变量测试
    ' ====================

    <TestMethod>
    Public Sub TestRestrictedTypeInAsyncMethod()
        Dim source As String = "
Imports System
Imports System.Runtime.InteropServices
Imports System.Threading.Tasks

<Obsolete(""Suppress default ref struct obsolete errors"")>
Class TestClass
    Async Function TestMethod() As Task
        Dim arr As Integer() = {1, 2, 3, 4, 5}
        Dim span As Span(Of Integer) = arr.AsSpan()  ' 这应该触发 BCX37052
        Await Task.Delay(100)
    End Function
End Class
"
        AssertThatShouldHaveError(source, source)
    End Sub

    <TestMethod>
    Public Sub TestRestrictedTypeInAsyncMethod2()
        Dim source As String = "
Imports System
Imports System.Runtime.InteropServices
Imports System.Threading.Tasks

<Obsolete(""Suppress default ref struct obsolete errors"")>
Class TestClass
    Async Function TestMethod() As Task(Of Integer)
        Dim span As Span(Of Integer) = stackalloc Integer(10)  ' 这应该触发 BCX37052
        Await Task.Delay(100)
        Return span.Length
    End Function
End Class
"
        AssertThatShouldHaveError(source, source)
    End Sub

    <TestMethod>
    Public Sub TestRestrictedTypeInAsyncMethodLocalVar()
        Dim source As String = "
Imports System
Imports System.Runtime.InteropServices
Imports System.Threading.Tasks

<Obsolete(""Suppress default ref struct obsolete errors"")>
Class TestClass
    Async Function TestMethod() As Task
        Dim localSpan As Span(Of Integer) = Span(Of Integer).Empty  ' 这应该触发 BCX37052
        Await Task.Delay(100)
    End Function
End Class
"
        AssertThatShouldHaveError(source, source)
    End Sub

    ' ====================
    ' Iterator 方法中声明受限类型变量测试
    ' ====================

    <TestMethod>
    Public Sub TestRestrictedTypeInIteratorMethod()
        Dim source As String = "
Imports System
Imports System.Runtime.InteropServices
Imports System.Collections.Generic

<Obsolete(""Suppress default ref struct obsolete errors"")>
Class TestClass
    Iterator Function TestMethod() As IEnumerable(Of Integer)
        Dim arr As Integer() = {1, 2, 3, 4, 5}
        Dim span As Span(Of Integer) = arr.AsSpan()  ' 这应该触发 BCX37052
        Yield span.Length
    End Function
End Class
"
        AssertThatShouldHaveError(source, source)
    End Sub

    <TestMethod>
    Public Sub TestRestrictedTypeInIteratorMethod2()
        Dim source As String = "
Imports System
Imports System.Runtime.InteropServices
Imports System.Collections.Generic

<Obsolete(""Suppress default ref struct obsolete errors"")>
Class TestClass
    Iterator Function TestMethod() As IEnumerator(Of Integer)
        Dim span As Span(Of Integer) = stackalloc Integer(5)  ' 这应该触发 BCX37052
        Yield span.Length
    End Function
End Class
"
        AssertThatShouldHaveError(source, source)
    End Sub

    <TestMethod>
    Public Sub TestRestrictedTypeInIteratorLocalVar()
        Dim source As String = "
Imports System
Imports System.Runtime.InteropServices
Imports System.Collections.Generic

<Obsolete(""Suppress default ref struct obsolete errors"")>
Class TestClass
    Iterator Function TestMethod() As IEnumerable(Of String)
        Dim localSpan As Span(Of Integer) = Span(Of Integer).Empty  ' 这应该触发 BCX37052
        Yield localSpan.Length.ToString()
    End Function
End Class
"
        AssertThatShouldHaveError(source, source)
    End Sub

    ' ====================
    ' Async Iterator 方法测试
    ' ====================

    <TestMethod>
    Public Sub TestRestrictedTypeInAsyncIteratorMethod()
        Dim source As String = "
Imports System
Imports System.Runtime.InteropServices
Imports System.Collections.Generic
Imports System.Threading.Tasks

<Obsolete(""Suppress default ref struct obsolete errors"")>
Class TestClass
    Async Iterator Function TestMethod() As IAsyncEnumerable(Of Integer)
        Dim arr As Integer() = {1, 2, 3, 4, 5}
        Dim span As Span(Of Integer) = arr.AsSpan()  ' 这应该触发 BCX37052
        Yield span.Length
        Await Task.Delay(100)
    End Function
End Class
"
        AssertThatShouldHaveError(source, source)
    End Sub

    ' ====================
    ' 参数中的受限类型测试（不应该触发）
    ' ====================

    <TestMethod>
    Public Sub TestRestrictedTypeAsParameterInAsyncMethod()
        Dim source As String = "
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
        AssertThatShouldNotHaveError(source, source)
    End Sub

    <TestMethod>
    Public Sub TestRestrictedTypeAsParameterInIteratorMethod()
        Dim source As String = "
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
        AssertThatShouldNotHaveError(source, source)
    End Sub

    ' ====================
    ' 正确用法测试
    ' ====================

    <TestMethod>
    Public Sub TestNormalAsyncMethod()
        Dim source As String = "
Imports System
Imports System.Threading.Tasks

Class TestClass
    Async Function TestMethod() As Task
        Dim x As Integer = 42
        Await Task.Delay(100)
    End Function
End Class
"
        AssertThatShouldNotHaveError(source, source)
    End Sub

    <TestMethod>
    Public Sub TestNormalIteratorMethod()
        Dim source As String = "
Imports System
Imports System.Collections.Generic

Class TestClass
    Iterator Function TestMethod() As IEnumerable(Of Integer)
        Dim x As Integer = 42
        Yield x
    End Function
End Class
"
        AssertThatShouldNotHaveError(source, source)
    End Sub

    <TestMethod>
    Public Sub TestNormalAsyncIteratorMethod()
        Dim source As String = "
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
        AssertThatShouldNotHaveError(source, source)
    End Sub

End Class

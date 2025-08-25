Imports Microsoft.VisualStudio.TestTools.UnitTesting

<TestClass>
Public Class RefStructBCX32061AnalyzerTests

    Private Shared Sub AssertThatShouldHaveError(snippetContent As String, source As String)
        With GetSyntaxTreeTextAndDiagnostics(source, snippetContent)
            Assert.IsTrue(ContainsDiagnostic(.diagnostics, "BCX32061"), $"应该检测到 BCX32061 诊断。语法树内容:  {vbCrLf}{ .syntaxTreeText}")
        End With
    End Sub

    Private Shared Sub AssertThatShouldNotHaveError(snippetContent As String, source As String)
        With GetSyntaxTreeTextAndDiagnostics(source, snippetContent)
            Assert.IsFalse(ContainsDiagnostic(.diagnostics, "BCX32061"), $"不应该检测到 BCX32061 诊断。语法树内容:  {vbCrLf}{ .syntaxTreeText}")
        End With
    End Sub

    Private Shared Sub AssertThatDiagTriggeredInClass(snippetContent As String)
        Dim source As String = $"
Imports System
Imports System.Runtime.InteropServices

<Obsolete(""Suppress default ref struct obsolete errors"")>
Class TestClass
    {snippetContent}
End Class
"
        AssertThatShouldHaveError(snippetContent, source)
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
    ' 泛型类型参数测试
    ' ====================

    <TestMethod>
    Public Sub TestRestrictedTypeAsGenericParameter()
        Dim snippetContent = "Dim list As List(Of Span(Of Integer)) = New List(Of Span(Of Integer))()"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    <TestMethod>
    Public Sub TestRestrictedTypeAsGenericParameter2()
        Dim snippetContent = "Dim dict As Dictionary(Of String, Span(Of Integer)) = New Dictionary(Of String, Span(Of Integer))()"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    <TestMethod>
    Public Sub TestRestrictedTypeAsGenericParameter3()
        Dim snippetContent = "Dim nullableDict As Dictionary(Of Span(Of Integer)?, String) = New Dictionary(Of Span(Of Integer)?, String)()"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    <TestMethod>
    Public Sub TestRestrictedTypeAsGenericParameterInClass()
        Dim source As String = "
Imports System
Imports System.Runtime.InteropServices
Imports System.Collections.Generic

<Obsolete(""Suppress default ref struct obsolete errors"")>
Class TestClass
    Private field As List(Of Span(Of Integer))
End Class
"
        AssertThatShouldHaveError(source, source)
    End Sub

    <TestMethod>
    Public Sub TestRestrictedTypeAsGenericParameterInProperty()
        Dim source As String = "
Imports System
Imports System.Runtime.InteropServices
Imports System.Collections.Generic

<Obsolete(""Suppress default ref struct obsolete errors"")>
Class TestClass
    Public Property MyProperty As List(Of Span(Of Integer))
End Class
"
        AssertThatShouldHaveError(source, source)
    End Sub

    <TestMethod>
    Public Sub TestRestrictedTypeAsGenericParameterInMethod()
        Dim source As String = "
Imports System
Imports System.Runtime.InteropServices
Imports System.Collections.Generic

<Obsolete(""Suppress default ref struct obsolete errors"")>
Class TestClass
    Sub TestMethod(param As List(Of Span(Of Integer)))
    End Sub
End Class
"
        AssertThatShouldHaveError(source, source)
    End Sub

    <TestMethod>
    Public Sub TestRestrictedTypeAsGenericParameterInFunction()
        Dim source As String = "
Imports System
Imports System.Runtime.InteropServices
Imports System.Collections.Generic

<Obsolete(""Suppress default ref struct obsolete errors"")>
Class TestClass
    Function TestFunction() As List(Of Span(Of Integer))
        Return Nothing
    End Function
End Class
"
        AssertThatShouldHaveError(source, source)
    End Sub

    <TestMethod>
    Public Sub TestRestrictedTypeAsGenericParameterInDelegate()
        Dim source As String = "
Imports System
Imports System.Runtime.InteropServices
Imports System.Collections.Generic

<Obsolete(""Suppress default ref struct obsolete errors"")>
Class TestClass
    Delegate Sub TestDelegate(param As List(Of Span(Of Integer)))
End Class
"
        AssertThatShouldHaveError(source, source)
    End Sub

    ' ====================
    ' 泛型约束测试
    ' ====================

    <TestMethod>
    Public Sub TestRestrictedTypeAsTypeConstraint()
        Dim source As String = "
Imports System
Imports System.Runtime.InteropServices

<Obsolete(""Suppress default ref struct obsolete errors"")>
Class TestClass
    Sub TestMethod(Of T As Span(Of Integer))()
    End Sub
End Class
"
        AssertThatShouldHaveError(source, source)
    End Sub

    <TestMethod>
    Public Sub TestRestrictedTypeAsTypeConstraint2()
        Dim source As String = "
Imports System
Imports System.Runtime.InteropServices

<Obsolete(""Suppress default ref struct obsolete errors"")>
Class TestClass
    Sub TestMethod(Of T As {Span(Of Integer), IDisposable})()
    End Sub
End Class
"
        AssertThatShouldHaveError(source, source)
    End Sub

    ' ====================
    ' 正确用法测试
    ' ====================

    <TestMethod>
    Public Sub TestNormalGenericUsage()
        Dim source As String = "
Imports System
Imports System.Collections.Generic

Class TestClass
    Sub TestMethod()
        Dim list As List(Of Integer) = New List(Of Integer)()
        Dim dict As Dictionary(Of String, Integer) = New Dictionary(Of String, Integer)()
    End Sub
End Class
"
        AssertThatShouldNotHaveError(source, source)
    End Sub

    <TestMethod>
    Public Sub TestNormalGenericConstraint()
        Dim source As String = "
Imports System

Class TestClass
    Sub TestMethod(Of T As IDisposable)()
    End Sub
End Class
"
        AssertThatShouldNotHaveError(source, source)
    End Sub

End Class

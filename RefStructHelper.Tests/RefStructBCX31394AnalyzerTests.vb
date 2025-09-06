Imports Microsoft.VisualStudio.TestTools.UnitTesting

<TestClass>
Public Class RefStructBCX31394AnalyzerTests

    Private Shared Sub AssertThatShouldHaveError(source As FormattableString)
        With GetSyntaxTreeTextAndDiagnostics(source)
            Assert.IsTrue(ContainsDiagnostic(.diagnostics, "BCX31394"), $"Should detect BCX31394 diagnostic. Syntax tree content: {vbCrLf}{ .syntaxTreeText}")
        End With
    End Sub

    Private Shared Sub AssertThatShouldNotHaveError(source As FormattableString)
        With GetSyntaxTreeTextAndDiagnostics(source)
            Assert.IsFalse(ContainsDiagnostic(.diagnostics, "BCX31394"), $"Should not detect BCX31394 diagnostic. Syntax tree content: {vbCrLf}{ .syntaxTreeText}")
        End With
    End Sub

    Private Shared Sub AssertThatDiagTriggeredInSub(snippetContent As String)
        Dim source As FormattableString = $"
Imports System
Imports System.Runtime.InteropServices

<Obsolete(""Suppress default ref struct obsolete errors"")>
Class TestClass

    ' Helper method
    Sub TestMethodTakingObject(obj As Object)
        ' This method is used to test parameter passing scenarios
    End Sub

    ' Helper method
    Sub TestMethodTakingValueType(obj As ValueType)
        ' This method is used to test parameter passing scenarios
    End Sub

    Private Class Something
        Sub New()
        End Sub
        Sub New(arg As Object)
        End Sub
        Public Property SomeValue As Object
    End Class

    Private Event SomeEvent(arg As Object)

    Sub TestMethod()
        Dim arr As Integer() = {{1, 2, 3, 4, 5}}
        Dim span As Span(Of Integer) = arr.AsSpan()
        {snippetContent}
    End Sub
End Class
"
        AssertThatShouldHaveError(source)
    End Sub

    <TestMethod>
    Public Sub TestSpanToObjectAssignment()
        Dim snippetContent = "Dim obj As Object = span"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    ' Use CType to convert Span to Object
    <TestMethod>
    Public Sub TestCTypeSpanToObject()
        Dim snippetContent = "Dim obj As Object = CType(span, Object)"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    ' Use DirectCast to convert Span to Object
    <TestMethod>
    Public Sub TestDirectCastSpanToObject()
        Dim snippetContent = "Dim obj As Object = DirectCast(span, Object)"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    <TestMethod, Ignore("Overlaps with Option Strict, no need to implement")>
    Public Sub TestRestrictedTypeEqualsOperatorObject()
        Dim snippetContent = "Dim wrong = span = CObj(0)"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    ' Use equals operator to compare span with ValueType
    <TestMethod, Ignore("Overlaps with Option Strict, no need to implement")>
    Public Sub TestRestrictedTypeEqualsOperatorValueType()
        Dim snippetContent = "Dim wrong = span = CType(Nothing, ValueType)"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    ' Assign Span to ValueType variable
    <TestMethod>
    Public Sub TestSpanToValueTypeAssignment()
        Dim snippetContent = "Dim valueType As ValueType = span"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    ' Use CType to convert Span to ValueType
    <TestMethod>
    Public Sub TestCTypeSpanToValueType()
        Dim snippetContent = "Dim valueType As ValueType = CType(span, ValueType)"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    ' Use CObj to convert Span to Object
    <TestMethod>
    Public Sub TestCObjSpanToObject()
        Dim snippetContent = "Dim obj = CObj(span)"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    ' Use TryCast to convert Span to Object
    <TestMethod>
    Public Sub TestTryCastSpanToObject()
        Dim snippetContent = "Dim obj As Object = TryCast(span, Object)"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    ' Use TryCast to convert Span to ValueType
    <TestMethod>
    Public Sub TestTryCastSpanToValueType()
        Dim snippetContent = "Dim valueType As ValueType = TryCast(span, ValueType)"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    ' Use DirectCast to convert Span to ValueType
    <TestMethod>
    Public Sub TestDirectCastSpanToValueType()
        Dim snippetContent = "Dim valueType As ValueType = DirectCast(span, ValueType)"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    ' Assign Span to ValueType variable (direct assignment)
    <TestMethod>
    Public Sub TestSpanToValueTypeAssignmentDirect()
        Dim snippetContent = "Dim valueType As ValueType = span"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    ' Pass Span as Object parameter
    <TestMethod>
    Public Sub TestSpanAsObjectParameter()
        Dim snippetContent = "TestMethodTakingObject(span)"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    ' Pass Span as ValueType parameter
    <TestMethod>
    Public Sub TestSpanAsValueTypeParameter()
        Dim snippetContent = "TestMethodTakingValueType(span)"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    <TestMethod>
    Public Sub TestEventRaiseWithSpan()
        Dim snippetContent = "RaiseEvent SomeEvent(span)"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    <TestMethod>
    Public Sub TestNewSomethingWithSpan()
        Dim snippetContent = "Dim x As New Something(span)"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    <TestMethod>
    Public Sub TestNewSomethingWithSpanImplicit()
        Dim snippetContent = "Dim y = New Something(span)"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    <TestMethod>
    Public Sub TestNewSomethingWithSpanWithExpr()
        Dim snippetContent = "Dim sth As New Something With {.SomeValue = span}"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    ' Return Span as Object from function
    <TestMethod>
    Public Sub TestReturnSpanAsObject()
        Dim source As FormattableString = $"
Imports System
Imports System.Runtime.InteropServices

<Obsolete(""Suppress default ref struct obsolete errors"")>
Class TestClass
    Function TestReturnSpanAsObject() As Object
        Dim arr As Integer() = {{1, 2, 3, 4, 5}}
        Dim span As Span(Of Integer) = arr.AsSpan()
        Return span
    End Function
End Class
"
        AssertThatShouldHaveError(source)
    End Sub

    ' Return Span as ValueType from function
    <TestMethod>
    Public Sub TestReturnSpanAsValueType()
        Dim source As FormattableString = $"
Imports System
Imports System.Runtime.InteropServices

<Obsolete(""Suppress default ref struct obsolete errors"")>
Class TestClass
    Function TestReturnSpanAsValueType() As ValueType
        Dim arr As Integer() = {{1, 2, 3, 4, 5}}
        Dim span As Span(Of Integer) = arr.AsSpan()
        Return span
    End Function
End Class
"
        AssertThatShouldHaveError(source)
    End Sub

    ' Similar test for ReadOnlySpan
    <TestMethod>
    Public Sub TestReadOnlySpanToObject()
        Dim source As FormattableString = $"
Imports System
Imports System.Runtime.InteropServices

<Obsolete(""Suppress default ref struct obsolete errors"")>
Class TestClass
    Sub TestMethod()
        Dim readOnlySpan As ReadOnlySpan(Of Integer) = {{1, 2, 3, 4, 5}}
        Dim obj As Object = readOnlySpan  ' This should also trigger BCX31394
    End Sub
End Class
"
        AssertThatShouldHaveError(source)
    End Sub

    ' Correct usage should not trigger
    <TestMethod>
    Public Sub TestCorrectUsage()
        Dim snippet = "' Use Span directly without boxing conversion
Dim arr As Integer() = {1, 2, 3, 4, 5}
Dim span As Span(Of Integer) = arr.AsSpan()
span(0) = 10
Dim length As Integer = span.Length
' These are all correct usages and should not trigger"
        AssertThatCorrectInMethod(snippet, True)
    End Sub

    ' Normal code without using Span should not trigger
    <TestMethod>
    Public Sub TestNormalCode()
        Dim snippet = "Dim obj As Object = ""hello""
Dim arr As Object() = {""hello"", 42}
Dim valueType As ValueType = 123"
        AssertThatCorrectInMethod(snippet, False)
    End Sub

    Private Shared Sub AssertThatCorrectInMethod(snippet As String, useRuntimeInterop As Boolean)
        Dim source As FormattableString
        If useRuntimeInterop Then
            source = $"
Imports System
Imports System.Runtime.InteropServices

<Obsolete(""Suppress default ref struct obsolete errors"")>
Class TestClass
    Sub TestMethod()
{snippet}
    End Sub
End Class
"
        Else
            source = $"
Imports System

Class TestClass
    Sub TestMethod()
{snippet}
    End Sub
End Class
"
        End If
        AssertThatShouldNotHaveError(source)
    End Sub

End Class

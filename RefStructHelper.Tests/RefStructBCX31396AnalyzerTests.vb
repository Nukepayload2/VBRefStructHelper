Imports Microsoft.VisualStudio.TestTools.UnitTesting

<TestClass>
Public Class RefStructBCX31396AnalyzerTests

    Private Shared Sub AssertThatShouldHaveError(source As FormattableString)
        With GetSyntaxTreeTextAndDiagnostics(source)
            Assert.IsTrue(ContainsDiagnostic(.diagnostics, "BCX31396"), $"Should detect BCX31396 diagnostic. Syntax tree content:  {vbCrLf}{ .syntaxTreeText}")
        End With
    End Sub

    Private Shared Sub AssertThatShouldNotHaveError(source As FormattableString)
        With GetSyntaxTreeTextAndDiagnostics(source)
            Assert.IsFalse(ContainsDiagnostic(.diagnostics, "BCX31396"), $"Should not detect BCX31396 diagnostic. Syntax tree content:  {vbCrLf}{ .syntaxTreeText}")
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
    ' Nullable restricted type test, including places where As statements can be written and inferred places
    ' ====================

    <TestMethod>
    Public Sub TestNullableSpan()
        Dim snippetContent = "Dim nullableSpan As Span(Of Integer)? = Nothing"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    <TestMethod>
    Public Sub TestNullableSpan2()
        Dim snippetContent = "Dim nullableSpan? As Span(Of Integer) = Nothing"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    ' TODO: Use AssertThatDiagTriggeredInClass to supplement places where As can be used: method parameter declaration, return value declaration, delegate parameter declaration, Sub New parameter declaration, event parameter declaration, event return value declaration, field declaration, property declaration

    <TestMethod>
    Public Sub TestNullableSpanWithInference()
        Dim snippetContent = "Dim nullableSpan? = span"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    ' ====================
    ' Restricted types in arrays test
    ' ====================

    <TestMethod>
    Public Sub TestSpanArray()
        Dim snippetContent = "Dim spanArray As Span(Of Integer)()"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    <TestMethod>
    Public Sub TestSpanArray2()
        Dim snippetContent = "Dim spanArray() As Span(Of Integer)"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    ' ====================
    ' Struct field/property test
    ' ====================

    <TestMethod>
    Public Sub TestStructWithRestrictedField()
        Dim source As FormattableString = $"
Imports System
Imports System.Runtime.InteropServices

<Obsolete(""Suppress default ref struct obsolete errors"")>
Structure TestStruct
    Public RestrictedField As Span(Of Integer)
End Structure
"
        AssertThatShouldHaveError(source)
    End Sub

    <TestMethod>
    Public Sub TestStructWithRestrictedAutoProperty()
        Dim source As FormattableString = $"
Imports System
Imports System.Runtime.InteropServices

<Obsolete(""Suppress default ref struct obsolete errors"")>
Structure TestStruct
    Public Property RestrictedProp As Span(Of Integer)
End Structure
"
        AssertThatShouldHaveError(source)
    End Sub

    <TestMethod>
    Public Sub TestStructWithRestrictedFullProperty()
        Dim source As FormattableString = $"
Imports System
Imports System.Runtime.InteropServices

<Obsolete(""Suppress default ref struct obsolete errors"")>
<System.Runtime.CompilerServices.IsByRefLike>
Structure TestStruct
    Public Property RestrictedProp As Span(Of Integer)
        Get
            Throw New NotImplementedException
        End Get
        Set(value As Span(Of Integer))
            Throw New NotImplementedException
        End Set
    End Property
End Structure
"
        AssertThatShouldHaveError(source)
    End Sub

    <TestMethod>
    Public Sub TestStructWithRestrictedFieldOk()
        Dim source As FormattableString = $"
Imports System
Imports System.Runtime.InteropServices

<Obsolete(""Suppress default ref struct obsolete errors"")>
<System.Runtime.CompilerServices.IsByRefLike>
Structure TestStruct
    Public RestrictedField As Span(Of Integer)
End Structure
"
        AssertThatShouldNotHaveError(source)
    End Sub

    <TestMethod>
    Public Sub TestStructWithRestrictedPropertyOk()
        Dim source As FormattableString = $"
Imports System
Imports System.Runtime.InteropServices

<Obsolete(""Suppress default ref struct obsolete errors"")>
<System.Runtime.CompilerServices.IsByRefLike>
Structure TestStruct
    Public Property RestrictedProp As Span(Of Integer)
End Structure
"
        AssertThatShouldNotHaveError(source)
    End Sub

    <TestMethod>
    Public Sub TestNonRestrictedStruct()
        Dim snippet = "Structure NormalStruct
    Public NormalField As Integer
End Structure

Class TestClass
    Sub TestMethod()
        Dim normalStruct As New NormalStruct()
    End Sub
End Class"
        Dim source As FormattableString = $"
Imports System
Imports System.Runtime.InteropServices

{snippet}
"
        AssertThatShouldNotHaveError(source)
    End Sub

    ' ====================
    ' Class field/property test
    ' ====================

    Private Shared Sub AssertThatDiagTriggeredInClass(snippetContent As String)
        Dim source As FormattableString = $"
Imports System
Imports System.Runtime.InteropServices

<Obsolete(""Suppress default ref struct obsolete errors"")>
Class TestClass
    {snippetContent}
End Class
"
        AssertThatShouldHaveError(source)
    End Sub

    <TestMethod>
    Public Sub TestClassWithRestrictedField()
        Dim source As String = "
Public RestrictedField As Span(Of Integer)
"
        AssertThatDiagTriggeredInClass(source)
    End Sub

    <TestMethod>
    Public Sub TestClassWithRestrictedProperty()
        Dim source As String = "
Public Property RestrictedField As Span(Of Integer)
"
        AssertThatDiagTriggeredInClass(source)
    End Sub

    <TestMethod>
    Public Sub TestClassWithRestrictedFullProperty()
        Dim source As FormattableString = $"
Imports System
Imports System.Runtime.InteropServices

<Obsolete(""Suppress default ref struct obsolete errors"")>
Class TestClassWithRestrictedProperty
    Public Property RestrictedProp As Span(Of Integer)
        Get
            Throw New NotImplementedException
        End Get
        Set(value As Span(Of Integer))
            Throw New NotImplementedException
        End Set
    End Property
End Class
"
        AssertThatShouldHaveError(source)
    End Sub

    <TestMethod>
    Public Sub TestSpanStaticLocal()
        ' Static compiles to fields with thread synchronization code, which belongs to class fields
        Dim snippetContent = "Static somethine As Span(Of Integer) = span"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    ' ====================
    ' Anonymous type member test
    ' ====================

    <TestMethod>
    Public Sub TestAnonymousTypeWithRestrictedMember()
        Dim snippetContent = "Dim anon = New With {.RestrictedValue = span}"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    <TestMethod>
    Public Sub TestAnonymousTypeWithRestrictedMember2()
        Dim snippetContent = "Dim anon = New With {Key .RestrictedValue = span}"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    ' ====================
    ' ByRef parameter test
    ' ====================

    <TestMethod>
    Public Sub TestMethodWithRestrictedByRefParameter()
        Dim source As FormattableString = $"
Imports System
Imports System.Runtime.InteropServices

<Obsolete(""Suppress default ref struct obsolete errors"")>
Class TestClass
    Sub TestMethod(ByRef restrictedParam As Span(Of Integer))
    End Sub
End Class
"
        AssertThatShouldHaveError(source)
    End Sub

    ' ====================
    ' Collection test
    ' ====================

    ' Add Span to Object array
    <TestMethod>
    Public Sub TestSpanInObjectArray()
        Dim snippetContent = "Dim objArray As Object() = {span, ""hello"", 42}"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    <TestMethod>
    Public Sub TestSpanInObjectArray2()
        Dim snippetContent = "Dim objArray = New Object() {span, ""hello"", 42}"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    <TestMethod>
    Public Sub TestSpanInObjectArray3()
        Dim snippetContent = "Dim objArray = {span, ""hello"", 42}"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    ' Add Span to Object array (42, span)
    <TestMethod>
    Public Sub TestSpanInObjectArray4()
        Dim snippetContent = "Dim objArray = {42, span}"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    ' Add Span to Object array (CType(42, Object), span)
    <TestMethod>
    Public Sub TestSpanInObjectArray5()
        Dim snippetContent = "Dim objArray = {CType(42, Object), span}"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    ' Add Span to Object array As Object() = {span}
    <TestMethod>
    Public Sub TestSpanInObjectArray6()
        Dim snippetContent = "Dim objArray As Object() = {span}"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    ' Add Span to Object array () As Object = {span}
    <TestMethod>
    Public Sub TestSpanInObjectArray7()
        Dim snippetContent = "Dim objArray() As Object = {span}"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    <TestMethod>
    Public Sub TestSpanInObjectList()
        Dim snippetContent = "Dim objList4 = New List(Of Object) From {span, ""hello"", 42}"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    <TestMethod>
    Public Sub TestSpanInObjectList2()
        Dim snippetContent = "Dim objList4 As New List(Of Object) From {span, ""hello"", 42}"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    ' Add Span to ValueType array
    <TestMethod>
    Public Sub TestSpanInValueTypeArray()
        Dim snippetContent = "Dim vtArray As ValueType() = {span, 42}"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    ' Add Span to ValueType array (CType(42, ValueType), span)
    <TestMethod>
    Public Sub TestSpanInValueTypeArray2()
        Dim snippetContent = "Dim vtArray = {CType(42, ValueType), span}"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    <TestMethod>
    Public Sub TestSpanInValueTypeArray3()
        Dim snippetContent = "Dim vtArray() As ValueType = {span}"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    <TestMethod>
    Public Sub TestSpanInValueTypeArray4()
        Dim snippetContent = "Dim vtArray As ValueType() = {span}"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    <TestMethod>
    Public Sub TestSpanInValueTypeArray5()
        Dim snippetContent = "Dim vtArray() As ValueType = {span, 42}"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    ' Add Span to ValueType list
    <TestMethod>
    Public Sub TestSpanInValueTypeList()
        Dim snippetContent = "Dim vtList1 = New List(Of ValueType) From {span, 42}"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    <TestMethod>
    Public Sub TestSpanInValueTypeList2()
        Dim snippetContent = "Dim vtList2 As New List(Of ValueType) From {span, 42}"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    ' Add Span to Object dictionary
    <TestMethod>
    Public Sub TestSpanInObjectDictionary()
        Dim snippetContent = "Dim objDict1 = New Dictionary(Of String, Object) From {{""hello"", span}}"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    ' Add Span to Object dictionary (As New Dictionary)
    <TestMethod>
    Public Sub TestSpanInObjectDictionary2()
        Dim snippetContent = "Dim objDict2 As New Dictionary(Of String, Object) From {{""hello"", span}}"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    ' Add Span to ValueType dictionary
    <TestMethod>
    Public Sub TestSpanInValueTypeDictionary()
        Dim snippetContent = "Dim vtDict1 = New Dictionary(Of Integer, ValueType) From {{1, span}}"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    ' Add Span to ValueType dictionary (As New Dictionary)
    <TestMethod>
    Public Sub TestSpanInValueTypeDictionary2()
        Dim snippetContent = "Dim vtDict2 As New Dictionary(Of Integer, ValueType) From {{2, span}}"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    ' ====================
    ' Return value test
    ' ====================

    <TestMethod>
    Public Sub TestMethodReturnRestrictedType()
        Dim source As FormattableString = $"
Imports System
Imports System.Runtime.InteropServices

<Obsolete(""Suppress default ref struct obsolete errors"")>
Class TestClass
    Function TestReturn() As Span(Of Integer) ' Should report error here, return type is restricted type
        Dim arr As Integer() = {{1, 2, 3, 4, 5}}
        Dim span As Span(Of Integer) = arr.AsSpan()
        Return span
    End Function
End Class
"
        AssertThatShouldHaveError(source)
    End Sub

    <TestMethod>
    Public Sub TestOperatorReturnRestrictedType()
        Dim source As FormattableString = $"
Imports System
Imports System.Runtime.InteropServices

<Obsolete(""Suppress default ref struct obsolete errors"")>
Class TestClass

    Shared Narrowing Operator CType(instance As TestClass) As Span(Of Integer) ' This should trigger BCX31396
        Throw New NotSupportedException
    End Operator

End Class
"
        AssertThatShouldHaveError(source)
    End Sub

    ' ====================
    ' False positive test
    ' ====================

    <TestMethod>
    Public Sub TestRegularMembers()
        Dim snippet = "Dim arr As Integer() = {1, 2, 3, 4, 5}
Dim span As Span(Of Integer) = arr.AsSpan()
arr(0) = span.Length
Dim sliced = span.Slice(0,2)"
        AssertThatCorrectInMethod(snippet)
    End Sub

    Private Shared Sub AssertThatCorrectInMethod(snippet As String)
        Dim source As FormattableString = $"
Imports System
Imports System.Runtime.InteropServices

<Obsolete(""Suppress default ref struct obsolete errors"")>
Class TestClass
    Sub TestMethod()
{snippet}
    End Sub
End Class
"
        AssertThatShouldNotHaveError(source)
    End Sub

    Private Shared Sub AssertThatCorrectInClass(snippet As String)
        Dim source As FormattableString = $"
Imports System
Imports System.Runtime.InteropServices

<Obsolete(""Suppress default ref struct obsolete errors"")>
Class TestClass
{snippet}
End Class
"
        AssertThatShouldNotHaveError(source)
    End Sub

    <TestMethod>
    Public Sub TestRegularMembersRegression1()
        Dim snippet = "Private Class Something2
        Sub New(arg As Span(Of Integer))
        End Sub
    End Class

    Sub CorrectUsages()
        Dim y2 = New Something2(span)
    End Sub"
        AssertThatCorrectInClass(snippet)
    End Sub

End Class

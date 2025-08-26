Imports Microsoft.VisualStudio.TestTools.UnitTesting

<TestClass>
Public Class RefStructBCX31396AnalyzerTests

    Private Shared Sub AssertThatShouldHaveError(snippetContent As String, source As String)
        With GetSyntaxTreeTextAndDiagnostics(source, snippetContent)
            Assert.IsTrue(ContainsDiagnostic(.diagnostics, "BCX31396"), $"应该检测到 BCX31396 诊断。语法树内容:  {vbCrLf}{ .syntaxTreeText}")
        End With
    End Sub

    Private Shared Sub AssertThatShouldNotHaveError(snippetContent As String, source As String)
        With GetSyntaxTreeTextAndDiagnostics(source, snippetContent)
            Assert.IsFalse(ContainsDiagnostic(.diagnostics, "BCX31396"), $"应该检测到 BCX31396 诊断。语法树内容:  {vbCrLf}{ .syntaxTreeText}")
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
    ' Nullable 受限类型测试, 包括能写 As 语句的地方和推断的地方
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

    ' TODO: 用 AssertThatDiagTriggeredInClass 补充能用 As 的地方：方法参数声明，返回值声明，委托参数声明，Sub New 参数声明，事件参数声明，事件返回值声明，字段声明，属性声明

    <TestMethod>
    Public Sub TestNullableSpanWithInference()
        Dim snippetContent = "Dim nullableSpan? = span"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    ' ====================
    ' 数组中的受限类型测试
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
    ' 结构体字段/属性测试
    ' ====================

    <TestMethod>
    Public Sub TestStructWithRestrictedField()
        Dim source As String = "
Imports System
Imports System.Runtime.InteropServices

<Obsolete(""Suppress default ref struct obsolete errors"")>
Structure TestStruct
    Public RestrictedField As Span(Of Integer)
End Structure
"
        AssertThatShouldHaveError(source, source)
    End Sub

    <TestMethod>
    Public Sub TestStructWithRestrictedAutoProperty()
        Dim source As String = "
Imports System
Imports System.Runtime.InteropServices

<Obsolete(""Suppress default ref struct obsolete errors"")>
Structure TestStruct
    Public Property RestrictedProp As Span(Of Integer)
End Structure
"
        AssertThatShouldHaveError(source, source)
    End Sub

    <TestMethod>
    Public Sub TestStructWithRestrictedFullProperty()
        Dim source As String = "
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
        AssertThatShouldHaveError(source, source)
    End Sub

    <TestMethod>
    Public Sub TestStructWithRestrictedFieldOk()
        Dim source As String = "
Imports System
Imports System.Runtime.InteropServices

<Obsolete(""Suppress default ref struct obsolete errors"")>
<System.Runtime.CompilerServices.IsByRefLike>
Structure TestStruct
    Public RestrictedField As Span(Of Integer)
End Structure
"
        AssertThatShouldNotHaveError(source, source)
    End Sub

    <TestMethod>
    Public Sub TestStructWithRestrictedPropertyOk()
        Dim source As String = "
Imports System
Imports System.Runtime.InteropServices

<Obsolete(""Suppress default ref struct obsolete errors"")>
<System.Runtime.CompilerServices.IsByRefLike>
Structure TestStruct
    Public Property RestrictedProp As Span(Of Integer)
End Structure
"
        AssertThatShouldNotHaveError(source, source)
    End Sub

    <TestMethod>
    Public Sub TestNonRestrictedStruct()
        Dim source As String = "
Imports System
Imports System.Runtime.InteropServices

Structure NormalStruct
    Public NormalField As Integer
End Structure

Class TestClass
    Sub TestMethod()
        Dim normalStruct As New NormalStruct()
    End Sub
End Class
"
        AssertThatShouldNotHaveError(source, source)
    End Sub

    ' ====================
    ' 类字段/属性测试
    ' ====================

    Private Sub AssertThatDiagTriggeredInClass(snippetContent As String)
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
        Dim source As String = "
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
        AssertThatShouldHaveError(source, source)
    End Sub

    ' ====================
    ' 匿名类型成员测试
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
    ' ByRef 参数测试
    ' ====================

    <TestMethod>
    Public Sub TestMethodWithRestrictedByRefParameter()
        Dim source As String = "
Imports System
Imports System.Runtime.InteropServices

<Obsolete(""Suppress default ref struct obsolete errors"")>
Class TestClass
    Sub TestMethod(ByRef restrictedParam As Span(Of Integer))
    End Sub
End Class
"
        AssertThatShouldHaveError(source, source)
    End Sub

    ' ====================
    ' 集合测试
    ' ====================

    ' 将 Span 添加到 Object 数组
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

    ' 将 Span 添加到 Object 数组 (42, span)
    <TestMethod>
    Public Sub TestSpanInObjectArray4()
        Dim snippetContent = "Dim objArray = {42, span}"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    ' 将 Span 添加到 Object 数组 (CType(42, Object), span)
    <TestMethod>
    Public Sub TestSpanInObjectArray5()
        Dim snippetContent = "Dim objArray = {CType(42, Object), span}"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    ' 将 Span 添加到 Object 数组 As Object() = {span}
    <TestMethod>
    Public Sub TestSpanInObjectArray6()
        Dim snippetContent = "Dim objArray As Object() = {span}"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    ' 将 Span 添加到 Object 数组 () As Object = {span}
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

    ' 将 Span 添加到 ValueType 数组
    <TestMethod>
    Public Sub TestSpanInValueTypeArray()
        Dim snippetContent = "Dim vtArray As ValueType() = {span, 42}"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    ' 将 Span 添加到 ValueType 数组 (CType(42, ValueType), span)
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

    ' 将 Span 添加到 ValueType 列表
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

    ' 将 Span 添加到 Object 字典
    <TestMethod>
    Public Sub TestSpanInObjectDictionary()
        Dim snippetContent = "Dim objDict1 = New Dictionary(Of String, Object) From {{""hello"", span}}"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    ' 将 Span 添加到 Object 字典 (As New Dictionary)
    <TestMethod>
    Public Sub TestSpanInObjectDictionary2()
        Dim snippetContent = "Dim objDict2 As New Dictionary(Of String, Object) From {{""hello"", span}}"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    ' 将 Span 添加到 ValueType 字典
    <TestMethod>
    Public Sub TestSpanInValueTypeDictionary()
        Dim snippetContent = "Dim vtDict1 = New Dictionary(Of Integer, ValueType) From {{1, span}}"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    ' 将 Span 添加到 ValueType 字典 (As New Dictionary)
    <TestMethod>
    Public Sub TestSpanInValueTypeDictionary2()
        Dim snippetContent = "Dim vtDict2 As New Dictionary(Of Integer, ValueType) From {{2, span}}"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    ' ====================
    ' 返回值测试
    ' ====================

    <TestMethod>
    Public Sub TestMethodReturnRestrictedType()
        Dim source As String = "
Imports System
Imports System.Runtime.InteropServices

<Obsolete(""Suppress default ref struct obsolete errors"")>
Class TestClass
    Function TestReturn() As Span(Of Integer)
        Dim arr As Integer() = {1, 2, 3, 4, 5}
        Dim span As Span(Of Integer) = arr.AsSpan()
        Return span
    End Function
End Class
"
        AssertThatShouldHaveError(source, source)
    End Sub

    ' ====================
    ' 误判测试
    ' ====================

    <TestMethod>
    Public Sub TestRegularMembers()
        Dim source As String = "
Imports System
Imports System.Runtime.InteropServices

<Obsolete(""Suppress default ref struct obsolete errors"")>
Class TestClass
    Sub TestMethod()
        Dim arr As Integer() = {{1, 2, 3, 4, 5}}
        Dim span As Span(Of Integer) = arr.AsSpan()
        arr(0) = span.Length
        Dim sliced = span.Slice(0,2)
    End Sub
End Class
"
        AssertThatShouldNotHaveError(source, source)
    End Sub

End Class

Imports Microsoft.VisualStudio.TestTools.UnitTesting

<TestClass, Ignore("还没实现")>
Public Class RefStructInCollectionInitializerAnalyzerTests

    Private Sub AssertThatDiagTriggeredInSub(snippetContent As String)
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
        With GetSyntaxTreeTextAndDiagnostics(source, snippetContent)
            Assert.IsTrue(ContainsDiagnostic(.diagnostics, "BCX31396"), $"应该检测到 BCX31396 诊断。语法树内容: { .syntaxTreeText}")
        End With
    End Sub

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

End Class

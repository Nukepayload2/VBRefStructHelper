Imports Microsoft.VisualStudio.TestTools.UnitTesting

<TestClass>
Public Class RefStructConvertToBoxedTypeAnalyzerTests

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
        With GetSyntaxTreeTextAndDiagnostics(source)
            Assert.IsTrue(ContainsDiagnostic(.diagnostics, "BCX31394"), $"应该检测到 BCX31394 诊断。语法树内容: { .syntaxTreeText}")
        End With
    End Sub

    <TestMethod>
    Public Sub TestSpanToObjectAssignment()
        Dim snippetContent = "Dim obj As Object = span ' 这应该触发 BCX31394"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    ' 使用 CType 转换 Span 到 Object
    <TestMethod>
    Public Sub TestCTypeSpanToObject()
        Dim snippetContent = "Dim obj As Object = CType(span, Object)  ' 这应该触发 BCX31394"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    ' 使用 DirectCast 转换 Span 到 Object
    <TestMethod>
    Public Sub TestDirectCastSpanToObject()
        Dim snippetContent = "Dim obj As Object = DirectCast(span, Object)  ' 这应该触发 BCX31394"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    ' 将 Span 添加到 Object 数组
    <TestMethod>
    Public Sub TestSpanInObjectArray()
        Dim snippetContent = "Dim objArray As Object() = {span, ""hello"", 42}  ' 这应该触发 BCX31394"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    <TestMethod>
    Public Sub TestSpanInObjectArray2()
        Dim snippetContent = "Dim objArray = New Object() {span, ""hello"", 42}  ' 这应该触发 BCX31394"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    <TestMethod>
    Public Sub TestSpanInObjectArray3()
        Dim snippetContent = "Dim objArray = {span, ""hello"", 42}  ' 这应该触发 BCX31394"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    ' 将 Span 添加到 Object 数组 (42, span)
    <TestMethod>
    Public Sub TestSpanInObjectArray4()
        Dim snippetContent = "Dim objArray = {42, span}  ' 这应该触发 BCX31394"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    ' 将 Span 添加到 Object 数组 (CType(42, Object), span)
    <TestMethod>
    Public Sub TestSpanInObjectArray5()
        Dim snippetContent = "Dim objArray = {CType(42, Object), span}  ' 这应该触发 BCX31394"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    ' 将 Span 添加到 Object 数组 As Object() = {span}
    <TestMethod>
    Public Sub TestSpanInObjectArray6()
        Dim snippetContent = "Dim objArray As Object() = {span}  ' 这应该触发 BCX31394"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    ' 将 Span 添加到 Object 数组 () As Object = {span}
    <TestMethod>
    Public Sub TestSpanInObjectArray7()
        Dim snippetContent = "Dim objArray() As Object = {span}  ' 这应该触发 BCX31394"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    <TestMethod>
    Public Sub TestSpanInObjectList()
        Dim snippetContent = "Dim objList4 = New List(Of Object) From {span, ""hello"", 42}  ' 这应该触发 BCX31394"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    <TestMethod>
    Public Sub TestSpanInObjectList2()
        Dim snippetContent = "Dim objList4 As New List(Of Object) From {span, ""hello"", 42}  ' 这应该触发 BCX31394"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    ' 将 Span 赋值给 ValueType 变量
    <TestMethod>
    Public Sub TestSpanToValueTypeAssignment()
        Dim snippetContent = "Dim valueType As ValueType = span  ' 这应该触发 BCX31394"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    ' 使用 CType 转换 Span 到 ValueType
    <TestMethod>
    Public Sub TestCTypeSpanToValueType()
        Dim snippetContent = "Dim valueType As ValueType = CType(span, ValueType)  ' 这应该触发 BCX31394"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    <TestMethod>
    Public Sub TestCObjSpan()
        Dim snippetContent = "Dim valueType = CObj(span)  ' 这应该触发 BCX31394"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    ' 使用 CObj 转换 Span 到 Object
    <TestMethod>
    Public Sub TestCObjSpanToObject()
        Dim snippetContent = "Dim obj = CObj(span)  ' 这应该触发 BCX31394"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    ' 使用 TryCast 转换 Span 到 Object
    <TestMethod>
    Public Sub TestTryCastSpanToObject()
        Dim snippetContent = "Dim obj As Object = TryCast(span, Object)  ' 这应该触发 BCX31394"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    ' 使用 TryCast 转换 Span 到 ValueType
    <TestMethod>
    Public Sub TestTryCastSpanToValueType()
        Dim snippetContent = "Dim valueType As ValueType = TryCast(span, ValueType)  ' 这应该触发 BCX31394"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    ' 使用 DirectCast 转换 Span 到 ValueType
    <TestMethod>
    Public Sub TestDirectCastSpanToValueType()
        Dim snippetContent = "Dim valueType As ValueType = DirectCast(span, ValueType)  ' 这应该触发 BCX31394"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    ' 将 Span 赋值给 ValueType 变量 (直接赋值)
    <TestMethod>
    Public Sub TestSpanToValueTypeAssignmentDirect()
        Dim snippetContent = "Dim valueType As ValueType = span  ' 这应该触发 BCX31394"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    ' 将 Span 添加到 ValueType 数组
    <TestMethod>
    Public Sub TestSpanInValueTypeArray()
        Dim snippetContent = "Dim vtArray As ValueType() = {span, 42}  ' 这应该触发 BCX31394"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    ' 将 Span 添加到 ValueType 数组 (CType(42, ValueType), span)
    <TestMethod>
    Public Sub TestSpanInValueTypeArray2()
        Dim snippetContent = "Dim vtArray = {CType(42, ValueType), span}  ' 这应该触发 BCX31394"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    <TestMethod>
    Public Sub TestSpanInValueTypeArray3()
        Dim snippetContent = "Dim vtArray() As ValueType = {span}  ' 这应该触发 BCX31394"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    <TestMethod>
    Public Sub TestSpanInValueTypeArray4()
        Dim snippetContent = "Dim vtArray As ValueType() = {span}  ' 这应该触发 BCX31394"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    <TestMethod>
    Public Sub TestSpanInValueTypeArray5()
        Dim snippetContent = "Dim vtArray() As ValueType = {span, 42}  ' 这应该触发 BCX31394"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    ' 将 Span 添加到 ValueType 列表
    <TestMethod>
    Public Sub TestSpanInValueTypeList()
        Dim snippetContent = "Dim vtList1 = New List(Of ValueType) From {span, 42}  ' 这应该触发 BCX31394"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    <TestMethod>
    Public Sub TestSpanInValueTypeList2()
        Dim snippetContent = "Dim vtList2 As New List(Of ValueType) From {span, 42}  ' 这应该触发 BCX31394"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    ' 将 Span 添加到 Object 字典
    <TestMethod>
    Public Sub TestSpanInObjectDictionary()
        Dim snippetContent = "Dim objDict1 = New Dictionary(Of String, Object) From {{""hello"", span}}  ' 这应该触发 BCX31394"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    ' 将 Span 添加到 Object 字典 (As New Dictionary)
    <TestMethod>
    Public Sub TestSpanInObjectDictionary2()
        Dim snippetContent = "Dim objDict2 As New Dictionary(Of String, Object) From {{""hello"", span}}  ' 这应该触发 BCX31394"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    ' 将 Span 添加到 ValueType 字典
    <TestMethod>
    Public Sub TestSpanInValueTypeDictionary()
        Dim snippetContent = "Dim vtDict1 = New Dictionary(Of Integer, ValueType) From {{1, span}}  ' 这应该触发 BCX31394"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    ' 将 Span 添加到 ValueType 字典 (As New Dictionary)
    <TestMethod>
    Public Sub TestSpanInValueTypeDictionary2()
        Dim snippetContent = "Dim vtDict2 As New Dictionary(Of Integer, ValueType) From {{2, span}}  ' 这应该触发 BCX31394"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    ' 从函数返回 Span 作为 Object
    <TestMethod>
    Public Sub TestReturnSpanAsObject()
        Dim source As String = "
Imports System
Imports System.Runtime.InteropServices

<Obsolete(""Suppress default ref struct obsolete errors"")>
Class TestClass
    Function TestReturnSpanAsObject() As Object
        Dim arr As Integer() = {1, 2, 3, 4, 5}
        Dim span As Span(Of Integer) = arr.AsSpan()
        Return span  ' 这应该触发 BCX31394
    End Function
End Class
"
        With GetSyntaxTreeTextAndDiagnostics(source)
            Assert.IsTrue(ContainsDiagnostic(.diagnostics, "BCX31394"), $"应该检测到 BCX31394 诊断。语法树内容: { .syntaxTreeText}")
        End With
    End Sub

    ' 从函数返回 Span 作为 ValueType
    <TestMethod>
    Public Sub TestReturnSpanAsValueType()
        Dim source As String = "
Imports System
Imports System.Runtime.InteropServices

<Obsolete(""Suppress default ref struct obsolete errors"")>
Class TestClass
    Function TestReturnSpanAsValueType() As ValueType
        Dim arr As Integer() = {1, 2, 3, 4, 5}
        Dim span As Span(Of Integer) = arr.AsSpan()
        Return span  ' 这应该触发 BCX31394
    End Function
End Class
"
        With GetSyntaxTreeTextAndDiagnostics(source)
            Assert.IsTrue(ContainsDiagnostic(.diagnostics, "BCX31394"), $"应该检测到 BCX31394 诊断。语法树内容: { .syntaxTreeText}")
        End With
    End Sub

    ' 将 Span 作为 Object 参数传递
    <TestMethod>
    Public Sub TestSpanAsObjectParameter()
        Dim source As String = "
Imports System
Imports System.Runtime.InteropServices

<Obsolete(""Suppress default ref struct obsolete errors"")>
Class TestClass
    Sub TestMethod()
        Dim arr As Integer() = {1, 2, 3, 4, 5}
        Dim span As Span(Of Integer) = arr.AsSpan()
        TestMethodTakingObject(span)  ' 这应该触发 BCX31394
    End Sub

    Sub TestMethodTakingObject(obj As Object)
    End Sub
End Class
"
        With GetSyntaxTreeTextAndDiagnostics(source)
            Assert.IsTrue(ContainsDiagnostic(.diagnostics, "BCX31394"), $"应该检测到 BCX31394 诊断。语法树内容: { .syntaxTreeText}")
        End With
    End Sub

    ' 将 Span 作为 ValueType 参数传递
    <TestMethod>
    Public Sub TestSpanAsValueTypeParameter()
        Dim source As String = "
Imports System
Imports System.Runtime.InteropServices

<Obsolete(""Suppress default ref struct obsolete errors"")>
Class TestClass
    Sub TestMethod()
        Dim arr As Integer() = {1, 2, 3, 4, 5}
        Dim span As Span(Of Integer) = arr.AsSpan()
        TestMethodTakingValueType(span)  ' 这应该触发 BCX31394
    End Sub

    Sub TestMethodTakingValueType(obj As ValueType)
    End Sub
End Class
"
        With GetSyntaxTreeTextAndDiagnostics(source)
            Assert.IsTrue(ContainsDiagnostic(.diagnostics, "BCX31394"), $"应该检测到 BCX31394 诊断。语法树内容: { .syntaxTreeText}")
        End With
    End Sub

    ' ReadOnlySpan 的类似测试
    <TestMethod>
    Public Sub TestReadOnlySpanToObject()
        Dim source As String = "
Imports System
Imports System.Runtime.InteropServices

<Obsolete(""Suppress default ref struct obsolete errors"")>
Class TestClass
    Sub TestMethod()
        Dim readOnlySpan As ReadOnlySpan(Of Integer) = {1, 2, 3, 4, 5}
        Dim obj As Object = readOnlySpan  ' 这也应该触发 BCX31394
    End Sub
End Class
"
        With GetSyntaxTreeTextAndDiagnostics(source)
            Assert.IsTrue(ContainsDiagnostic(.diagnostics, "BCX31394"), $"应该检测到 BCX31394 诊断。语法树内容: { .syntaxTreeText}")
        End With
    End Sub

    ' 正确的用法不应该触发
    <TestMethod>
    Public Sub TestCorrectUsage()
        Dim source As String = "
Imports System
Imports System.Runtime.InteropServices

<Obsolete(""Suppress default ref struct obsolete errors"")>
Class TestClass
    Sub TestMethod()
        ' 直接使用 Span，不进行装箱转换
        Dim arr As Integer() = {1, 2, 3, 4, 5}
        Dim span As Span(Of Integer) = arr.AsSpan()
        span(0) = 10
        Dim length As Integer = span.Length
        ' 这些都是正确的用法，不应该触发
    End Sub
End Class
"
        With GetSyntaxTreeTextAndDiagnostics(source)
            Assert.IsFalse(ContainsDiagnostic(.diagnostics, "BCX31394"), $"不应该检测到 BCX31394 诊断。语法树内容: { .syntaxTreeText}")
        End With
    End Sub

    ' 没有使用 Span 的正常代码不应该触发
    <TestMethod>
    Public Sub TestNormalCode()
        Dim source As String = "
Imports System

Class TestClass
    Sub TestMethod()
        Dim obj As Object = ""hello""
        Dim arr As Object() = {""hello"", 42}
        Dim valueType As ValueType = 123
    End Sub
End Class
"
        With GetSyntaxTreeTextAndDiagnostics(source)
            Assert.IsFalse(ContainsDiagnostic(.diagnostics, "BCX31394"), $"不应该检测到 BCX31394 诊断。语法树内容: { .syntaxTreeText}")
        End With
    End Sub

End Class

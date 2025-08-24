Imports System
Imports System.Runtime.InteropServices

<Obsolete("Suppress default ref struct obsolete errors")>
Public Class RefStructConvertToBoxedType
    ' 演示 Span 类型的各种错误用法，这些应该被 RefStructConvertToBoxedTypeAnalyzer 检测到

    ' 1. 直接将 Span 赋值给 Object 变量
    Sub TestSpanToObjectAssignment()
        Dim arr As Integer() = {1, 2, 3, 4, 5}
        Dim span As Span(Of Integer) = arr.AsSpan()
        Dim obj As Object = span  ' 这应该触发 BCX31394
    End Sub

    ' 2. 使用 CType 转换 Span 到 Object
    Sub TestCTypeSpanToObject()
        Dim arr As Integer() = {1, 2, 3, 4, 5}
        Dim span As Span(Of Integer) = arr.AsSpan()
        Dim obj As Object = CType(span, Object)  ' 这应该触发 BCX31394
    End Sub

    ' 3. 使用 DirectCast 转换 Span 到 Object
    Sub TestDirectCastSpanToObject()
        Dim arr As Integer() = {1, 2, 3, 4, 5}
        Dim span As Span(Of Integer) = arr.AsSpan()
        Dim obj As Object = DirectCast(span, Object)  ' 这应该触发 BCX31394
    End Sub

    ' 4. 将 Span 作为 Object 参数传递
    Sub TestSpanAsObjectParameter()
        Dim arr As Integer() = {1, 2, 3, 4, 5}
        Dim span As Span(Of Integer) = arr.AsSpan()
        TestMethodTakingObject(span)  ' 这应该触发 BCX31394
    End Sub

    ' 5. 将 Span 添加到 Object 数组
    Sub TestSpanInObjectArray()
        Dim arr As Integer() = {1, 2, 3, 4, 5}
        Dim span As Span(Of Integer) = arr.AsSpan()
        Dim objArray As Object() = {span, "hello", 42}  ' 这应该触发 BCX31394
    End Sub

    ' 6. 从函数返回 Span 作为 Object
    Function TestReturnSpanAsObject() As Object
        Dim arr As Integer() = {1, 2, 3, 4, 5}
        Dim span As Span(Of Integer) = arr.AsSpan()
        Return span  ' 这应该触发 BCX31394
    End Function

    ' 7. 将 Span 赋值给 ValueType 变量
    Sub TestSpanToValueTypeAssignment()
        Dim arr As Integer() = {1, 2, 3, 4, 5}
        Dim span As Span(Of Integer) = arr.AsSpan()
        Dim valueType As ValueType = span  ' 这应该触发 BCX31394
    End Sub

    ' 8. 使用 CType 转换 Span 到 ValueType
    Sub TestCTypeSpanToValueType()
        Dim arr As Integer() = {1, 2, 3, 4, 5}
        Dim span As Span(Of Integer) = arr.AsSpan()
        Dim valueType As ValueType = CType(span, ValueType)  ' 这应该触发 BCX31394
    End Sub

    ' 辅助方法
    Sub TestMethodTakingObject(obj As Object)
        ' 这个方法用于测试参数传递场景
    End Sub

    ' 9. ReadOnlySpan 的类似测试
    Sub TestReadOnlySpanToObject()
        Dim readOnlySpan As ReadOnlySpan(Of Integer) = {1, 2, 3, 4, 5}
        Dim obj As Object = readOnlySpan  ' 这也应该触发 BCX31394
    End Sub

    ' 正确的用法示例（不应该触发）
    Sub TestCorrectUsage()
        ' 直接使用 Span，不进行装箱转换
        Dim arr As Integer() = {1, 2, 3, 4, 5}
        Dim span As Span(Of Integer) = arr.AsSpan()
        span(0) = 10
        Dim length As Integer = span.Length
        ' 这些都是正确的用法
    End Sub
End Class

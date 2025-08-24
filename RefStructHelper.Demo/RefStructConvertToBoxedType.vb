<Obsolete("Suppress default ref struct obsolete errors")>
Public Class RefStructConvertToBoxedType
    ' 演示 Span 类型的各种错误用法，这些应该被 RefStructConvertToBoxedTypeAnalyzer 检测到

    ' 辅助方法
    Sub TestMethodTakingObject(obj As Object)
        ' 这个方法用于测试参数传递场景
    End Sub

    ' 辅助方法
    Sub TestMethodTakingValueType(obj As ValueType)
        ' 这个方法用于测试参数传递场景
    End Sub

    Sub WrongUsagesTypeCast()
        Dim arr As Integer() = {1, 2, 3, 4, 5}
        Dim span As Span(Of Integer) = arr.AsSpan()

        ' 验证区域
        Dim obj1 As Object = span ' 这应该触发 BCX31394
        Dim obj2 As Object = CType(span, Object)  ' 这应该触发 BCX31394
        Dim obj3 = CObj(span)  ' 这应该触发 BCX31394
        Dim obj4 As Object = DirectCast(span, Object)  ' 这应该触发 BCX31394
        Dim obj5 As Object = TryCast(span, Object)  ' 这应该触发 BCX31394

        Dim vt1 As ValueType = span ' 这应该触发 BCX31394
        Dim vt2 As ValueType = CType(span, ValueType)  ' 这应该触发 BCX31394
        Dim vt3 As ValueType = TryCast(span, ValueType)  ' 这应该触发 BCX31394
        Dim vt4 As ValueType = DirectCast(span, ValueType)  ' 这应该触发 BCX31394

        TestMethodTakingObject(span)  ' 这应该触发 BCX31394
        TestMethodTakingValueType(span)  ' 这应该触发 BCX31394
    End Sub

    Function WrongUsagesTypeCastAtReturn() As Object
        Dim arr As Integer() = {1, 2, 3, 4, 5}
        Dim span As Span(Of Integer) = arr.AsSpan()
        Return span  ' 这应该触发 BCX31394
    End Function

    Function WrongUsagesTypeCastAtReturn2() As ValueType
        Dim arr As Integer() = {1, 2, 3, 4, 5}
        Dim span As Span(Of Integer) = arr.AsSpan()
        Return span  ' 这应该触发 BCX31394
    End Function

    Sub TestCorrectUsage()
        ' 直接使用 Span，不进行装箱转换
        Dim arr As Integer() = {1, 2, 3, 4, 5}
        Dim span As Span(Of Integer) = arr.AsSpan()
        span(0) = 10
        Dim length As Integer = span.Length
    End Sub
End Class

<Obsolete("Suppress default ref struct obsolete errors")>
Public Class RestrictedTypeMemberAccessDemo
    ' 演示调用受限类型继承的Object/ValueType方法的错误使用 (BCX31393)

    Sub WrongUsages()
        Dim arr As Integer() = {1, 2, 3, 4, 5}
        Dim span As Span(Of Integer) = arr.AsSpan()

        ' 调用继承的Object方法
        Dim str As String = span.ToString()  ' 这应该触发 BCX31393
        Dim result As Boolean = span.Equals(span)  ' 这应该触发 BCX31393
        Dim hash As Integer = span.GetHashCode()  ' 这应该触发 BCX31393
        Dim type As Type = span.GetType()  ' 这应该触发 BCX31393
        Dim refResult As Boolean = ReferenceEquals(span, span)  ' 这应该触发 BCX31393

        ' 调用继承的ValueType方法
        Dim vtResult As Boolean = span.Equals(CType(Nothing, ValueType))  ' 这应该触发 BCX31393

        ' 字符串插值
        Dim interpolated As String = $"Span: {span}"  ' 这应该触发 BCX31393
    End Sub

    Sub CorrectUsages()
        Dim arr As Integer() = {1, 2, 3, 4, 5}
        Dim span As Span(Of Integer) = arr.AsSpan()

        ' 正确使用 - 调用Span自己的方法
        Dim length As Integer = span.Length
        Dim sliced = span.Slice(0, 2)
        span(0) = 10
    End Sub
End Class

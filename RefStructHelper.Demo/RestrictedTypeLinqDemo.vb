<Obsolete("Suppress default ref struct obsolete errors")>
Public Class RestrictedTypeLinqDemo
    ' 演示LINQ中受限类型的使用限制 (BCX36598)

    Sub WrongUsages()
        Dim arr As Integer() = {1, 2, 3, 4, 5}
        Dim span As Span(Of Integer) = arr.AsSpan()

        ' LINQ查询中使用受限类型
        Dim q1 = From a In arr Where a = span.Length  ' 这应该触发 BCX36598
        Dim q2 = From a In arr Let b = span.Length Select a + b  ' 这应该触发 BCX36598
        Dim q3 = From a In arr Select a, g = span.Length  ' 这应该触发 BCX36598
        Dim q4 = From a In arr Group By span.Length Into Group  ' 这应该触发 BCX36598

        ' LINQ范围变量声明
        Dim q5 = From a In arr Select a, g = span  ' 这应该触发 BCX36598
        Dim q6 = From a In arr Let b = span Select a, b  ' 这应该触发 BCX36598
        Dim q7 = From a In arr Select a, g = arr.AsSpan  ' 这应该触发 BCX36598
        Dim q8 = From a In arr Let b = arr.AsSpan Select a, b  ' 这应该触发 BCX36598
        Dim q9 = From a In arr Group By span Into Group  ' 这应该触发 BCX36598
    End Sub

    Sub CorrectUsages()
        ' 正确的LINQ使用
        Dim arr As Integer() = {1, 2, 3, 4, 5}
        Dim query = From item In arr Where item > 0 Select item
        Dim result = query.ToList()
    End Sub
End Class

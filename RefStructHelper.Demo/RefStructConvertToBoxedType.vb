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

        Dim objArray1 As Object() = {span, "hello", 42}  ' 这应该触发 BCX31394
        Dim objArray2 = {42, span}  ' 这应该触发 BCX31394
        Dim objArray3() As Object = {span, "hello", 42}  ' 这应该触发 BCX31394
        Dim objArray4 = {CType(42, Object), span}  ' 这应该触发 BCX31394
        Dim objArray5 As Object() = {span}  ' 这应该触发 BCX31394
        Dim objArray6() As Object = {span}  ' 这应该触发 BCX31394

        Dim vtArray1 As ValueType() = {span, 42}  ' 这应该触发 BCX31394
        Dim vtArray2 = {CType(42, ValueType), span}   ' 这应该触发 BCX31394
        Dim vtArray3() As ValueType = {span}  ' 这应该触发 BCX31394
        Dim vtArray4 As ValueType() = {span}  ' 这应该触发 BCX31394
        Dim vtArray5() As ValueType = {span, 42}  ' 这应该触发 BCX31394

        Dim objList1 = New List(Of Object) From {span, "hello", 42}  ' 这应该触发 BCX31394
        Dim objList2 As New List(Of Object) From {span, "hello", 42}  ' 这应该触发 BCX31394

        Dim vtList1 = New List(Of ValueType) From {span, 42}  ' 这应该触发 BCX31394
        Dim vtList2 As New List(Of ValueType) From {span, 42}  ' 这应该触发 BCX31394

        Dim objDict1 = New Dictionary(Of String, Object) From {{"hello", span}}  ' 这应该触发 BCX31394
        Dim objDict2 As New Dictionary(Of String, Object) From {{"hello", span}}  ' 这应该触发 BCX31394

        Dim vtDict1 = New Dictionary(Of Integer, ValueType) From {{1, span}}  ' 这应该触发 BCX31394
        Dim vtDict2 As New Dictionary(Of Integer, ValueType) From {{2, span}}  ' 这应该触发 BCX31394

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

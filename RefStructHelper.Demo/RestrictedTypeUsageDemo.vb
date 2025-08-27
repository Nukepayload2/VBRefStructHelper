<Obsolete("Suppress default ref struct obsolete errors")>
Public Class RestrictedTypeUsageDemo
    ' 演示受限类型的使用限制 (BCX31396)

    Sub WrongUsages()
        Dim arr As Integer() = {1, 2, 3, 4, 5}
        Dim span As Span(Of Integer) = arr.AsSpan()

        ' Nullable类型声明
        Dim nullableSpan As Span(Of Integer)? = Nothing  ' 这应该触发 BCX31396
        Dim nullableSpan2? As Span(Of Integer) = Nothing  ' 这应该触发 BCX31396

        Static somethine As Span(Of Integer) = span  ' 这应该触发 BCX31396

        ' 数组类型声明
        Dim spanArray As Span(Of Integer)()  ' 这应该触发 BCX31396
        Dim spanArray2() As Span(Of Integer)  ' 这应该触发 BCX31396

        ' 集合初始化表达式
        Dim objArray1 As Object() = {span, "hello", 42}  ' 这应该触发 BCX31396
        Dim objArray2 = {42, span}  ' 这应该触发 BCX31396
        Dim objArray3() As Object = {span, "hello", 42}  ' 这应该触发 BCX31396
        Dim objArray4 = {CType(42, Object), span}  ' 这应该触发 BCX31396
        Dim objArray5 As Object() = {span}  ' 这应该触发 BCX31396
        Dim objArray6() As Object = {span}  ' 这应该触发 BCX31396

        Dim vtArray1 As ValueType() = {span, 42}  ' 这应该触发 BCX31396
        Dim vtArray2 = {CType(42, ValueType), span}   ' 这应该触发 BCX31396
        Dim vtArray3() As ValueType = {span}  ' 这应该触发 BCX31396
        Dim vtArray4 As ValueType() = {span}  ' 这应该触发 BCX31396
        Dim vtArray5() As ValueType = {span, 42}  ' 这应该触发 BCX31396

        Dim objList1 = New List(Of Object) From {span, "hello", 42}  ' 这应该触发 BCX31396
        Dim objList2 As New List(Of Object) From {span, "hello", 42}  ' 这应该触发 BCX31396

        Dim vtList1 = New List(Of ValueType) From {span, 42}  ' 这应该触发 BCX31396
        Dim vtList2 As New List(Of ValueType) From {span, 42}  ' 这应该触发 BCX31396

        Dim objDict1 = New Dictionary(Of String, Object) From {{"hello", span}}  ' 这应该触发 BCX31396
        Dim objDict2 As New Dictionary(Of String, Object) From {{"hello", span}}  ' 这应该触发 BCX31396

        Dim vtDict1 = New Dictionary(Of Integer, ValueType) From {{1, span}}  ' 这应该触发 BCX31396
        Dim vtDict2 As New Dictionary(Of Integer, ValueType) From {{2, span}}  ' 这应该触发 BCX31396
    End Sub

    ' 类字段声明错误
    Public RestrictedField As Span(Of Integer)  ' 这应该触发 BCX31396

    ' 类属性声明错误
    Public Property RestrictedProp As Span(Of Integer)  ' 这应该触发 BCX31396

    ' 匿名类型成员错误
    Sub AnonymousTypeWrongUsage()
        Dim arr As Integer() = {1, 2, 3, 4, 5}
        Dim span As Span(Of Integer) = arr.AsSpan()

        Dim anon1 = New With {.RestrictedValue = span}  ' 这应该触发 BCX31396
        Dim anon2 = New With {Key .RestrictedValue = span}  ' 这应该触发 BCX31396
    End Sub

    ' ByRef参数错误
    Sub MethodWithRestrictedByRefParameter(ByRef restrictedParam As Span(Of Integer))  ' 这应该触发 BCX31396
    End Sub

    ' 返回值错误
    Function MethodReturnRestrictedType() As Span(Of Integer)  ' 这应该触发 BCX31396
        Dim arr As Integer() = {1, 2, 3, 4, 5}
        Dim span As Span(Of Integer) = arr.AsSpan()
        Return span
    End Function

    Shared Narrowing Operator CType(instance As RestrictedTypeUsageDemo) As Span(Of Integer) ' 这应该触发 BCX31396
        Throw New NotSupportedException
    End Operator

    Private Class Something2
        Sub New(arg As Span(Of Integer))
        End Sub
    End Class

    Sub CorrectUsages()
        Dim arr As Integer() = {1, 2, 3, 4, 5}
        Dim span As Span(Of Integer) = arr.AsSpan()

        ' 正确使用 - 局部变量
        Dim localSpan As Span(Of Integer) = span
        Dim sliced = span.Slice(0, 2)

        Dim y2 = New Something2(span)
        ' 正确使用 - 参数传递
        ProcessSpan(span)
    End Sub

    Sub ProcessSpan(span As Span(Of Integer))
        ' 正常处理span
    End Sub
End Class

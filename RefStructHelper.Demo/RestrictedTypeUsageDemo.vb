<Obsolete("Suppress default ref struct obsolete errors")>
Public Class RestrictedTypeUsageDemo
    ' Demonstrate usage restrictions of restricted types (BCX31396)

    Sub WrongUsages()
        Dim arr As Integer() = {1, 2, 3, 4, 5}
        Dim span As Span(Of Integer) = arr.AsSpan()

        ' Nullable type declaration
        Dim nullableSpan As Span(Of Integer)? = Nothing  ' This should trigger BCX31396
        Dim nullableSpan2? As Span(Of Integer) = Nothing  ' This should trigger BCX31396

        Static somethine As Span(Of Integer) = span  ' This should trigger BCX31396

        ' Array type declaration
        Dim spanArray As Span(Of Integer)()  ' This should trigger BCX31396
        Dim spanArray2() As Span(Of Integer)  ' This should trigger BCX31396

        ' Collection initializer expressions
        Dim objArray1 As Object() = {span, "hello", 42}  ' This should trigger BCX31396
        Dim objArray2 = {42, span}  ' This should trigger BCX31396
        Dim objArray3() As Object = {span, "hello", 42}  ' This should trigger BCX31396
        Dim objArray4 = {CType(42, Object), span}  ' This should trigger BCX31396
        Dim objArray5 As Object() = {span}  ' This should trigger BCX31396
        Dim objArray6() As Object = {span}  ' This should trigger BCX31396

        Dim vtArray1 As ValueType() = {span, 42}  ' This should trigger BCX31396
        Dim vtArray2 = {CType(42, ValueType), span}   ' This should trigger BCX31396
        Dim vtArray3() As ValueType = {span}  ' This should trigger BCX31396
        Dim vtArray4 As ValueType() = {span}  ' This should trigger BCX31396
        Dim vtArray5() As ValueType = {span, 42}  ' This should trigger BCX31396

        Dim objList1 = New List(Of Object) From {span, "hello", 42}  ' This should trigger BCX31396
        Dim objList2 As New List(Of Object) From {span, "hello", 42}  ' This should trigger BCX31396

        Dim vtList1 = New List(Of ValueType) From {span, 42}  ' This should trigger BCX31396
        Dim vtList2 As New List(Of ValueType) From {span, 42}  ' This should trigger BCX31396

        Dim objDict1 = New Dictionary(Of String, Object) From {{"hello", span}}  ' This should trigger BCX31396
        Dim objDict2 As New Dictionary(Of String, Object) From {{"hello", span}}  ' This should trigger BCX31396

        Dim vtDict1 = New Dictionary(Of Integer, ValueType) From {{1, span}}  ' This should trigger BCX31396
        Dim vtDict2 As New Dictionary(Of Integer, ValueType) From {{2, span}}  ' This should trigger BCX31396
    End Sub

    ' Class field declaration error
    Public RestrictedField As Span(Of Integer)  ' This should trigger BCX31396

    ' Class property declaration error
    Public Property RestrictedProp As Span(Of Integer)  ' This should trigger BCX31396

    ' Anonymous type member error
    Sub AnonymousTypeWrongUsage()
        Dim arr As Integer() = {1, 2, 3, 4, 5}
        Dim span As Span(Of Integer) = arr.AsSpan()

        Dim anon1 = New With {.RestrictedValue = span}  ' This should trigger BCX31396
        Dim anon2 = New With {Key .RestrictedValue = span}  ' This should trigger BCX31396
    End Sub

    ' ByRef parameter error
    Sub MethodWithRestrictedByRefParameter(ByRef restrictedParam As Span(Of Integer))  ' This should trigger BCX31396
    End Sub

    ' Return value error
    Function MethodReturnRestrictedType() As Span(Of Integer)  ' This should trigger BCX31396
        Dim arr As Integer() = {1, 2, 3, 4, 5}
        Dim span As Span(Of Integer) = arr.AsSpan()
        Return span
    End Function

    Shared Narrowing Operator CType(instance As RestrictedTypeUsageDemo) As Span(Of Integer) ' This should trigger BCX31396
        Throw New NotSupportedException
    End Operator

    Private Class Something2
        Sub New(arg As Span(Of Integer))
        End Sub
    End Class

    Sub CorrectUsages()
        Dim arr As Integer() = {1, 2, 3, 4, 5}
        Dim span As Span(Of Integer) = arr.AsSpan()

        ' Correct usage - local variables
        Dim localSpan As Span(Of Integer) = span
        Dim sliced = span.Slice(0, 2)

        Dim y2 = New Something2(span)
        ' Correct usage - parameter passing
        ProcessSpan(span)
    End Sub

    Sub ProcessSpan(span As Span(Of Integer))
        ' Normal span processing
    End Sub
End Class

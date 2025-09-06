<Obsolete("Suppress default ref struct obsolete errors")>
Public Class RestrictedTypeInCollection

    Sub WrongUsages()
        Dim arr As Integer() = {1, 2, 3, 4, 5}
        Dim span As Span(Of Integer) = arr.AsSpan()

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

End Class

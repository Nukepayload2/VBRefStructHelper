<Obsolete("Suppress default ref struct obsolete errors")>
Public Class RestrictedTypeConvertToBoxedType
    ' Demonstrate various incorrect usages of Span types

    <System.Runtime.CompilerServices.IsByRefLike>
    Private Structure DemoType
        Implements IDisposable

        Public Sub Dispose() Implements IDisposable.Dispose
            Throw New NotSupportedException()
        End Sub
    End Structure

    Sub WrongUsagesTypeCast()
        Dim arr As Integer() = {1, 2, 3, 4, 5}
        Dim span As Span(Of Integer) = arr.AsSpan()

        ' Verification area
        Dim obj1 As Object = span ' This should trigger BCX31394
        Dim obj2 As Object = CType(span, Object)  ' This should trigger BCX31394
        Dim obj3 = CObj(span)  ' This should trigger BCX31394
        Dim obj4 As Object = DirectCast(span, Object)  ' This should trigger BCX31394
        Dim obj5 As Object = TryCast(span, Object)  ' This should trigger BCX31394

        Dim vt1 As ValueType = span ' This should trigger BCX31394
        Dim vt2 As ValueType = CType(span, ValueType)  ' This should trigger BCX31394
        Dim vt3 As ValueType = TryCast(span, ValueType)  ' This should trigger BCX31394
        Dim vt4 As ValueType = DirectCast(span, ValueType)  ' This should trigger BCX31394

        Dim sth As New Something With {.SomeValue = span} ' This should trigger BCX31394

        Dim oops As IDisposable = New DemoType ' This should trigger BCX31394
    End Sub

    ' Helper method
    Sub TestMethodTakingObject(obj As Object)
        ' This method is used to test parameter passing scenarios
    End Sub

    ' Helper method
    Sub TestMethodTakingValueType(obj As ValueType)
        ' This method is used to test parameter passing scenarios
    End Sub

    Private Class Something
        Sub New()
        End Sub
        Sub New(arg As Object)
        End Sub
        Public Property SomeValue As Object
    End Class

    Private Event SomeEvent(arg As Object)

    Sub WrongUsagesInvoke()
        Dim arr As Integer() = {1, 2, 3, 4, 5}
        Dim span As Span(Of Integer) = arr.AsSpan()

        TestMethodTakingObject(span)  ' This should trigger BCX31394
        TestMethodTakingValueType(span)  ' This should trigger BCX31394
        RaiseEvent SomeEvent(span)  ' This should trigger BCX31394
        Dim x As New Something(span)  ' This should trigger BCX31394
        Dim y = New Something(span)  ' This should trigger BCX31394
    End Sub

    Function WrongUsagesTypeCastAtReturn() As Object
        Dim arr As Integer() = {1, 2, 3, 4, 5}
        Dim span As Span(Of Integer) = arr.AsSpan()
        Return span  ' This should trigger BCX31394
    End Function

    Function WrongUsagesTypeCastAtReturn2() As ValueType
        Dim arr As Integer() = {1, 2, 3, 4, 5}
        Dim span As Span(Of Integer) = arr.AsSpan()
        Return span  ' This should trigger BCX31394
    End Function

    Sub TestCorrectUsage()
        ' Use Span directly without boxing conversion
        Dim arr As Integer() = {1, 2, 3, 4, 5}
        Dim span As Span(Of Integer) = arr.AsSpan()
        span(0) = 10
        Dim length As Integer = span.Length
    End Sub
End Class

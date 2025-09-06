<Obsolete("Suppress default ref struct obsolete errors")>
Public Class RestrictedTypeMemberAccessDemo
    ' Demonstrate incorrect usage of calling inherited Object/ValueType methods on restricted types (BCX31393)

    <System.Runtime.CompilerServices.IsByRefLike>
    Private Structure DemoType
    End Structure

    Sub WrongUsages()
        Dim demoType As DemoType

        ' Call inherited Object methods
        Dim str As String = demoType.ToString()  ' This should trigger BCX31393
        Dim result As Boolean = demoType.Equals(demoType)  ' This should trigger BCX31393
        Dim hash As Integer = demoType.GetHashCode()  ' This should trigger BCX31393
        Dim type As Type = demoType.GetType()  ' This should trigger BCX31393
        Dim refResult As Boolean = ReferenceEquals(demoType, demoType)  ' This should trigger BCX31393
        ' Call inherited ValueType methods
        Dim vtResult As Boolean = demoType.Equals(CType(Nothing, ValueType))  ' This should trigger BCX31393

        ' String interpolation
        Dim interpolated As String = $"Span: {demoType}"  ' This should trigger BCX31393
    End Sub

    Sub CorrectUsages()
        Dim arr As Integer() = {1, 2, 3, 4, 5}
        Dim span As Span(Of Integer) = arr.AsSpan()

        ' Correct usage - call Span's own methods
        Dim length As Integer = span.Length
        Dim sliced = span.Slice(0, 2)
        span(0) = 10
    End Sub
End Class

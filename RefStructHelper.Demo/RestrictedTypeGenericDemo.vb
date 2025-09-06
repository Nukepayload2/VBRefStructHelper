<Obsolete("Suppress default ref struct obsolete errors")>
Public Class RestrictedTypeGenericDemo
    ' Demonstrate restrictions of restricted types as generic parameters (BCX32061)

    Sub WrongUsages()
        Dim arr As Integer() = {1, 2, 3, 4, 5}
        Dim span As Span(Of Integer) = arr.AsSpan()

        ' Generic type parameters
        Dim list As New List(Of Span(Of Integer))  ' This should trigger BCX32061
        Dim dict As New Dictionary(Of String, Span(Of Integer))  ' This should trigger BCX32061
        Dim list2 As List(Of Span(Of Integer))  ' This should trigger BCX32061
        Dim dict2 As Dictionary(Of String, Span(Of Integer))  ' This should trigger BCX32061
        Dim list3 = New List(Of Span(Of Integer))  ' This should trigger BCX32061
        Dim dict3 = New Dictionary(Of String, Span(Of Integer))  ' This should trigger BCX32061
    End Sub

    ' Inherits declaration error
    Private Class Wrong
        Inherits List(Of Span(Of Integer))  ' This should trigger BCX32061
        Implements IEnumerable(Of Span(Of Integer))  ' This should trigger BCX32061
    End Class

    ' Event declaration error
    Event MyEvent As EventHandler(Of Span(Of Integer))  ' This should trigger BCX32061
    Event MyEvent2(gg As Action(Of Span(Of Integer)))  ' This should trigger BCX32061

    ' Field declaration error
    Private _field As List(Of Span(Of Integer))  ' This should trigger BCX32061
    Public Field As Dictionary(Of String, Span(Of Integer))  ' This should trigger BCX32061

    ' Property declaration error
    Public Property MyProp As List(Of Span(Of Integer))  ' This should trigger BCX32061
    Private Property MyProp2 As Dictionary(Of String, Span(Of Integer))  ' This should trigger BCX32061

    ' Method parameter/return value error
    Sub TestMethod3(param As List(Of Span(Of Integer)))  ' This should trigger BCX32061
    End Sub

    Sub TestMethod4(Optional param As IEnumerable(Of Span(Of Integer)) = Nothing)  ' This should trigger BCX32061
    End Sub

    Function TestFunction() As List(Of Span(Of Integer))  ' This should trigger BCX32061
        Return Nothing
    End Function

    ' Delegate declaration error
    Delegate Function TestDelegate() As List(Of Span(Of Integer))  ' This should trigger BCX32061
    Delegate Sub TestDelegate2(param As Dictionary(Of String, Span(Of Integer)))  ' This should trigger BCX32061

    Sub CorrectUsages()
        ' Correct usage of normal generics
        Dim list As New List(Of Integer)
        Dim dict As New Dictionary(Of String, Integer)
    End Sub
End Class

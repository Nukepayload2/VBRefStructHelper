<Obsolete("Suppress default ref struct obsolete errors")>
Public Class RestrictedTypeGenericDemo
    ' 演示受限类型作为泛型参数的限制 (BCX32061)

    Sub WrongUsages()
        Dim arr As Integer() = {1, 2, 3, 4, 5}
        Dim span As Span(Of Integer) = arr.AsSpan()

        ' 泛型类型参数
        Dim list As New List(Of Span(Of Integer))  ' 这应该触发 BCX32061
        Dim dict As New Dictionary(Of String, Span(Of Integer))  ' 这应该触发 BCX32061
        Dim list2 As List(Of Span(Of Integer))  ' 这应该触发 BCX32061
        Dim dict2 As Dictionary(Of String, Span(Of Integer))  ' 这应该触发 BCX32061
        Dim list3 = New List(Of Span(Of Integer))  ' 这应该触发 BCX32061
        Dim dict3 = New Dictionary(Of String, Span(Of Integer))  ' 这应该触发 BCX32061
    End Sub

    ' Inherits声明错误
    Private Class Wrong
        Inherits List(Of Span(Of Integer))  ' 这应该触发 BCX32061
        Implements IEnumerable(Of Span(Of Integer))  ' 这应该触发 BCX32061
    End Class

    ' Event声明错误
    Event MyEvent As EventHandler(Of Span(Of Integer))  ' 这应该触发 BCX32061
    Event MyEvent2(gg As Action(Of Span(Of Integer)))  ' 这应该触发 BCX32061

    ' 字段声明错误
    Private _field As List(Of Span(Of Integer))  ' 这应该触发 BCX32061
    Public Field As Dictionary(Of String, Span(Of Integer))  ' 这应该触发 BCX32061

    ' 属性声明错误
    Public Property MyProp As List(Of Span(Of Integer))  ' 这应该触发 BCX32061
    Private Property MyProp2 As Dictionary(Of String, Span(Of Integer))  ' 这应该触发 BCX32061

    ' 方法参数/返回值错误
    Sub TestMethod3(param As List(Of Span(Of Integer)))  ' 这应该触发 BCX32061
    End Sub

    Sub TestMethod4(Optional param As IEnumerable(Of Span(Of Integer)) = Nothing)  ' 这应该触发 BCX32061
    End Sub

    Function TestFunction() As List(Of Span(Of Integer))  ' 这应该触发 BCX32061
        Return Nothing
    End Function

    ' 委托声明错误
    Delegate Function TestDelegate() As List(Of Span(Of Integer))  ' 这应该触发 BCX32061
    Delegate Sub TestDelegate2(param As Dictionary(Of String, Span(Of Integer)))  ' 这应该触发 BCX32061

    Sub CorrectUsages()
        ' 正确使用普通泛型
        Dim list As New List(Of Integer)
        Dim dict As New Dictionary(Of String, Integer)
    End Sub
End Class

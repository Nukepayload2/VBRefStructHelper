<Obsolete("Suppress default ref struct obsolete errors")>
Public Class RestrictedTypeAsyncOrIteratorDemo
    ' 演示Async/Iterator方法中受限类型的使用限制 (BCX37052)

    ' Async方法中的错误使用
    Async Function TestMethod() As Task
        Dim arr As Integer() = {1, 2, 3, 4, 5}
        Dim span As Span(Of Integer) = arr.AsSpan()  ' 这应该触发 BCX37052
        Await Task.Delay(100)
    End Function

    Async Function TestMethod2() As Task
        Dim arr As Integer() = {1, 2, 3, 4, 5}
        Dim span = arr.AsSpan()  ' 这应该触发 BCX37052
        Await Task.Delay(100)
    End Function

    Async Sub TestMethod3()
        Dim arr As Integer() = {1, 2, 3, 4, 5}
        Dim span = arr.AsSpan()  ' 这应该触发 BCX37052
        Await Task.Delay(100)
    End Sub

    ' Iterator方法中的错误使用
    Iterator Function TestMethod4() As IEnumerable(Of Integer)
        Dim arr As Integer() = {1, 2, 3, 4, 5}
        Dim span As Span(Of Integer) = arr.AsSpan()  ' 这应该触发 BCX37052
        Yield span.Length
    End Function

    Iterator Function TestMethod5() As IEnumerable(Of Integer)
        Dim arr As Integer() = {1, 2, 3, 4, 5}
        Dim span = arr.AsSpan()  ' 这应该触发 BCX37052
        Yield span.Length
    End Function

    Iterator Function CorrectUsages() As IEnumerable
        ' 正常的Async/Iterator方法
        Yield 42
    End Function
End Class

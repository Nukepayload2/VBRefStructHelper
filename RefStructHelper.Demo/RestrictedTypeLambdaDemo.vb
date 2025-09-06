<Obsolete("Suppress default ref struct obsolete errors")>
Public Class RestrictedTypeLambdaDemo
    ' Demonstrate usage restrictions of restricted types in Lambda expressions (BCX36640)

    Sub WrongUsages()
        Dim arr As Integer() = {1, 2, 3, 4, 5}
        Dim span As Span(Of Integer) = arr.AsSpan()

        ' Capture restricted types in Lambda expressions
        Dim action As Action = Sub() Console.WriteLine(span.Length)  ' This should trigger BCX36640
        Dim func As Func(Of Integer) = Function() span.Length  ' This should trigger BCX36640
        Dim items = arr.Select(Function(x) span)  ' This should trigger BCX36640
        arr.ToList().ForEach(Sub(item) Console.WriteLine(span.Length))  ' This should trigger BCX36640
        Dim result = arr.Where(Function(x) span.Length > 0).ToArray()  ' This should trigger BCX36640
        Dim result2 = arr.Select(Function(x) span).ToArray()  ' This should trigger BCX36640

        ' Multi-line Lambda expressions
        Dim action2 As Action = Sub()
                                    Console.WriteLine(span.Length)  ' This should trigger BCX36640
                                End Sub

        Dim func2 As Func(Of Integer) = Function()
                                            Return span.Length  ' This should trigger BCX36640
                                        End Function

        ' Nested Lambda
        Dim func3 As Func(Of Func(Of Integer)) = Function() Function() span.Length  ' This should trigger BCX36640
    End Sub

    Sub CorrectUsages()
        ' Correct Lambda usage
        Dim arr As Integer() = {1, 2, 3, 4, 5}
        Dim action As Action = Sub() Console.WriteLine("Hello")
        Dim func As Func(Of Integer, Integer) = Function(x) x * 2
        Dim result = arr.Select(Function(x) x * 2).ToArray()

        ' Correctly declare local Span variables inside Lambda
        Dim result2 = arr.Select(Function(x)
                                     Dim localSpan As Span(Of Integer) = arr.AsSpan()
                                     Return localSpan.Length
                                 End Function).ToArray()
    End Sub
End Class

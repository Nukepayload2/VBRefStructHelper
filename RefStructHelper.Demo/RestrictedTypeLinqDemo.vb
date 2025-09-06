<Obsolete("Suppress default ref struct obsolete errors")>
Public Class RestrictedTypeLinqDemo
    ' Demonstrate usage restrictions of restricted types in LINQ (BCX36598)

    Sub WrongUsages()
        Dim arr As Integer() = {1, 2, 3, 4, 5}
        Dim span As Span(Of Integer) = arr.AsSpan()

        ' Use restricted types in LINQ queries
        Dim q1 = From a In arr Where a = span.Length  ' This should trigger BCX36598
        Dim q2 = From a In arr Let b = span.Length Select a + b  ' This should trigger BCX36598
        Dim q3 = From a In arr Select a, g = span.Length  ' This should trigger BCX36598
        Dim q4 = From a In arr Group By span.Length Into Group  ' This should trigger BCX36598

        ' LINQ range variable declaration
        Dim q5 = From a In arr Select a, g = span  ' This should trigger BCX36598
        Dim q6 = From a In arr Let b = span Select a, b  ' This should trigger BCX36598
        Dim q7 = From a In arr Select a, g = arr.AsSpan  ' This should trigger BCX36598
        Dim q8 = From a In arr Let b = arr.AsSpan Select a, b  ' This should trigger BCX36598
        Dim q9 = From a In arr Group By span Into Group  ' This should trigger BCX36598
    End Sub

    Sub CorrectUsages()
        ' Correct LINQ usage
        Dim arr As Integer() = {1, 2, 3, 4, 5}
        Dim query = From item In arr.AsSpan.ToArray Where item > 0 Select item
        Dim result = query.ToList()
    End Sub
End Class

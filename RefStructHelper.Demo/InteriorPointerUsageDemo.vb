Option Strict On

Imports Nukepayload2.CompilerServices.Unsafe

Class InteriorPointerUsageDemo
    Sub BoxPointer(p As InteriorPointer(Of Integer))
        p.ToString()
        Dim obj As Object = p
    End Sub

    Function Oops() As InteriorPointer(Of Integer)
        Dim someValue = 42
        Return VarPtr(someValue)
    End Function

#Disable Warning BCX31396 ' Restricted type cannot be used in this context
    Function SuppressErrorIfYouAreSure(p As InteriorPointer(Of Integer)) As InteriorPointer(Of Integer)
#Enable Warning BCX31396 ' Restricted type cannot be used in this context
        Return p.IncrementAndGet
    End Function
End Class

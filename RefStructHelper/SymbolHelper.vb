Imports System.Runtime.CompilerServices
Imports Microsoft.CodeAnalysis

Module SymbolHelper

    Private ReadOnly _restrictedTypeCache As New ConditionalWeakTable(Of ITypeSymbol, StrongBox(Of Boolean))

    Function IsReferenceType(typeSymbol As ITypeSymbol) As Boolean
        If typeSymbol Is Nothing Then Return False
        Dim ts = TryCast(typeSymbol, INamedTypeSymbol)
        If ts IsNot Nothing Then
            Return ts.TypeKind <> TypeKind.Structure
        End If
        Return typeSymbol.SpecialType = SpecialType.System_Object OrElse typeSymbol.SpecialType = SpecialType.System_ValueType
    End Function

    Function IsRestrictedType(typeSymbol As ITypeSymbol) As Boolean
        If typeSymbol Is Nothing Then Return False

        ' Check cache first to improve performance
        Dim cachedValue As StrongBox(Of Boolean)
        If _restrictedTypeCache.TryGetValue(typeSymbol, cachedValue) Then
            Return cachedValue.Value
        End If

        ' Check if the type has System.Runtime.CompilerServices.IsByRefLikeAttribute
        For Each attribute In typeSymbol.GetAttributes()
            If attribute.AttributeClass?.ToDisplayString() = "System.Runtime.CompilerServices.IsByRefLikeAttribute" Then
                _restrictedTypeCache.GetOrCreateValue(typeSymbol).Value = True
                Return True
            End If
        Next

        _restrictedTypeCache.GetOrCreateValue(typeSymbol).Value = False
        Return False
    End Function

End Module

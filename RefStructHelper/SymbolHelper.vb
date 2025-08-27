Imports System.Collections.Concurrent
Imports Microsoft.CodeAnalysis

Module SymbolHelper

    Function IsReferenceType(typeSymbol As ITypeSymbol) As Boolean
        If typeSymbol Is Nothing Then Return False
        Dim ts = TryCast(typeSymbol, INamedTypeSymbol)
        If ts IsNot Nothing Then
            Return ts.TypeKind <> TypeKind.Structure
        End If
        Return typeSymbol.SpecialType = SpecialType.System_Object OrElse typeSymbol.SpecialType = SpecialType.System_ValueType
    End Function

    Function IsRestrictedType(typeSymbol As ITypeSymbol, restrictedTypeCache As ConcurrentDictionary(Of ITypeSymbol, Boolean)) As Boolean
        If typeSymbol Is Nothing Then Return False

        ' Check cache first to improve performance
        Dim isRestricted As Boolean
        If restrictedTypeCache.TryGetValue(typeSymbol, isRestricted) Then
            Return isRestricted
        End If

        ' Check if the type has System.Runtime.CompilerServices.IsByRefLikeAttribute
        For Each attribute In typeSymbol.GetAttributes()
            If attribute.AttributeClass?.ToDisplayString() = "System.Runtime.CompilerServices.IsByRefLikeAttribute" Then
                restrictedTypeCache.TryAdd(typeSymbol, True)
                Return True
            End If
        Next

        restrictedTypeCache.TryAdd(typeSymbol, False)
        Return False
    End Function

End Module

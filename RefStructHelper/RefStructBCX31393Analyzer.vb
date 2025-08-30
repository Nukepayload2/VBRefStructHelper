Imports System.Collections.Concurrent
Imports System.Collections.Immutable
Imports System.Runtime.CompilerServices
Imports System.Threading
Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.Diagnostics
Imports Microsoft.CodeAnalysis.VisualBasic
Imports Microsoft.CodeAnalysis.VisualBasic.Syntax

' BCX31393: 防止调用受限类型继承的实例方法装箱受限类型
<DiagnosticAnalyzer(LanguageNames.VisualBasic)>
Public Class RefStructBCX31393Analyzer
    Inherits DiagnosticAnalyzer

    Public Const DiagnosticId = "BCX31393"

    ' You can change these strings in the Resources.resx file.
    Private Shared ReadOnly Title As New LocalizableResourceString("ERR_RestrictedAccess", My.Resources.ResourceManager, GetType(My.Resources.Resources))
    Private Shared ReadOnly MessageFormat As New LocalizableResourceString("ERR_RestrictedAccess", My.Resources.ResourceManager, GetType(My.Resources.Resources))
    Private Shared ReadOnly Description As LocalizableString = "Restricted types cannot access members inherited from Object or ValueType due to boxing limitations."
    Private Const Category As String = "Type Safety"

    Private Shared ReadOnly Rule As New DiagnosticDescriptor(
        DiagnosticId, Title, MessageFormat, Category, DiagnosticSeverity.Error, isEnabledByDefault:=True, description:=Description)

    Public Overrides ReadOnly Property SupportedDiagnostics As ImmutableArray(Of DiagnosticDescriptor) = ImmutableArray.Create(Rule)

    Public Overrides Sub Initialize(context As AnalysisContext)
        context.ConfigureGeneratedCodeAnalysis(GeneratedCodeAnalysisFlags.None)
        context.EnableConcurrentExecution()

        ' Register for compilation start action
        context.RegisterCompilationStartAction(AddressOf CompilationStartAction)
    End Sub

    Private Sub CompilationStartAction(context As CompilationStartAnalysisContext)
        ' Get references to Object and ValueType for comparison
        Dim objectType = context.Compilation.GetSpecialType(SpecialType.System_Object)
        Dim valueType = context.Compilation.GetSpecialType(SpecialType.System_ValueType)

        ' Register for member access expressions and invocations
        context.RegisterSyntaxNodeAction(Sub(ctx) AnalyzeMemberAccess(ctx, objectType, valueType), SyntaxKind.SimpleMemberAccessExpression)
        context.RegisterSyntaxNodeAction(Sub(ctx) AnalyzeInvocation(ctx, objectType, valueType), SyntaxKind.InvocationExpression)
        context.RegisterSyntaxNodeAction(AddressOf AnalyzeInterpolation, SyntaxKind.InterpolatedStringExpression)
    End Sub


    Private Sub AnalyzeMemberAccess(context As SyntaxNodeAnalysisContext, objectType As ITypeSymbol, valueType As ITypeSymbol)
        Dim memberAccess = DirectCast(context.Node, MemberAccessExpressionSyntax)
        Dim semanticModel = context.SemanticModel
        Dim cancellationToken = context.CancellationToken

        ' Check if the expression is a restricted type
        Dim expressionType = semanticModel.GetTypeInfo(memberAccess.Expression, cancellationToken).Type
        If expressionType Is Nothing OrElse Not IsRestrictedType(expressionType) Then
            Return
        End If

        ' Check if the member being accessed is inherited from Object or ValueType
        Dim symbolInfo = semanticModel.GetSymbolInfo(memberAccess, cancellationToken)
        If symbolInfo.Symbol IsNot Nothing Then
            If IsInheritedFromObjectOrValueType(symbolInfo.Symbol, objectType, valueType) Then
                ReportDiagnostic(context, memberAccess, expressionType)
            End If
        End If
    End Sub

    Private Sub AnalyzeInvocation(context As SyntaxNodeAnalysisContext, objectType As ITypeSymbol, valueType As ITypeSymbol)
        Dim invocation = DirectCast(context.Node, InvocationExpressionSyntax)
        Dim semanticModel = context.SemanticModel
        Dim cancellationToken = context.CancellationToken

        ' Check if this is a static method call like ReferenceEquals that takes restricted types as Object parameters
        Dim symbolInfo = semanticModel.GetSymbolInfo(invocation, cancellationToken)
        If symbolInfo.Symbol IsNot Nothing AndAlso TypeOf symbolInfo.Symbol Is IMethodSymbol Then
            Dim methodSymbol = DirectCast(symbolInfo.Symbol, IMethodSymbol)

            ' Check if this is ReferenceEquals or similar static method
            If methodSymbol.IsStatic AndAlso
               (methodSymbol.Parameters.Any(Function(p) SymbolEqualityComparer.Default.Equals(p.Type, objectType))) Then

                ' Check each argument to see if any restricted types are being passed as Object parameters
                If invocation.ArgumentList IsNot Nothing Then
                    For i = 0 To Math.Min(invocation.ArgumentList.Arguments.Count - 1, methodSymbol.Parameters.Length - 1)
                        Dim argument = invocation.ArgumentList.Arguments(i)
                        Dim parameter = methodSymbol.Parameters(i)

                        ' If parameter expects Object/ValueType but argument is restricted type
                        If (SymbolEqualityComparer.Default.Equals(parameter.Type, objectType) OrElse
                            SymbolEqualityComparer.Default.Equals(parameter.Type, valueType)) Then

                            If TypeOf argument Is SimpleArgumentSyntax Then
                                Dim simpleArg = DirectCast(argument, SimpleArgumentSyntax)
                                Dim argType = semanticModel.GetTypeInfo(simpleArg.Expression, cancellationToken).Type
                                If argType IsNot Nothing AndAlso IsRestrictedType(argType) Then
                                    ReportDiagnostic(context, simpleArg.Expression, argType)
                                End If
                            End If
                        End If
                    Next
                End If
            End If
        End If

        ' Also check member access invocations
        If TypeOf invocation.Expression Is MemberAccessExpressionSyntax Then
            Dim memberAccess = DirectCast(invocation.Expression, MemberAccessExpressionSyntax)
            Dim expressionType = semanticModel.GetTypeInfo(memberAccess.Expression, cancellationToken).Type

            If expressionType IsNot Nothing AndAlso IsRestrictedType(expressionType) Then
                If symbolInfo.Symbol IsNot Nothing AndAlso IsInheritedFromObjectOrValueType(symbolInfo.Symbol, objectType, valueType) Then
                    ReportDiagnostic(context, memberAccess.Expression, expressionType)
                End If
            End If
        End If
    End Sub

    Private Sub AnalyzeInterpolation(context As SyntaxNodeAnalysisContext)
        Dim interpolation = DirectCast(context.Node, InterpolatedStringExpressionSyntax)
        Dim semanticModel = context.SemanticModel
        Dim cancellationToken = context.CancellationToken

        ' Check each interpolation component
        For Each content In interpolation.Contents
            If TypeOf content Is InterpolationSyntax Then
                Dim interp = DirectCast(content, InterpolationSyntax)
                Dim expressionType = semanticModel.GetTypeInfo(interp.Expression, cancellationToken).Type

                If expressionType IsNot Nothing AndAlso IsRestrictedType(expressionType) Then
                    ' String interpolation calls ToString() which would cause boxing
                    ReportDiagnostic(context, interp.Expression, expressionType)
                End If
            End If
        Next
    End Sub

    Private Function IsInheritedFromObjectOrValueType(symbol As ISymbol, objectType As ITypeSymbol, valueType As ITypeSymbol) As Boolean
        If symbol Is Nothing Then Return False

        ' Check if this is a method or property inherited from Object or ValueType
        If TypeOf symbol Is IMethodSymbol Then
            Dim methodSymbol = DirectCast(symbol, IMethodSymbol)
            Dim containingType = methodSymbol.ContainingType

            ' Check if the method is defined in Object or ValueType
            If SymbolEqualityComparer.Default.Equals(containingType, objectType) OrElse
               SymbolEqualityComparer.Default.Equals(containingType, valueType) Then
                Return True
            End If
        End If

        If TypeOf symbol Is IPropertySymbol Then
            Dim propertySymbol = DirectCast(symbol, IPropertySymbol)
            Dim containingType = propertySymbol.ContainingType

            ' Check if the property is defined in Object or ValueType
            If SymbolEqualityComparer.Default.Equals(containingType, objectType) OrElse
               SymbolEqualityComparer.Default.Equals(containingType, valueType) Then
                Return True
            End If
        End If

        Return False
    End Function

    Private Sub ReportDiagnostic(context As SyntaxNodeAnalysisContext, syntaxNode As SyntaxNode, restrictedType As ITypeSymbol)
        Dim diagnostic As Diagnostic = Diagnostic.Create(Rule, syntaxNode.GetLocation(), restrictedType.Name)
        context.ReportDiagnostic(diagnostic)
    End Sub

End Class

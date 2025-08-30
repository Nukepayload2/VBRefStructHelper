Imports System.Collections.Concurrent
Imports System.Collections.Immutable
Imports System.Runtime.CompilerServices
Imports System.Threading
Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.Diagnostics
Imports Microsoft.CodeAnalysis.VisualBasic
Imports Microsoft.CodeAnalysis.VisualBasic.Syntax

' BCX36598: 防止 LINQ 范围变量声明或者闭包捕获受限类型
<DiagnosticAnalyzer(LanguageNames.VisualBasic)>
Public Class RefStructBCX36598Analyzer
    Inherits DiagnosticAnalyzer

    Public Const DiagnosticId = "BCX36598"

    ' You can change these strings in the Resources.resx file.
    Private Shared ReadOnly Title As New LocalizableResourceString(NameOf(My.Resources.ERR_ConstraintIsRestrictedType1), My.Resources.ResourceManager, GetType(My.Resources.Resources))
    Private Shared ReadOnly MessageFormat As New LocalizableResourceString(NameOf(My.Resources.ERR_ConstraintIsRestrictedType1), My.Resources.ResourceManager, GetType(My.Resources.Resources))
    Private Shared ReadOnly Description As New LocalizableResourceString(NameOf(My.Resources.DESC_ConstraintIsRestrictedType1), My.Resources.ResourceManager, GetType(My.Resources.Resources))
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
        ' Create a cache for restricted type checks to improve performance
        Dim restrictedTypeCache As New ConcurrentDictionary(Of ITypeSymbol, Boolean)(SymbolEqualityComparer.Default)

        ' Register for LINQ query expressions
        context.RegisterSyntaxNodeAction(AddressOf AnalyzeQueryExpression, SyntaxKind.QueryExpression)
    End Sub

    Private Sub CheckExpressionForRestrictedTypes(expression As ExpressionSyntax, context As SyntaxNodeAnalysisContext,
                                                                                                  semanticModel As SemanticModel, cancellationToken As CancellationToken)

        ' Early exit for better performance - only check specific node types
        Select Case expression.Kind()
            Case SyntaxKind.IdentifierName
                CheckIdentifierForRestrictedType(DirectCast(expression, IdentifierNameSyntax), context, semanticModel, cancellationToken)

            Case SyntaxKind.SimpleMemberAccessExpression
                CheckMemberAccessForRestrictedType(DirectCast(expression, MemberAccessExpressionSyntax), context, semanticModel, cancellationToken)

            Case SyntaxKind.InvocationExpression
                CheckInvocationForRestrictedType(DirectCast(expression, InvocationExpressionSyntax), context, semanticModel, cancellationToken)
        End Select
    End Sub

    Private Sub CheckIdentifierForRestrictedType(identifier As IdentifierNameSyntax, context As SyntaxNodeAnalysisContext,
                                                                                              semanticModel As SemanticModel, cancellationToken As CancellationToken)

        Dim symbolInfo = semanticModel.GetSymbolInfo(identifier, cancellationToken)
        If symbolInfo.Symbol IsNot Nothing Then
            Dim symbolType As ITypeSymbol = Nothing

            ' Get the type of the symbol
            Select Case symbolInfo.Symbol.Kind
                Case SymbolKind.Field
                    symbolType = DirectCast(symbolInfo.Symbol, IFieldSymbol).Type
                Case SymbolKind.Local
                    symbolType = DirectCast(symbolInfo.Symbol, ILocalSymbol).Type
                Case SymbolKind.Parameter
                    symbolType = DirectCast(symbolInfo.Symbol, IParameterSymbol).Type
                Case SymbolKind.Property
                    symbolType = DirectCast(symbolInfo.Symbol, IPropertySymbol).Type
            End Select

            If symbolType IsNot Nothing AndAlso IsRestrictedType(symbolType) Then
                ReportDiagnostic(context, identifier, symbolType)
            End If
        End If
    End Sub

    Private Sub CheckMemberAccessForRestrictedType(memberAccess As MemberAccessExpressionSyntax, context As SyntaxNodeAnalysisContext,
                                                                                                    semanticModel As SemanticModel, cancellationToken As CancellationToken)

        ' Check the expression part of member access (e.g., 'span' in 'span.Length')
        Dim expressionType = semanticModel.GetTypeInfo(memberAccess.Expression, cancellationToken).Type
        If expressionType IsNot Nothing AndAlso IsRestrictedType(expressionType) Then
            ' Additional check: if this is part of a chain that eventually returns a non-restricted type, don't report
            Dim parent = memberAccess.Parent
            If parent IsNot Nothing AndAlso (parent.IsKind(SyntaxKind.SimpleMemberAccessExpression) OrElse parent.IsKind(SyntaxKind.InvocationExpression)) Then
                ' This is part of a chain like arr.AsSpan().ToArray() or arr.AsSpan.ToArray, don't report intermediate restricted types
                Return
            End If
            ' Additional check: if the final result type is not restricted, don't report
            Dim finalType = semanticModel.GetTypeInfo(memberAccess, cancellationToken).Type
            If finalType IsNot Nothing AndAlso Not IsRestrictedType(finalType) Then
                ' The final result is not restricted, so don't report intermediate restricted types
                Return
            End If
            ReportDiagnostic(context, memberAccess.Expression, expressionType)
        End If

        ' Also check if the member access itself returns a restricted type
        Dim memberAccessType = semanticModel.GetTypeInfo(memberAccess, cancellationToken).Type
        If memberAccessType IsNot Nothing AndAlso IsRestrictedType(memberAccessType) Then
            ' Additional check: if this is part of a chain that eventually returns a non-restricted type, don't report
            Dim parent = memberAccess.Parent
            If parent IsNot Nothing AndAlso (parent.IsKind(SyntaxKind.SimpleMemberAccessExpression) OrElse parent.IsKind(SyntaxKind.InvocationExpression)) Then
                ' This is part of a chain like arr.AsSpan().ToArray() or arr.AsSpan.ToArray, don't report intermediate restricted types
                Return
            End If
            ' Additional check: if the final result type is not restricted, don't report
            Dim finalType = semanticModel.GetTypeInfo(memberAccess, cancellationToken).Type
            If finalType IsNot Nothing AndAlso Not IsRestrictedType(finalType) Then
                ' The final result is not restricted, so don't report intermediate restricted types
                Return
            End If
            ReportDiagnostic(context, memberAccess, memberAccessType)
        End If
    End Sub

    Private Sub CheckInvocationForRestrictedType(invocation As InvocationExpressionSyntax, context As SyntaxNodeAnalysisContext,
                                                                                              semanticModel As SemanticModel, cancellationToken As CancellationToken)

        ' Check if the invocation returns a restricted type (e.g., arr.AsSpan())
        Dim invocationType = semanticModel.GetTypeInfo(invocation, cancellationToken).Type
        If invocationType IsNot Nothing AndAlso IsRestrictedType(invocationType) Then
            ' Additional check: if this is part of a chain that eventually returns a non-restricted type, don't report
            Dim parent = invocation.Parent
            If parent IsNot Nothing Then
                ' Check if this is part of a member access chain
                If parent.IsKind(SyntaxKind.SimpleMemberAccessExpression) Then
                    ' This is part of a chain like arr.AsSpan().ToArray(), don't report intermediate restricted types
                    Return
                End If
                ' Check if this is part of an invocation chain (method chaining)
                If parent.IsKind(SyntaxKind.InvocationExpression) Then
                    ' This is part of a chain like arr.AsSpan().ToArray(), don't report intermediate restricted types
                    Return
                End If
            End If
            ReportDiagnostic(context, invocation, invocationType)
        End If

        ' Check arguments for restricted types
        If invocation.ArgumentList IsNot Nothing Then
            For Each argument In invocation.ArgumentList.Arguments
                If TypeOf argument Is SimpleArgumentSyntax Then
                    Dim simpleArg = DirectCast(argument, SimpleArgumentSyntax)
                    CheckExpressionForRestrictedTypes(simpleArg.Expression, context, semanticModel, cancellationToken)
                End If
            Next
        End If
    End Sub

    Private Sub AnalyzeQueryExpression(context As SyntaxNodeAnalysisContext)
        Dim queryNode = DirectCast(context.Node, QueryExpressionSyntax)
        Dim semanticModel = context.SemanticModel
        Dim cancellationToken = context.CancellationToken

        ' Search for all nodes in the query expression that could involve restricted types
        For Each node In queryNode.DescendantNodesAndSelf()
            Select Case node.Kind()
                Case SyntaxKind.IdentifierName
                    Dim identifier = DirectCast(node, IdentifierNameSyntax)
                    CheckIdentifierForRestrictedType(identifier, context, semanticModel, cancellationToken)

                Case SyntaxKind.SimpleMemberAccessExpression
                    Dim memberAccess = DirectCast(node, MemberAccessExpressionSyntax)
                    CheckMemberAccessForRestrictedType(memberAccess, context, semanticModel, cancellationToken)

                Case SyntaxKind.InvocationExpression
                    Dim invocation = DirectCast(node, InvocationExpressionSyntax)
                    CheckInvocationForRestrictedType(invocation, context, semanticModel, cancellationToken)
            End Select
        Next
    End Sub

    Private Sub ReportDiagnostic(context As SyntaxNodeAnalysisContext, syntaxNode As SyntaxNode, restrictedType As ITypeSymbol)
        Dim diagnostic As Diagnostic = Diagnostic.Create(Rule, syntaxNode.GetLocation(), restrictedType.Name)
        context.ReportDiagnostic(diagnostic)
    End Sub

End Class

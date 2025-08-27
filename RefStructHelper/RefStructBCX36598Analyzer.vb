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
    Private Shared ReadOnly Title As LocalizableString = "Instance of restricted type cannot be used in a query expression"
    Private Shared ReadOnly MessageFormat As LocalizableString = "Instance of restricted type '{0}' cannot be used in a query expression"
    Private Shared ReadOnly Description As LocalizableString = "Restricted types cannot be used in LINQ query expressions as range variables or captured in closures."
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
        context.RegisterSyntaxNodeAction(Sub(ctx) AnalyzeQueryExpression(ctx, restrictedTypeCache), SyntaxKind.QueryExpression)
    End Sub


    Private Sub AnalyzeQueryExpression(context As SyntaxNodeAnalysisContext, restrictedTypeCache As ConcurrentDictionary(Of ITypeSymbol, Boolean))
        Dim queryNode = DirectCast(context.Node, QueryExpressionSyntax)
        Dim semanticModel = context.SemanticModel
        Dim cancellationToken = context.CancellationToken

        ' Search for all nodes in the query expression that could involve restricted types
        For Each node In queryNode.DescendantNodesAndSelf()
            Select Case node.Kind()
                Case SyntaxKind.IdentifierName
                    Dim identifier = DirectCast(node, IdentifierNameSyntax)
                    CheckIdentifierForRestrictedType(identifier, context, restrictedTypeCache, semanticModel, cancellationToken)
                
                Case SyntaxKind.SimpleMemberAccessExpression
                    Dim memberAccess = DirectCast(node, MemberAccessExpressionSyntax)
                    CheckMemberAccessForRestrictedType(memberAccess, context, restrictedTypeCache, semanticModel, cancellationToken)
                
                Case SyntaxKind.InvocationExpression
                    Dim invocation = DirectCast(node, InvocationExpressionSyntax)
                    CheckInvocationForRestrictedType(invocation, context, restrictedTypeCache, semanticModel, cancellationToken)
            End Select
        Next
    End Sub

    Private Sub AnalyzeQueryClause(clause As QueryClauseSyntax, context As SyntaxNodeAnalysisContext, 
                                  restrictedTypeCache As ConcurrentDictionary(Of ITypeSymbol, Boolean),
                                  semanticModel As SemanticModel, cancellationToken As CancellationToken)
        
        Select Case clause.Kind()
            Case SyntaxKind.FromClause
                AnalyzeFromClause(DirectCast(clause, FromClauseSyntax), context, restrictedTypeCache, semanticModel, cancellationToken)
            
            Case SyntaxKind.LetClause
                AnalyzeLetClause(DirectCast(clause, LetClauseSyntax), context, restrictedTypeCache, semanticModel, cancellationToken)
            
            Case SyntaxKind.WhereClause
                AnalyzeWhereClause(DirectCast(clause, WhereClauseSyntax), context, restrictedTypeCache, semanticModel, cancellationToken)
            
            Case SyntaxKind.SelectClause
                AnalyzeSelectClause(DirectCast(clause, SelectClauseSyntax), context, restrictedTypeCache, semanticModel, cancellationToken)
            
            Case SyntaxKind.GroupByClause
                AnalyzeGroupByClause(DirectCast(clause, GroupByClauseSyntax), context, restrictedTypeCache, semanticModel, cancellationToken)
            
            Case SyntaxKind.OrderByClause
                AnalyzeOrderByClause(DirectCast(clause, OrderByClauseSyntax), context, restrictedTypeCache, semanticModel, cancellationToken)
            
            Case SyntaxKind.AggregateClause
                AnalyzeAggregateClause(DirectCast(clause, AggregateClauseSyntax), context, restrictedTypeCache, semanticModel, cancellationToken)
        End Select
    End Sub

    Private Sub AnalyzeFromClause(fromClause As FromClauseSyntax, context As SyntaxNodeAnalysisContext,
                                  restrictedTypeCache As ConcurrentDictionary(Of ITypeSymbol, Boolean),
                                  semanticModel As SemanticModel, cancellationToken As CancellationToken)
        
        For Each variable In fromClause.Variables
            ' Check the collection expression for restricted type usage
            If variable.Expression IsNot Nothing Then
                CheckExpressionForRestrictedTypes(variable.Expression, context, restrictedTypeCache, semanticModel, cancellationToken)
            End If
            
            ' Check if the range variable itself is of a restricted type
            If variable.AsClause IsNot Nothing Then
                Dim rangeVariableType = semanticModel.GetTypeInfo(variable.AsClause.Type, cancellationToken).Type
                If rangeVariableType IsNot Nothing AndAlso IsRestrictedType(rangeVariableType, restrictedTypeCache) Then
                    ReportDiagnostic(context, variable.AsClause.Type, rangeVariableType)
                End If
            End If
        Next
    End Sub

    Private Sub AnalyzeLetClause(letClause As LetClauseSyntax, context As SyntaxNodeAnalysisContext,
                                restrictedTypeCache As ConcurrentDictionary(Of ITypeSymbol, Boolean),
                                semanticModel As SemanticModel, cancellationToken As CancellationToken)
        
        For Each variable In letClause.Variables
            If variable.Expression IsNot Nothing Then
                ' Check if the expression evaluates to a restricted type (range variable declaration)
                Dim exprType = semanticModel.GetTypeInfo(variable.Expression, cancellationToken).Type
                If exprType IsNot Nothing AndAlso IsRestrictedType(exprType, restrictedTypeCache) Then
                    ReportDiagnostic(context, variable.Expression, exprType)
                End If
                
                ' Check for closure capture of restricted types within the expression
                CheckExpressionForRestrictedTypes(variable.Expression, context, restrictedTypeCache, semanticModel, cancellationToken)
            End If
        Next
    End Sub

    Private Sub AnalyzeWhereClause(whereClause As WhereClauseSyntax, context As SyntaxNodeAnalysisContext,
                                  restrictedTypeCache As ConcurrentDictionary(Of ITypeSymbol, Boolean),
                                  semanticModel As SemanticModel, cancellationToken As CancellationToken)
        
        CheckExpressionForRestrictedTypes(whereClause.Condition, context, restrictedTypeCache, semanticModel, cancellationToken)
    End Sub

    Private Sub AnalyzeSelectClause(selectClause As SelectClauseSyntax, context As SyntaxNodeAnalysisContext,
                                   restrictedTypeCache As ConcurrentDictionary(Of ITypeSymbol, Boolean),
                                   semanticModel As SemanticModel, cancellationToken As CancellationToken)
        
        For Each variable In selectClause.Variables
            If variable.Expression IsNot Nothing Then
                ' Check if the expression evaluates to a restricted type (range variable declaration)
                Dim exprType = semanticModel.GetTypeInfo(variable.Expression, cancellationToken).Type
                If exprType IsNot Nothing AndAlso IsRestrictedType(exprType, restrictedTypeCache) Then
                    ReportDiagnostic(context, variable.Expression, exprType)
                End If
                
                ' Check for closure capture of restricted types within the expression
                CheckExpressionForRestrictedTypes(variable.Expression, context, restrictedTypeCache, semanticModel, cancellationToken)
            End If
        Next
    End Sub

    Private Sub AnalyzeGroupByClause(groupByClause As GroupByClauseSyntax, context As SyntaxNodeAnalysisContext,
                                    restrictedTypeCache As ConcurrentDictionary(Of ITypeSymbol, Boolean),
                                    semanticModel As SemanticModel, cancellationToken As CancellationToken)
        
        For Each item In groupByClause.Items
            If item.Expression IsNot Nothing Then
                ' Check if the grouping key expression evaluates to a restricted type
                Dim exprType = semanticModel.GetTypeInfo(item.Expression, cancellationToken).Type
                If exprType IsNot Nothing AndAlso IsRestrictedType(exprType, restrictedTypeCache) Then
                    ReportDiagnostic(context, item.Expression, exprType)
                End If
                
                ' Check for closure capture of restricted types within the expression
                CheckExpressionForRestrictedTypes(item.Expression, context, restrictedTypeCache, semanticModel, cancellationToken)
            End If
        Next
        
        ' Check aggregation functions in the Into clause
        If groupByClause.AggregationVariables.Count > 0 Then
            For Each aggVar In groupByClause.AggregationVariables
                If aggVar.Aggregation IsNot Nothing Then
                    CheckExpressionForRestrictedTypes(aggVar.Aggregation, context, restrictedTypeCache, semanticModel, cancellationToken)
                End If
            Next
        End If
    End Sub

    Private Sub AnalyzeOrderByClause(orderByClause As OrderByClauseSyntax, context As SyntaxNodeAnalysisContext,
                                    restrictedTypeCache As ConcurrentDictionary(Of ITypeSymbol, Boolean),
                                    semanticModel As SemanticModel, cancellationToken As CancellationToken)
        
        For Each ordering In orderByClause.Orderings
            If ordering.Expression IsNot Nothing Then
                CheckExpressionForRestrictedTypes(ordering.Expression, context, restrictedTypeCache, semanticModel, cancellationToken)
            End If
        Next
    End Sub

    Private Sub AnalyzeAggregateClause(aggregateClause As AggregateClauseSyntax, context As SyntaxNodeAnalysisContext,
                                      restrictedTypeCache As ConcurrentDictionary(Of ITypeSymbol, Boolean),
                                      semanticModel As SemanticModel, cancellationToken As CancellationToken)
        
        For Each variable In aggregateClause.Variables
            If variable.Expression IsNot Nothing Then
                CheckExpressionForRestrictedTypes(variable.Expression, context, restrictedTypeCache, semanticModel, cancellationToken)
            End If
        Next
        
        If aggregateClause.AggregationVariables.Count > 0 Then
            For Each aggVar In aggregateClause.AggregationVariables
                If aggVar.Aggregation IsNot Nothing Then
                    CheckExpressionForRestrictedTypes(aggVar.Aggregation, context, restrictedTypeCache, semanticModel, cancellationToken)
                End If
            Next
        End If
    End Sub

    Private Sub CheckExpressionForRestrictedTypes(expression As ExpressionSyntax, context As SyntaxNodeAnalysisContext,
                                                 restrictedTypeCache As ConcurrentDictionary(Of ITypeSymbol, Boolean),
                                                 semanticModel As SemanticModel, cancellationToken As CancellationToken)
        
        ' Walk through all descendant nodes to find identifier references
        For Each node In expression.DescendantNodesAndSelf()
            Select Case node.Kind()
                Case SyntaxKind.IdentifierName
                    CheckIdentifierForRestrictedType(DirectCast(node, IdentifierNameSyntax), context, restrictedTypeCache, semanticModel, cancellationToken)
                
                Case SyntaxKind.SimpleMemberAccessExpression
                    CheckMemberAccessForRestrictedType(DirectCast(node, MemberAccessExpressionSyntax), context, restrictedTypeCache, semanticModel, cancellationToken)
                
                Case SyntaxKind.InvocationExpression
                    CheckInvocationForRestrictedType(DirectCast(node, InvocationExpressionSyntax), context, restrictedTypeCache, semanticModel, cancellationToken)
            End Select
        Next
    End Sub

    Private Sub CheckIdentifierForRestrictedType(identifier As IdentifierNameSyntax, context As SyntaxNodeAnalysisContext,
                                               restrictedTypeCache As ConcurrentDictionary(Of ITypeSymbol, Boolean),
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
            
            If symbolType IsNot Nothing AndAlso IsRestrictedType(symbolType, restrictedTypeCache) Then
                ReportDiagnostic(context, identifier, symbolType)
            End If
        End If
    End Sub

    Private Sub CheckMemberAccessForRestrictedType(memberAccess As MemberAccessExpressionSyntax, context As SyntaxNodeAnalysisContext,
                                                  restrictedTypeCache As ConcurrentDictionary(Of ITypeSymbol, Boolean),
                                                  semanticModel As SemanticModel, cancellationToken As CancellationToken)
        
        ' Check the expression part of member access (e.g., 'span' in 'span.Length')
        Dim expressionType = semanticModel.GetTypeInfo(memberAccess.Expression, cancellationToken).Type
        If expressionType IsNot Nothing AndAlso IsRestrictedType(expressionType, restrictedTypeCache) Then
            ReportDiagnostic(context, memberAccess.Expression, expressionType)
        End If
        
        ' Also check if the member access itself returns a restricted type
        Dim memberAccessType = semanticModel.GetTypeInfo(memberAccess, cancellationToken).Type
        If memberAccessType IsNot Nothing AndAlso IsRestrictedType(memberAccessType, restrictedTypeCache) Then
            ReportDiagnostic(context, memberAccess, memberAccessType)
        End If
    End Sub

    Private Sub CheckInvocationForRestrictedType(invocation As InvocationExpressionSyntax, context As SyntaxNodeAnalysisContext,
                                               restrictedTypeCache As ConcurrentDictionary(Of ITypeSymbol, Boolean),
                                               semanticModel As SemanticModel, cancellationToken As CancellationToken)
        
        ' Check if the invocation returns a restricted type (e.g., arr.AsSpan())
        Dim invocationType = semanticModel.GetTypeInfo(invocation, cancellationToken).Type
        If invocationType IsNot Nothing AndAlso IsRestrictedType(invocationType, restrictedTypeCache) Then
            ReportDiagnostic(context, invocation, invocationType)
        End If
        
        ' Check arguments for restricted types
        If invocation.ArgumentList IsNot Nothing Then
            For Each argument In invocation.ArgumentList.Arguments
                If TypeOf argument Is SimpleArgumentSyntax Then
                    Dim simpleArg = DirectCast(argument, SimpleArgumentSyntax)
                    CheckExpressionForRestrictedTypes(simpleArg.Expression, context, restrictedTypeCache, semanticModel, cancellationToken)
                End If
            Next
        End If
    End Sub

    Private Sub ReportDiagnostic(context As SyntaxNodeAnalysisContext, syntaxNode As SyntaxNode, restrictedType As ITypeSymbol)
        Dim diagnostic As Diagnostic = Diagnostic.Create(Rule, syntaxNode.GetLocation(), restrictedType.Name)
        context.ReportDiagnostic(diagnostic)
    End Sub

End Class
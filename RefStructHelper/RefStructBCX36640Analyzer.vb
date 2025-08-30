Imports System.Collections.Concurrent
Imports System.Collections.Immutable
Imports System.Runtime.CompilerServices
Imports System.Threading
Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.Diagnostics
Imports Microsoft.CodeAnalysis.VisualBasic
Imports Microsoft.CodeAnalysis.VisualBasic.Syntax

' BCX36640: 防止 Lambda 闭包捕获受限类型
<DiagnosticAnalyzer(LanguageNames.VisualBasic)>
Public Class RefStructBCX36640Analyzer
    Inherits DiagnosticAnalyzer

    Public Const DiagnosticId = "BCX36640"

    ' You can change these strings in the Resources.resx file.
    Private Shared ReadOnly Title As New LocalizableResourceString(NameOf(My.Resources.TITLE_CannotLiftRestrictedTypeLambda), My.Resources.ResourceManager, GetType(My.Resources.Resources))
    Private Shared ReadOnly MessageFormat As New LocalizableResourceString(NameOf(My.Resources.ERR_CannotLiftRestrictedTypeLambda), My.Resources.ResourceManager, GetType(My.Resources.Resources))
    Private Shared ReadOnly Description As New LocalizableResourceString(NameOf(My.Resources.DESC_CannotLiftRestrictedTypeLambda), My.Resources.ResourceManager, GetType(My.Resources.Resources))
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

        ' Register for Lambda expressions
        context.RegisterSyntaxNodeAction(AddressOf AnalyzeLambdaExpression, SyntaxKind.SingleLineFunctionLambdaExpression)
        context.RegisterSyntaxNodeAction(AddressOf AnalyzeLambdaExpression, SyntaxKind.SingleLineSubLambdaExpression)
        context.RegisterSyntaxNodeAction(AddressOf AnalyzeLambdaExpression, SyntaxKind.MultiLineFunctionLambdaExpression)
        context.RegisterSyntaxNodeAction(AddressOf AnalyzeLambdaExpression, SyntaxKind.MultiLineSubLambdaExpression)
    End Sub


    Private Sub AnalyzeLambdaExpression(context As SyntaxNodeAnalysisContext)
        Dim lambdaNode = DirectCast(context.Node, LambdaExpressionSyntax)
        Dim semanticModel = context.SemanticModel
        Dim cancellationToken = context.CancellationToken

        ' Get all identifiers inside the lambda expression that could be closure captures
        For Each node In lambdaNode.DescendantNodesAndSelf()
            Select Case node.Kind()
                Case SyntaxKind.IdentifierName
                    Dim identifier = DirectCast(node, IdentifierNameSyntax)
                    CheckIdentifierForClosureCapture(identifier, lambdaNode, context, semanticModel, cancellationToken)

                Case SyntaxKind.SimpleMemberAccessExpression
                    Dim memberAccess = DirectCast(node, MemberAccessExpressionSyntax)
                    CheckMemberAccessForClosureCapture(memberAccess, lambdaNode, context, semanticModel, cancellationToken)
            End Select
        Next
    End Sub

    Private Sub CheckIdentifierForClosureCapture(identifier As IdentifierNameSyntax, lambdaNode As LambdaExpressionSyntax,
                                               context As SyntaxNodeAnalysisContext,
                                               semanticModel As SemanticModel, cancellationToken As CancellationToken)

        Dim symbolInfo = semanticModel.GetSymbolInfo(identifier, cancellationToken)
        If symbolInfo.Symbol IsNot Nothing Then
            Dim symbolType As ITypeSymbol = Nothing
            Dim isClosureCapture As Boolean = False

            ' Early exit if symbol type is already known to be non-restricted
            Select Case symbolInfo.Symbol.Kind
                Case SymbolKind.Field
                    symbolType = DirectCast(symbolInfo.Symbol, IFieldSymbol).Type
                    ' Field access could be closure capture if the field is from outer scope
                    isClosureCapture = IsFromOuterScope(symbolInfo.Symbol, lambdaNode, semanticModel)

                Case SymbolKind.Local
                    Dim localSymbol = DirectCast(symbolInfo.Symbol, ILocalSymbol)
                    symbolType = localSymbol.Type
                    ' Check if local variable is defined outside the lambda (closure capture)
                    isClosureCapture = IsFromOuterScope(localSymbol, lambdaNode, semanticModel)

                Case SymbolKind.Parameter
                    Dim parameterSymbol = DirectCast(symbolInfo.Symbol, IParameterSymbol)
                    symbolType = parameterSymbol.Type
                    ' Check if parameter is from outer method (not lambda parameter)
                    isClosureCapture = IsFromOuterScope(parameterSymbol, lambdaNode, semanticModel)

                Case SymbolKind.Property
                    symbolType = DirectCast(symbolInfo.Symbol, IPropertySymbol).Type
                    ' Property access could be closure capture if accessing outer instance
                    isClosureCapture = IsFromOuterScope(symbolInfo.Symbol, lambdaNode, semanticModel)
            End Select

            ' Early exit conditions for better performance
            If isClosureCapture AndAlso symbolType IsNot Nothing AndAlso IsRestrictedType(symbolType) Then
                ReportDiagnostic(context, identifier, symbolType)
            End If
        End If
    End Sub

    Private Sub CheckMemberAccessForClosureCapture(memberAccess As MemberAccessExpressionSyntax, lambdaNode As LambdaExpressionSyntax,
                                                  context As SyntaxNodeAnalysisContext,
                                                  semanticModel As SemanticModel, cancellationToken As CancellationToken)

        ' Check the expression part of member access (e.g., 'span' in 'span.Length')
        Dim expressionType = semanticModel.GetTypeInfo(memberAccess.Expression, cancellationToken).Type
        If expressionType IsNot Nothing AndAlso IsRestrictedType(expressionType) Then
            ' Check if the expression refers to a closure capture
            If IsClosureCaptureExpression(memberAccess.Expression, lambdaNode, semanticModel, cancellationToken) Then
                ReportDiagnostic(context, memberAccess.Expression, expressionType)
            End If
        End If

        ' Also check if the member access itself returns a restricted type that's being captured
        Dim memberAccessType = semanticModel.GetTypeInfo(memberAccess, cancellationToken).Type
        If memberAccessType IsNot Nothing AndAlso IsRestrictedType(memberAccessType) Then
            ' If the member access returns a restricted type and the base expression is a closure capture
            If IsClosureCaptureExpression(memberAccess.Expression, lambdaNode, semanticModel, cancellationToken) Then
                ReportDiagnostic(context, memberAccess, memberAccessType)
            End If
        End If
    End Sub

    Private Sub CheckInvocationForClosureCapture(invocation As InvocationExpressionSyntax, lambdaNode As LambdaExpressionSyntax,
                                               context As SyntaxNodeAnalysisContext,
                                               semanticModel As SemanticModel, cancellationToken As CancellationToken)

        ' Check if the invocation returns a restricted type and involves closure capture
        Dim invocationType = semanticModel.GetTypeInfo(invocation, cancellationToken).Type
        If invocationType IsNot Nothing AndAlso IsRestrictedType(invocationType) Then
            ' Check if the invocation target involves closure capture
            If TypeOf invocation.Expression Is MemberAccessExpressionSyntax Then
                Dim memberAccess = DirectCast(invocation.Expression, MemberAccessExpressionSyntax)
                If IsClosureCaptureExpression(memberAccess.Expression, lambdaNode, semanticModel, cancellationToken) Then
                    ReportDiagnostic(context, invocation, invocationType)
                End If
            ElseIf TypeOf invocation.Expression Is IdentifierNameSyntax Then
                Dim identifier = DirectCast(invocation.Expression, IdentifierNameSyntax)
                If IsClosureCaptureExpression(identifier, lambdaNode, semanticModel, cancellationToken) Then
                    ReportDiagnostic(context, invocation, invocationType)
                End If
            End If
        End If

        ' Check arguments for closure captures of restricted types
        If invocation.ArgumentList IsNot Nothing Then
            For Each argument In invocation.ArgumentList.Arguments
                If TypeOf argument Is SimpleArgumentSyntax Then
                    Dim simpleArg = DirectCast(argument, SimpleArgumentSyntax)
                    Dim argType = semanticModel.GetTypeInfo(simpleArg.Expression, cancellationToken).Type
                    If argType IsNot Nothing AndAlso IsRestrictedType(argType) Then
                        If IsClosureCaptureExpression(simpleArg.Expression, lambdaNode, semanticModel, cancellationToken) Then
                            ReportDiagnostic(context, simpleArg.Expression, argType)
                        End If
                    End If
                End If
            Next
        End If
    End Sub

    Private Function IsFromOuterScope(symbol As ISymbol, lambdaNode As LambdaExpressionSyntax, semanticModel As SemanticModel) As Boolean
        If symbol Is Nothing Then Return False
        
        ' For local variables, check if they are declared outside the lambda
        If TypeOf symbol Is ILocalSymbol Then
            Dim localSymbol = DirectCast(symbol, ILocalSymbol)
            For Each location In localSymbol.Locations
                If location.IsInSource Then
                    Dim syntaxRef = location.SourceTree.GetRoot().FindNode(location.SourceSpan)
                    ' Check if the declaration is outside the lambda
                    If Not lambdaNode.Span.Contains(syntaxRef.Span) Then
                        Return True
                    End If
                End If
            Next
            Return False
        End If
        
        ' For parameters, check if they belong to the containing method (not lambda parameters)
        If TypeOf symbol Is IParameterSymbol Then
            Dim parameterSymbol = DirectCast(symbol, IParameterSymbol)
            Dim containingMethod = parameterSymbol.ContainingSymbol
            
            ' Get the lambda's parent method
            Dim parentMethod = GetContainingMethod(lambdaNode)
            If parentMethod IsNot Nothing Then
                Dim parentMethodSymbol = semanticModel.GetDeclaredSymbol(parentMethod)
                ' If the parameter belongs to the parent method, it's a closure capture
                Return SymbolEqualityComparer.Default.Equals(containingMethod, parentMethodSymbol)
            End If
        End If
        
        ' For fields and properties, assume they could be closure captures
        If symbol.Kind = SymbolKind.Field OrElse symbol.Kind = SymbolKind.Property Then
            Return True
        End If
        
        Return False
    End Function

    Private Function IsClosureCaptureExpression(expression As ExpressionSyntax, lambdaNode As LambdaExpressionSyntax,
                                              semanticModel As SemanticModel, cancellationToken As CancellationToken) As Boolean
        
        If TypeOf expression Is IdentifierNameSyntax Then
            Dim identifier = DirectCast(expression, IdentifierNameSyntax)
            Dim symbolInfo = semanticModel.GetSymbolInfo(identifier, cancellationToken)
            If symbolInfo.Symbol IsNot Nothing Then
                Return IsFromOuterScope(symbolInfo.Symbol, lambdaNode, semanticModel)
            End If
        End If
        
        Return False
    End Function

    Private Function GetContainingMethod(node As SyntaxNode) As MethodBlockBaseSyntax
        Dim current = node.Parent
        While current IsNot Nothing
            If TypeOf current Is MethodBlockBaseSyntax Then
                Return DirectCast(current, MethodBlockBaseSyntax)
            End If
            current = current.Parent
        End While
        Return Nothing
    End Function

    Private Sub ReportDiagnostic(context As SyntaxNodeAnalysisContext, syntaxNode As SyntaxNode, restrictedType As ITypeSymbol)
        Dim diagnostic As Diagnostic = Diagnostic.Create(Rule, syntaxNode.GetLocation(), restrictedType.Name)
        context.ReportDiagnostic(diagnostic)
    End Sub

End Class

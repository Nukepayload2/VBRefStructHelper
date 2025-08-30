Imports System.Collections.Concurrent
Imports System.Collections.Immutable
Imports System.Runtime.CompilerServices
Imports System.Threading
Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.Diagnostics
Imports Microsoft.CodeAnalysis.VisualBasic
Imports Microsoft.CodeAnalysis.VisualBasic.Syntax

' 一些工具在 SymbolHelper
<DiagnosticAnalyzer(LanguageNames.VisualBasic)>
Public Class RefStructBCX31394Analyzer
    Inherits DiagnosticAnalyzer

    Public Const DiagnosticId = "BCX31394"

    ' You can change these strings in the Resources.resx file.
    Private Shared ReadOnly Title As New LocalizableResourceString(NameOf(My.Resources.TITLE_RestrictedConversion1), My.Resources.ResourceManager, GetType(My.Resources.Resources))
    Private Shared ReadOnly MessageFormat As New LocalizableResourceString(NameOf(My.Resources.ERR_RestrictedConversion1), My.Resources.ResourceManager, GetType(My.Resources.Resources))
    Private Shared ReadOnly Description As New LocalizableResourceString(NameOf(My.Resources.DESC_RestrictedConversion1), My.Resources.ResourceManager, GetType(My.Resources.Resources))
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
        ' Register for syntax node actions to catch various conversion scenarios
        context.RegisterSyntaxNodeAction(AddressOf AnalyzeCastExpression, SyntaxKind.CTypeExpression)
        context.RegisterSyntaxNodeAction(AddressOf AnalyzeCastExpression, SyntaxKind.DirectCastExpression)
        context.RegisterSyntaxNodeAction(AddressOf AnalyzeCastExpression, SyntaxKind.TryCastExpression)
        context.RegisterSyntaxNodeAction(AddressOf AnalyzeCastExpression, SyntaxKind.PredefinedCastExpression)
        context.RegisterSyntaxNodeAction(AddressOf AnalyzeAssignment, SyntaxKind.SimpleAssignmentStatement)
        context.RegisterSyntaxNodeAction(AddressOf AnalyzeArgument, SyntaxKind.SimpleArgument)
        context.RegisterSyntaxNodeAction(AddressOf AnalyzeReturnStatement, SyntaxKind.ReturnStatement)
        context.RegisterSyntaxNodeAction(AddressOf AnalyzeLocalDeclaration, SyntaxKind.LocalDeclarationStatement)
        context.RegisterSyntaxNodeAction(AddressOf AnalyzeObjectCreation, SyntaxKind.ObjectCreationExpression)
        context.RegisterSyntaxNodeAction(AddressOf AnalyzeRaiseEvent, SyntaxKind.RaiseEventStatement)
        context.RegisterSyntaxNodeAction(AddressOf AnalyzeNamedFieldInitializer, SyntaxKind.NamedFieldInitializer)
    End Sub

    Private Sub AnalyzeRaiseEvent(context As SyntaxNodeAnalysisContext)
        Dim raiseEventNode = DirectCast(context.Node, RaiseEventStatementSyntax)
        Dim semanticModel = context.SemanticModel
        Dim cancellationToken = context.CancellationToken

        ' Get the event symbol being raised
        Dim eventSymbol = TryCast(semanticModel.GetSymbolInfo(raiseEventNode.Name, cancellationToken).Symbol, IEventSymbol)
        If eventSymbol Is Nothing Then
            Return
        End If

        ' Get the event arguments
        If raiseEventNode.ArgumentList IsNot Nothing Then
            For i = 0 To raiseEventNode.ArgumentList.Arguments.Count - 1
                Dim arg = raiseEventNode.ArgumentList.Arguments(i)
                Dim argNode = TryCast(arg, SimpleArgumentSyntax)
                If argNode Is Nothing Then Continue For
                ' Get the type of the argument expression
                Dim expressionType = semanticModel.GetTypeInfo(argNode.Expression, cancellationToken).Type

                ' Get the target parameter type from the event symbol
                Dim targetType As ITypeSymbol = Nothing
                Dim eventArgs = eventSymbol.Type
                Dim delegateMethod = TryCast(eventArgs, INamedTypeSymbol)?.DelegateInvokeMethod
                If delegateMethod Is Nothing Then Continue For
                Dim parameters = delegateMethod.Parameters
                If i < parameters.Length Then
                    targetType = parameters(i).Type
                End If

                ' Check for restricted type conversion to Object or ValueType
                CheckRestrictedConversion(expressionType, targetType, argNode, context)
            Next
        End If
    End Sub

    Private Sub AnalyzeObjectCreation(context As SyntaxNodeAnalysisContext)
        Dim creationNode = DirectCast(context.Node, ObjectCreationExpressionSyntax)
        Dim semanticModel = context.SemanticModel
        Dim cancellationToken = context.CancellationToken

        ' Check each argument in the object creation
        If creationNode.ArgumentList Is Nothing Then
            Return
        End If

        For Each arg In creationNode.ArgumentList.Arguments
            Dim argNode = TryCast(arg, SimpleArgumentSyntax)
            If argNode Is Nothing Then
                Continue For
            End If

            ' Get the type of the argument expression
            Dim expressionType = semanticModel.GetTypeInfo(argNode.Expression, cancellationToken).Type

            ' Try to determine the target parameter type
            Dim targetType As ITypeSymbol = Nothing
            Dim parameter = GetParameterForArgument(argNode, semanticModel, cancellationToken)
            If parameter IsNot Nothing Then
                targetType = parameter.Type
            End If

            ' Check for restricted type conversion to Object or ValueType
            CheckRestrictedConversion(expressionType, targetType, argNode, context)
        Next
    End Sub


    Private Sub AnalyzeCastExpression(context As SyntaxNodeAnalysisContext)
        Dim semanticModel = context.SemanticModel
        Dim cancellationToken = context.CancellationToken

        ' Handle different types of cast expressions
        Select Case context.Node.Kind()
            Case SyntaxKind.CTypeExpression, SyntaxKind.DirectCastExpression, SyntaxKind.TryCastExpression
                Dim castNode = DirectCast(context.Node, CastExpressionSyntax)
                ' Get the type of the expression being cast
                Dim expressionType = semanticModel.GetTypeInfo(castNode.Expression, cancellationToken).Type
                Dim targetType = semanticModel.GetTypeInfo(castNode.Type, cancellationToken).Type

                ' Check for restricted type conversion to Object or ValueType
                CheckRestrictedConversion(expressionType, targetType, castNode, context)

            Case SyntaxKind.PredefinedCastExpression
                Dim castNode = DirectCast(context.Node, PredefinedCastExpressionSyntax)
                ' Get the type of the expression being cast
                Dim expressionType = semanticModel.GetTypeInfo(castNode.Expression, cancellationToken).Type

                ' For predefined casts like CObj, we need to determine the target type
                ' CObj converts to Object, so we can get the Object type from the compilation
                Dim objectType = semanticModel.Compilation.GetSpecialType(SpecialType.System_Object)

                ' Check for restricted type conversion to Object
                CheckRestrictedConversion(expressionType, objectType, castNode, context)
        End Select
    End Sub

    Private Sub AnalyzeAssignment(context As SyntaxNodeAnalysisContext)
        Dim assignmentNode = DirectCast(context.Node, AssignmentStatementSyntax)
        Dim semanticModel = context.SemanticModel
        Dim cancellationToken = context.CancellationToken

        ' Regular assignment
        Dim expressionType = semanticModel.GetTypeInfo(assignmentNode.Right, cancellationToken).Type
        Dim targetType = semanticModel.GetTypeInfo(assignmentNode.Left, cancellationToken).Type

        ' Check for restricted type conversion to Object or ValueType
        CheckRestrictedConversion(expressionType, targetType, assignmentNode.Right, context)
    End Sub

    Private Sub AnalyzeArgument(context As SyntaxNodeAnalysisContext)
        Dim argNode = DirectCast(context.Node, SimpleArgumentSyntax)
        Dim semanticModel = context.SemanticModel
        Dim cancellationToken = context.CancellationToken

        ' Get the type of the argument expression
        Dim expressionType = semanticModel.GetTypeInfo(argNode.Expression, cancellationToken).Type

        ' Try to determine the target parameter type
        Dim targetType As ITypeSymbol = Nothing
        Dim parameter = GetParameterForArgument(argNode, semanticModel, cancellationToken)
        If parameter IsNot Nothing Then
            targetType = parameter.Type
        End If

        ' Check for restricted type conversion to Object or ValueType
        If targetType IsNot Nothing Then
            CheckRestrictedConversion(expressionType, targetType, argNode, context)
        End If
    End Sub

    Private Sub AnalyzeReturnStatement(context As SyntaxNodeAnalysisContext)
        Dim returnNode = DirectCast(context.Node, ReturnStatementSyntax)
        Dim semanticModel = context.SemanticModel
        Dim cancellationToken = context.CancellationToken

        If returnNode.Expression Is Nothing Then
            Return
        End If

        ' Get the return expression type
        Dim expressionType = semanticModel.GetTypeInfo(returnNode.Expression, cancellationToken).Type

        ' Try to get the return type of the containing method
        Dim containingSymbol = semanticModel.GetEnclosingSymbol(returnNode.SpanStart, cancellationToken)
        Dim methodSymbol As IMethodSymbol = TryCast(containingSymbol, IMethodSymbol)
        If methodSymbol Is Nothing Then
            methodSymbol = TryCast(containingSymbol.ContainingSymbol, IMethodSymbol)
        End If

        If methodSymbol IsNot Nothing Then
            Dim returnType = methodSymbol.ReturnType
            ' Check for restricted type conversion to Object or ValueType
            CheckRestrictedConversion(expressionType, returnType, returnNode.Expression, context)
        End If
    End Sub

    Private Sub AnalyzeLocalDeclaration(context As SyntaxNodeAnalysisContext)
        Dim localDeclNode = DirectCast(context.Node, LocalDeclarationStatementSyntax)
        Dim semanticModel = context.SemanticModel
        Dim cancellationToken = context.CancellationToken

        ' Check each declarator in the declaration
        For Each declarator In localDeclNode.Declarators
            ' Check if there's an initializer
            If declarator.Initializer IsNot Nothing Then
                ' Get the type of the variable being declared
                Dim variableType As ITypeSymbol
                If declarator.AsClause IsNot Nothing Then
                    ' Get the declared type from AsClause
                    variableType = semanticModel.GetTypeInfo(declarator.AsClause.Type, cancellationToken).Type
                Else
                    ' Infer the type from the initializer
                    variableType = semanticModel.GetTypeInfo(declarator.Initializer.Value, cancellationToken).Type
                End If

                ' Regular initialization
                ' Get the type of the initializer expression
                Dim initializerType = semanticModel.GetTypeInfo(declarator.Initializer.Value, cancellationToken).Type

                ' Check for restricted type conversion to Object or ValueType
                CheckRestrictedConversion(initializerType, variableType, declarator.Initializer.Value, context)
            End If
        Next
    End Sub

    Private Sub CheckRestrictedConversion(expressionType As ITypeSymbol,
                                          targetType As ITypeSymbol,
                                          node As SyntaxNode,
                                          context As SyntaxNodeAnalysisContext,
                                          <CallerMemberName> Optional callerName As String = Nothing)
        ' Early exit conditions to improve performance
        If expressionType Is Nothing OrElse targetType Is Nothing Then Return

        ' Early exit if target is not a reference type - no need to check restricted type
        If Not IsReferenceType(targetType) Then Return

        ' Check if this is a restricted type being converted to Object or ValueType
        If IsRestrictedType(expressionType) Then
            Dim diagnostic As Diagnostic = Diagnostic.Create(Rule, node.GetLocation(), expressionType.Name)
            context.ReportDiagnostic(diagnostic)
        End If
    End Sub

    Private Function GetParameterForArgument(argNode As SimpleArgumentSyntax, semanticModel As SemanticModel, cancellationToken As CancellationToken) As IParameterSymbol
        ' Try to determine which parameter this argument corresponds to
        ' This requires walking up the syntax tree to find the invocation or object creation

        Dim parent = argNode.Parent
        While parent IsNot Nothing
            Dim argumentList As ArgumentListSyntax = Nothing
            Dim methodSymbol As IMethodSymbol = Nothing

            Dim symbolInfo = semanticModel.GetSymbolInfo(parent, cancellationToken)
            If symbolInfo.Symbol IsNot Nothing Then
                methodSymbol = TryCast(symbolInfo.Symbol, IMethodSymbol)
                argumentList = If(TryCast(parent, InvocationExpressionSyntax)?.ArgumentList, TryCast(parent, ObjectCreationExpressionSyntax)?.ArgumentList)
            End If

            If argumentList IsNot Nothing AndAlso methodSymbol IsNot Nothing Then
                Dim args = argumentList.Arguments
                Dim argIndex As Integer = -1
                For i As Integer = 0 To args.Count - 1
                    If args(i) Is argNode Then
                        argIndex = i
                        Exit For
                    End If
                Next

                If argIndex >= 0 AndAlso argIndex < methodSymbol.Parameters.Length Then
                    Return methodSymbol.Parameters(argIndex)
                End If
            End If

            parent = parent.Parent
        End While

        Return Nothing
    End Function

    Private Sub AnalyzeNamedFieldInitializer(context As SyntaxNodeAnalysisContext)
        Dim fieldInitNode = DirectCast(context.Node, NamedFieldInitializerSyntax)
        Dim semanticModel = context.SemanticModel
        Dim cancellationToken = context.CancellationToken

        ' Try to determine the target field type
        Dim fieldSymbol = semanticModel.GetSymbolInfo(fieldInitNode.Name, cancellationToken).Symbol
        Dim targetType As ITypeSymbol = Nothing
        If fieldSymbol IsNot Nothing Then
            targetType = If(TryCast(fieldSymbol, IFieldSymbol)?.Type, TryCast(fieldSymbol, IPropertySymbol)?.Type)
        End If
        Dim expressionSymbol = TryCast(semanticModel.GetSymbolInfo(fieldInitNode.Expression, cancellationToken).Symbol, ILocalSymbol)
        CheckRestrictedConversion(expressionSymbol.Type, targetType, fieldInitNode.Expression, context)
    End Sub
End Class

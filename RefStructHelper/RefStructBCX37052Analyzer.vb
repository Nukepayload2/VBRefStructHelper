Imports System.Collections.Concurrent
Imports System.Collections.Immutable
Imports System.Runtime.CompilerServices
Imports System.Threading
Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.Diagnostics
Imports Microsoft.CodeAnalysis.VisualBasic
Imports Microsoft.CodeAnalysis.VisualBasic.Syntax

' BCX37052: 编译为 Async/Iterator 状态机的函数，不允许受限类型变量
<DiagnosticAnalyzer(LanguageNames.VisualBasic)>
Public Class RefStructBCX37052Analyzer
    Inherits DiagnosticAnalyzer

    Public Const DiagnosticId = "BCX37052"

    ' You can change these strings in the Resources.resx file.
    Private Shared ReadOnly Title As New LocalizableResourceString(NameOf(My.Resources.ERR_CannotLiftRestrictedTypeResumable1), My.Resources.ResourceManager, GetType(My.Resources.Resources))
    Private Shared ReadOnly MessageFormat As New LocalizableResourceString(NameOf(My.Resources.ERR_CannotLiftRestrictedTypeResumable1), My.Resources.ResourceManager, GetType(My.Resources.Resources))
    Private Shared ReadOnly Description As LocalizableString = "Restricted types cannot be declared as local variables in Async or Iterator methods due to state machine limitations."
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

        ' Register for local variable declarations in method blocks
        context.RegisterSyntaxNodeAction(AddressOf AnalyzeMethodBlock, SyntaxKind.FunctionBlock)
        context.RegisterSyntaxNodeAction(AddressOf AnalyzeMethodBlock, SyntaxKind.SubBlock)
    End Sub


    Private Sub AnalyzeMethodBlock(context As SyntaxNodeAnalysisContext)
        Dim methodBlock = DirectCast(context.Node, MethodBlockBaseSyntax)
        Dim semanticModel = context.SemanticModel
        Dim cancellationToken = context.CancellationToken

        ' Check if this method is Async or Iterator
        If Not IsAsyncOrIteratorMethod(methodBlock) Then
            Return
        End If

        ' Find all local variable declarations in this method
        For Each node In methodBlock.DescendantNodes()
            If node.IsKind(SyntaxKind.VariableDeclarator) Then
                Dim variableDeclarator = DirectCast(node, VariableDeclaratorSyntax)
                AnalyzeVariableDeclarator(variableDeclarator, context, semanticModel, cancellationToken)
            End If
        Next

        ' Check method parameters
        Dim methodStatement As MethodStatementSyntax = Nothing
        Select Case methodBlock.Kind()
            Case SyntaxKind.FunctionBlock
                Dim functionBlock = DirectCast(methodBlock, MethodBlockSyntax)
                methodStatement = DirectCast(functionBlock.BlockStatement, MethodStatementSyntax)
            Case SyntaxKind.SubBlock
                Dim subBlock = DirectCast(methodBlock, MethodBlockSyntax)
                methodStatement = DirectCast(subBlock.BlockStatement, MethodStatementSyntax)
        End Select

        If methodStatement IsNot Nothing AndAlso methodStatement.ParameterList IsNot Nothing Then
            For Each parameter In methodStatement.ParameterList.Parameters
                AnalyzeParameter(parameter, context, semanticModel, cancellationToken)
            Next
        End If
    End Sub

    Private Sub AnalyzeParameter(parameter As ParameterSyntax, context As SyntaxNodeAnalysisContext,
                                                              semanticModel As SemanticModel, cancellationToken As CancellationToken)
        
        If parameter.AsClause IsNot Nothing Then
            Dim parameterType = semanticModel.GetTypeInfo(parameter.AsClause.Type, cancellationToken).Type
            If parameterType IsNot Nothing AndAlso IsRestrictedType(parameterType) Then
                ReportDiagnostic(context, parameter.AsClause.Type, parameterType)
            End If
        End If
    End Sub

    Private Function IsAsyncOrIteratorMethod(methodBlock As MethodBlockBaseSyntax) As Boolean
        ' Check method modifiers for Async or Iterator keywords
        Dim methodStatement As MethodStatementSyntax = Nothing
        
        Select Case methodBlock.Kind()
            Case SyntaxKind.FunctionBlock
                Dim functionBlock = DirectCast(methodBlock, MethodBlockSyntax)
                methodStatement = DirectCast(functionBlock.BlockStatement, MethodStatementSyntax)
            Case SyntaxKind.SubBlock
                Dim subBlock = DirectCast(methodBlock, MethodBlockSyntax)
                methodStatement = DirectCast(subBlock.BlockStatement, MethodStatementSyntax)
        End Select

        If methodStatement IsNot Nothing Then
            For Each modifier In methodStatement.Modifiers
                If modifier.IsKind(SyntaxKind.AsyncKeyword) OrElse modifier.IsKind(SyntaxKind.IteratorKeyword) Then
                    Return True
                End If
            Next
        End If

        Return False
    End Function

    Private Sub AnalyzeVariableDeclarator(variableDeclarator As VariableDeclaratorSyntax, context As SyntaxNodeAnalysisContext,
                                                                                  semanticModel As SemanticModel, cancellationToken As CancellationToken)
        
        For Each name In variableDeclarator.Names
            Dim symbolInfo = semanticModel.GetSymbolInfo(name, cancellationToken)
            If symbolInfo.Symbol IsNot Nothing AndAlso TypeOf symbolInfo.Symbol Is ILocalSymbol Then
                Dim localSymbol = DirectCast(symbolInfo.Symbol, ILocalSymbol)
                Dim variableType = localSymbol.Type
                
                If variableType IsNot Nothing AndAlso IsRestrictedType(variableType) Then
                    ReportDiagnostic(context, name, variableType)
                    Return ' Only report once per declarator
                End If
            End If
        Next
        
        ' Also check for inferred types from initializers
        If variableDeclarator.Initializer IsNot Nothing AndAlso variableDeclarator.AsClause Is Nothing Then
            ' This is a Dim x = expression case with type inference
            Dim initializerType = semanticModel.GetTypeInfo(variableDeclarator.Initializer.Value, cancellationToken).Type
            If initializerType IsNot Nothing AndAlso IsRestrictedType(initializerType) Then
                ReportDiagnostic(context, variableDeclarator.Names.First(), initializerType)
            End If
        End If
        
        ' Check explicit type declarations
        If variableDeclarator.AsClause IsNot Nothing AndAlso variableDeclarator.AsClause.Type IsNot Nothing Then
            Dim explicitType = semanticModel.GetTypeInfo(variableDeclarator.AsClause.Type, cancellationToken).Type
            If explicitType IsNot Nothing AndAlso IsRestrictedType(explicitType) Then
                ReportDiagnostic(context, variableDeclarator.Names.First(), explicitType)
            End If
        End If
    End Sub

    Private Sub ReportDiagnostic(context As SyntaxNodeAnalysisContext, syntaxNode As SyntaxNode, restrictedType As ITypeSymbol)
        Dim diagnostic As Diagnostic = Diagnostic.Create(Rule, syntaxNode.GetLocation(), restrictedType.Name)
        context.ReportDiagnostic(diagnostic)
    End Sub

End Class

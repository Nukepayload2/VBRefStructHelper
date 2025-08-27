Imports System.Collections.Concurrent
Imports System.Collections.Immutable
Imports System.Runtime.CompilerServices
Imports System.Threading
Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.Diagnostics
Imports Microsoft.CodeAnalysis.VisualBasic
Imports Microsoft.CodeAnalysis.VisualBasic.Syntax

' BCX32061: 受限类型不能作为泛型参数
<DiagnosticAnalyzer(LanguageNames.VisualBasic)>
Public Class RefStructBCX32061Analyzer
    Inherits DiagnosticAnalyzer

    Public Const DiagnosticId = "BCX32061"

    ' You can change these strings in the Resources.resx file.
    Private Shared ReadOnly Title As LocalizableString = "Restricted type cannot be used as generic type argument"
    Private Shared ReadOnly MessageFormat As LocalizableString = "'{0}' cannot be used as a generic type argument. Restricted types cannot be used as generic type arguments."
    Private Shared ReadOnly Description As LocalizableString = "Restricted types cannot be used as generic type arguments."
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

        ' Register for various syntax node actions to catch different scenarios
        context.RegisterSyntaxNodeAction(Sub(ctx) AnalyzeGenericName(ctx, restrictedTypeCache), SyntaxKind.GenericName)
        context.RegisterSyntaxNodeAction(Sub(ctx) AnalyzeTypeConstraint(ctx, restrictedTypeCache), SyntaxKind.TypeConstraint)
    End Sub


    Private Sub AnalyzeGenericName(context As SyntaxNodeAnalysisContext, restrictedTypeCache As ConcurrentDictionary(Of ITypeSymbol, Boolean))
        Dim genericNameNode = DirectCast(context.Node, GenericNameSyntax)
        Dim semanticModel = context.SemanticModel
        Dim cancellationToken = context.CancellationToken

        ' Check each type argument in the generic type
        If genericNameNode.TypeArgumentList IsNot Nothing Then
            For Each typeArgument In genericNameNode.TypeArgumentList.Arguments
                Dim typeArgType = semanticModel.GetTypeInfo(typeArgument, cancellationToken).Type
                If typeArgType IsNot Nothing AndAlso IsRestrictedType(typeArgType, restrictedTypeCache) Then
                    Dim diagnostic As Diagnostic = Diagnostic.Create(Rule, typeArgument.GetLocation(), typeArgType.Name)
                    context.ReportDiagnostic(diagnostic)
                End If
            Next
        End If
    End Sub

    Private Sub AnalyzeTypeConstraint(context As SyntaxNodeAnalysisContext, restrictedTypeCache As ConcurrentDictionary(Of ITypeSymbol, Boolean))
        Dim constraintNode = DirectCast(context.Node, TypeConstraintSyntax)
        Dim semanticModel = context.SemanticModel
        Dim cancellationToken = context.CancellationToken

        ' Check if the constraint type is a restricted type
        Dim constraintType = semanticModel.GetTypeInfo(constraintNode.Type, cancellationToken).Type
        If constraintType IsNot Nothing AndAlso IsRestrictedType(constraintType, restrictedTypeCache) Then
            Dim diagnostic As Diagnostic = Diagnostic.Create(Rule, constraintNode.Type.GetLocation(), constraintType.Name)
            context.ReportDiagnostic(diagnostic)
        End If
    End Sub

End Class
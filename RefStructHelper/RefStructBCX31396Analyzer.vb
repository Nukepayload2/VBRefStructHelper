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
Public Class RefStructBCX31396Analyzer
    Inherits DiagnosticAnalyzer

    Public Const DiagnosticId = "BCX31396"

    ' You can change these strings in the Resources.resx file.
    Private Shared ReadOnly Title As LocalizableString = "Restricted type cannot be used in this context"
    Private Shared ReadOnly MessageFormat As LocalizableString = "'{0}' cannot be made nullable, and cannot be used as the data type of an array element, field, anonymous type member, type argument, 'ByRef' parameter, or return statement"
    Private Shared ReadOnly Description As LocalizableString = "Restricted types cannot be made nullable, used as array elements, fields, anonymous type members, type arguments, ByRef parameters, or return values."
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
        ' Check if OptionRestrict is enabled
        Dim compilation = context.Compilation
        If Not IsOptionRestrictEnabled(compilation) Then
            Return
        End If

        ' Create a cache for restricted type checks to improve performance
        Dim restrictedTypeCache As New ConcurrentDictionary(Of ITypeSymbol, Boolean)(SymbolEqualityComparer.Default)

        ' Register for various syntax node actions to catch different scenarios
        context.RegisterSyntaxNodeAction(Sub(ctx) AnalyzeNullableType(ctx, restrictedTypeCache), SyntaxKind.NullableType)
        context.RegisterSyntaxNodeAction(Sub(ctx) AnalyzeArrayType(ctx, restrictedTypeCache), SyntaxKind.ArrayType)
        context.RegisterSyntaxNodeAction(Sub(ctx) AnalyzeFieldDeclaration(ctx, restrictedTypeCache), SyntaxKind.FieldDeclaration)
        context.RegisterSyntaxNodeAction(Sub(ctx) AnalyzePropertyDeclaration(ctx, restrictedTypeCache), SyntaxKind.PropertyStatement)
        context.RegisterSyntaxNodeAction(Sub(ctx) AnalyzePropertyBlock(ctx, restrictedTypeCache), SyntaxKind.PropertyBlock)
        context.RegisterSyntaxNodeAction(Sub(ctx) AnalyzeParameter(ctx, restrictedTypeCache), SyntaxKind.Parameter)
        context.RegisterSyntaxNodeAction(Sub(ctx) AnalyzeAnonymousObjectCreation(ctx, restrictedTypeCache), SyntaxKind.AnonymousObjectCreationExpression)
        context.RegisterSyntaxNodeAction(Sub(ctx) AnalyzeObjectCreation(ctx, restrictedTypeCache), SyntaxKind.ObjectCreationExpression)
        context.RegisterSyntaxNodeAction(Sub(ctx) AnalyzeCollectionInitializer(ctx, restrictedTypeCache), SyntaxKind.CollectionInitializer)
        context.RegisterSyntaxNodeAction(Sub(ctx) AnalyzeArrayLiteral(ctx, restrictedTypeCache), SyntaxKind.CollectionInitializer)
        context.RegisterSyntaxNodeAction(Sub(ctx) AnalyzeLocalDeclaration(ctx, restrictedTypeCache), SyntaxKind.LocalDeclarationStatement)
        context.RegisterSyntaxNodeAction(Sub(ctx) AnalyzeVariableDeclarator(ctx, restrictedTypeCache), SyntaxKind.VariableDeclarator)
        context.RegisterSyntaxNodeAction(Sub(ctx) AnalyzeEqualsValue(ctx, restrictedTypeCache), SyntaxKind.EqualsValue)
        context.RegisterSyntaxNodeAction(Sub(ctx) AnalyzeObjectCollectionInitializer(ctx, restrictedTypeCache), SyntaxKind.ObjectCollectionInitializer)
    End Sub

    Private Function IsOptionRestrictEnabled(compilation As Compilation) As Boolean
        ' Check if <OptionRestrict>On</OptionRestrict> is set in the project
        ' For now, assume it's enabled for testing
        Return True
    End Function

    Private Sub AnalyzeNullableType(context As SyntaxNodeAnalysisContext, restrictedTypeCache As ConcurrentDictionary(Of ITypeSymbol, Boolean))
        Dim nullableTypeNode = DirectCast(context.Node, NullableTypeSyntax)
        Dim semanticModel = context.SemanticModel
        Dim cancellationToken = context.CancellationToken

        ' Get the underlying type
        Dim underlyingType = semanticModel.GetTypeInfo(nullableTypeNode.ElementType, cancellationToken).Type
        If underlyingType IsNot Nothing AndAlso IsRestrictedType(underlyingType, restrictedTypeCache) Then
            Dim diagnostic As Diagnostic = Diagnostic.Create(Rule, nullableTypeNode.GetLocation(), underlyingType.Name)
            context.ReportDiagnostic(diagnostic)
        End If
    End Sub

    Private Sub AnalyzeArrayType(context As SyntaxNodeAnalysisContext, restrictedTypeCache As ConcurrentDictionary(Of ITypeSymbol, Boolean))
        Dim arrayTypeNode = DirectCast(context.Node, ArrayTypeSyntax)
        Dim semanticModel = context.SemanticModel
        Dim cancellationToken = context.CancellationToken

        ' Get the element type
        Dim elementType = semanticModel.GetTypeInfo(arrayTypeNode.ElementType, cancellationToken).Type
        If elementType IsNot Nothing AndAlso IsRestrictedType(elementType, restrictedTypeCache) Then
            Dim diagnostic As Diagnostic = Diagnostic.Create(Rule, arrayTypeNode.GetLocation(), elementType.Name)
            context.ReportDiagnostic(diagnostic)
        End If
    End Sub

    Private Sub AnalyzeFieldDeclaration(context As SyntaxNodeAnalysisContext, restrictedTypeCache As ConcurrentDictionary(Of ITypeSymbol, Boolean))
        Dim fieldDeclNode = DirectCast(context.Node, FieldDeclarationSyntax)
        Dim semanticModel = context.SemanticModel
        Dim cancellationToken = context.CancellationToken

        ' Check each declarator in the field declaration
        For Each declarator In fieldDeclNode.Declarators
            If declarator.AsClause IsNot Nothing Then
                Dim fieldType = semanticModel.GetTypeInfo(declarator.AsClause.Type, cancellationToken).Type
                If fieldType IsNot Nothing AndAlso IsRestrictedType(fieldType, restrictedTypeCache) Then
                    ' Check if the containing type is a class (not a restricted struct)
                    Dim containingTypeSymbol = TryCast(semanticModel.GetDeclaredSymbol(fieldDeclNode.Parent, cancellationToken), INamedTypeSymbol)
                    If containingTypeSymbol IsNot Nothing Then
                        ' For classes, report diagnostic. For structs, only report if the struct is not restricted
                        If containingTypeSymbol.TypeKind = TypeKind.Class Then
                            Dim diagnostic As Diagnostic = Diagnostic.Create(Rule, declarator.AsClause.Type.GetLocation(), fieldType.Name)
                            context.ReportDiagnostic(diagnostic)
                        ElseIf containingTypeSymbol.TypeKind = TypeKind.Structure AndAlso Not IsRestrictedType(containingTypeSymbol, restrictedTypeCache) Then
                            Dim diagnostic As Diagnostic = Diagnostic.Create(Rule, declarator.AsClause.Type.GetLocation(), fieldType.Name)
                            context.ReportDiagnostic(diagnostic)
                        End If
                    End If
                End If
            End If
        Next
    End Sub

    Private Sub AnalyzePropertyDeclaration(context As SyntaxNodeAnalysisContext, restrictedTypeCache As ConcurrentDictionary(Of ITypeSymbol, Boolean))
        Dim propertyNode = DirectCast(context.Node, PropertyStatementSyntax)
        Dim semanticModel = context.SemanticModel
        Dim cancellationToken = context.CancellationToken

        If propertyNode.AsClause IsNot Nothing Then
            Dim propertyType = semanticModel.GetTypeInfo(propertyNode.AsClause.Type, cancellationToken).Type
            If propertyType IsNot Nothing AndAlso IsRestrictedType(propertyType, restrictedTypeCache) Then
                ' Check if the containing type is a class (not a restricted struct)
                Dim containingTypeSymbol = TryCast(semanticModel.GetDeclaredSymbol(propertyNode.Parent, cancellationToken), INamedTypeSymbol)
                If containingTypeSymbol IsNot Nothing Then
                    ' For classes, report diagnostic. For structs, only report if the struct is not restricted
                    If containingTypeSymbol.TypeKind = TypeKind.Class Then
                        Dim diagnostic As Diagnostic = Diagnostic.Create(Rule, propertyNode.AsClause.Type.GetLocation(), propertyType.Name)
                        context.ReportDiagnostic(diagnostic)
                    ElseIf containingTypeSymbol.TypeKind = TypeKind.Structure AndAlso Not IsRestrictedType(containingTypeSymbol, restrictedTypeCache) Then
                        Dim diagnostic As Diagnostic = Diagnostic.Create(Rule, propertyNode.AsClause.Type.GetLocation(), propertyType.Name)
                        context.ReportDiagnostic(diagnostic)
                    End If
                End If
            End If
        End If
    End Sub

    Private Sub AnalyzePropertyBlock(context As SyntaxNodeAnalysisContext, restrictedTypeCache As ConcurrentDictionary(Of ITypeSymbol, Boolean))
        Dim propertyBlockNode = DirectCast(context.Node, PropertyBlockSyntax)
        Dim semanticModel = context.SemanticModel
        Dim cancellationToken = context.CancellationToken

        ' Analyze the property statement
        If propertyBlockNode.PropertyStatement.AsClause IsNot Nothing Then
            Dim propertyType = semanticModel.GetTypeInfo(propertyBlockNode.PropertyStatement.AsClause.Type, cancellationToken).Type
            If propertyType IsNot Nothing AndAlso IsRestrictedType(propertyType, restrictedTypeCache) Then
                ' Check if the containing type is a class (not a restricted struct)
                Dim containingTypeSymbol = TryCast(semanticModel.GetDeclaredSymbol(propertyBlockNode.Parent, cancellationToken), INamedTypeSymbol)
                If containingTypeSymbol IsNot Nothing Then
                    ' For classes, report diagnostic. For structs, report diagnostic if the struct is restricted (IsByRefLike)
                    If containingTypeSymbol.TypeKind = TypeKind.Class Then
                        Dim diagnostic As Diagnostic = Diagnostic.Create(Rule, propertyBlockNode.PropertyStatement.AsClause.Type.GetLocation(), propertyType.Name)
                        context.ReportDiagnostic(diagnostic)
                    ElseIf containingTypeSymbol.TypeKind = TypeKind.Structure Then
                        ' For structures, always report the diagnostic for restricted types
                        Dim diagnostic As Diagnostic = Diagnostic.Create(Rule, propertyBlockNode.PropertyStatement.AsClause.Type.GetLocation(), propertyType.Name)
                        context.ReportDiagnostic(diagnostic)
                    End If
                End If
            End If
        End If
    End Sub

    Private Sub AnalyzeParameter(context As SyntaxNodeAnalysisContext, restrictedTypeCache As ConcurrentDictionary(Of ITypeSymbol, Boolean))
        Dim parameterNode = DirectCast(context.Node, ParameterSyntax)
        Dim semanticModel = context.SemanticModel
        Dim cancellationToken = context.CancellationToken

        If parameterNode.AsClause IsNot Nothing Then
            Dim parameterType = semanticModel.GetTypeInfo(parameterNode.AsClause.Type, cancellationToken).Type
            If parameterType IsNot Nothing AndAlso IsRestrictedType(parameterType, restrictedTypeCache) Then
                ' Check if it's a ByRef parameter
                If parameterNode.Modifiers.Any(Function(m) m.IsKind(SyntaxKind.ByRefKeyword)) Then
                    Dim diagnostic As Diagnostic = Diagnostic.Create(Rule, parameterNode.AsClause.Type.GetLocation(), parameterType.Name)
                    context.ReportDiagnostic(diagnostic)
                End If
            End If
        End If
    End Sub

    Private Sub AnalyzeAnonymousObjectCreation(context As SyntaxNodeAnalysisContext, restrictedTypeCache As ConcurrentDictionary(Of ITypeSymbol, Boolean))
        Dim anonCreationNode = DirectCast(context.Node, AnonymousObjectCreationExpressionSyntax)
        Dim semanticModel = context.SemanticModel
        Dim cancellationToken = context.CancellationToken

        ' Check each initializer in the anonymous object creation
        If anonCreationNode.Initializer IsNot Nothing Then
            For Each fieldInit As FieldInitializerSyntax In anonCreationNode.Initializer.Initializers
                Dim fieldInitExpr As ExpressionSyntax = Nothing
                If TypeOf fieldInit Is NamedFieldInitializerSyntax Then
                    fieldInitExpr = DirectCast(fieldInit, NamedFieldInitializerSyntax).Expression
                ElseIf TypeOf fieldInit Is InferredFieldInitializerSyntax Then
                    fieldInitExpr = DirectCast(fieldInit, InferredFieldInitializerSyntax).Expression
                End If

                If fieldInitExpr IsNot Nothing Then
                    Dim initializerType = semanticModel.GetTypeInfo(fieldInitExpr, cancellationToken).Type
                    If initializerType IsNot Nothing AndAlso IsRestrictedType(initializerType, restrictedTypeCache) Then
                        Dim diagnostic As Diagnostic = Diagnostic.Create(Rule, fieldInitExpr.GetLocation(), initializerType.Name)
                        context.ReportDiagnostic(diagnostic)
                    End If
                End If
            Next
        End If
    End Sub

    Private Sub AnalyzeObjectCreation(context As SyntaxNodeAnalysisContext, restrictedTypeCache As ConcurrentDictionary(Of ITypeSymbol, Boolean))
        Dim creationNode = DirectCast(context.Node, ObjectCreationExpressionSyntax)
        Dim semanticModel = context.SemanticModel
        Dim cancellationToken = context.CancellationToken

        ' Check each argument in the object creation
        If creationNode.ArgumentList IsNot Nothing Then
            For Each arg In creationNode.ArgumentList.Arguments
                Dim argNode = TryCast(arg, SimpleArgumentSyntax)
                If argNode IsNot Nothing Then
                    Dim argType = semanticModel.GetTypeInfo(argNode.Expression, cancellationToken).Type
                    If argType IsNot Nothing AndAlso IsRestrictedType(argType, restrictedTypeCache) Then
                        ' Check if this is a collection initializer context
                        Dim diagnostic As Diagnostic = Diagnostic.Create(Rule, argNode.Expression.GetLocation(), argType.Name)
                        context.ReportDiagnostic(diagnostic)
                    End If
                End If
            Next
        End If
    End Sub

    Private Sub AnalyzeCollectionInitializer(context As SyntaxNodeAnalysisContext, restrictedTypeCache As ConcurrentDictionary(Of ITypeSymbol, Boolean))
        Dim collectionInitNode = DirectCast(context.Node, CollectionInitializerSyntax)
        Dim semanticModel = context.SemanticModel
        Dim cancellationToken = context.CancellationToken

        ' Check each expression in the collection initializer
        For Each expr In collectionInitNode.Initializers
            Dim exprType = semanticModel.GetTypeInfo(expr, cancellationToken).Type
            If exprType IsNot Nothing AndAlso IsRestrictedType(exprType, restrictedTypeCache) Then
                Dim diagnostic As Diagnostic = Diagnostic.Create(Rule, expr.GetLocation(), exprType.Name)
                context.ReportDiagnostic(diagnostic)
            End If
        Next
    End Sub

    Private Sub AnalyzeArrayLiteral(context As SyntaxNodeAnalysisContext, restrictedTypeCache As ConcurrentDictionary(Of ITypeSymbol, Boolean))
        Dim arrayLiteralNode = DirectCast(context.Node, CollectionInitializerSyntax)
        Dim semanticModel = context.SemanticModel
        Dim cancellationToken = context.CancellationToken

        ' Check each expression in the array literal
        For Each expr In arrayLiteralNode.Initializers
            Dim exprType = semanticModel.GetTypeInfo(expr, cancellationToken).Type
            If exprType IsNot Nothing AndAlso IsRestrictedType(exprType, restrictedTypeCache) Then
                Dim diagnostic As Diagnostic = Diagnostic.Create(Rule, expr.GetLocation(), exprType.Name)
                context.ReportDiagnostic(diagnostic)
            End If
        Next
    End Sub

    Private Sub AnalyzeLocalDeclaration(context As SyntaxNodeAnalysisContext, restrictedTypeCache As ConcurrentDictionary(Of ITypeSymbol, Boolean))
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

                ' Check if the variable type is restricted
                If variableType IsNot Nothing AndAlso IsRestrictedType(variableType, restrictedTypeCache) Then
                    Dim diagnostic As Diagnostic = Diagnostic.Create(Rule, declarator.GetLocation(), variableType.Name)
                    context.ReportDiagnostic(diagnostic)
                End If
            End If

            ' Check for nullable type inference (e.g., Dim nullableSpan? = span)
            ' Look for the question token in the identifier
            For Each name In declarator.Names
                If Not name.Nullable.IsMissing Then
                    ' Check if the nullable token is a question mark
                    If name.Nullable.IsKind(SyntaxKind.QuestionToken) Then
                        ' Get the inferred type from the initializer
                        If declarator.Initializer IsNot Nothing Then
                            Dim initializerType = semanticModel.GetTypeInfo(declarator.Initializer.Value, cancellationToken).Type
                            If initializerType IsNot Nothing AndAlso IsRestrictedType(initializerType, restrictedTypeCache) Then
                                Dim diagnostic As Diagnostic = Diagnostic.Create(Rule, name.Nullable.GetLocation(), initializerType.Name)
                                context.ReportDiagnostic(diagnostic)
                            End If
                        End If
                    End If
                End If
            Next
        Next
    End Sub

    Private Sub AnalyzeVariableDeclarator(context As SyntaxNodeAnalysisContext, restrictedTypeCache As ConcurrentDictionary(Of ITypeSymbol, Boolean))
        Dim variableDeclaratorNode = DirectCast(context.Node, VariableDeclaratorSyntax)
        Dim semanticModel = context.SemanticModel
        Dim cancellationToken = context.CancellationToken

        ' Check struct fields and properties for non-restricted structs containing restricted types
        Dim containingSymbol = semanticModel.GetDeclaredSymbol(variableDeclaratorNode.Parent?.Parent, cancellationToken)
        If containingSymbol IsNot Nothing AndAlso containingSymbol.Kind = SymbolKind.NamedType Then
            Dim containingType = TryCast(containingSymbol, INamedTypeSymbol)
            ' Check if containing type is a struct but not restricted
            If containingType IsNot Nothing AndAlso containingType.TypeKind = TypeKind.Structure AndAlso Not IsRestrictedType(containingType, restrictedTypeCache) Then
                If variableDeclaratorNode.AsClause IsNot Nothing Then
                    Dim fieldType = semanticModel.GetTypeInfo(variableDeclaratorNode.AsClause.Type, cancellationToken).Type
                    If fieldType IsNot Nothing AndAlso IsRestrictedType(fieldType, restrictedTypeCache) Then
                        Dim diagnostic As Diagnostic = Diagnostic.Create(Rule, variableDeclaratorNode.AsClause.Type.GetLocation(), fieldType.Name)
                        context.ReportDiagnostic(diagnostic)
                    End If
                End If
            End If
        End If
    End Sub

    Private Sub AnalyzeEqualsValue(context As SyntaxNodeAnalysisContext, restrictedTypeCache As ConcurrentDictionary(Of ITypeSymbol, Boolean))
        Dim equalsValueNode = DirectCast(context.Node, EqualsValueSyntax)
        Dim semanticModel = context.SemanticModel
        Dim cancellationToken = context.CancellationToken

        ' Get the type of the value being assigned
        Dim valueType = semanticModel.GetTypeInfo(equalsValueNode.Value, cancellationToken).Type
        If valueType IsNot Nothing AndAlso IsRestrictedType(valueType, restrictedTypeCache) Then
            ' Check if this is in a collection initializer context
            Dim parent = equalsValueNode.Parent
            If parent IsNot Nothing Then
                ' Check if parent is a collection initializer or array
                If parent.IsKind(SyntaxKind.CollectionInitializer) Then
                    Dim diagnostic As Diagnostic = Diagnostic.Create(Rule, equalsValueNode.Value.GetLocation(), valueType.Name)
                    context.ReportDiagnostic(diagnostic)
                End If
            End If
        End If
    End Sub

    Private Sub AnalyzeObjectCollectionInitializer(context As SyntaxNodeAnalysisContext, restrictedTypeCache As ConcurrentDictionary(Of ITypeSymbol, Boolean))
        Dim collectionInitNode = DirectCast(context.Node, ObjectCollectionInitializerSyntax)
        Dim semanticModel = context.SemanticModel
        Dim cancellationToken = context.CancellationToken

        ' Check each expression in the object collection initializer
        If collectionInitNode.Initializer IsNot Nothing Then
            For Each expr In collectionInitNode.Initializer.Initializers
                Dim exprType = semanticModel.GetTypeInfo(expr, cancellationToken).Type
                If exprType IsNot Nothing AndAlso IsRestrictedType(exprType, restrictedTypeCache) Then
                    Dim diagnostic As Diagnostic = Diagnostic.Create(Rule, expr.GetLocation(), exprType.Name)
                    context.ReportDiagnostic(diagnostic)
                End If
            Next
        End If
    End Sub

End Class

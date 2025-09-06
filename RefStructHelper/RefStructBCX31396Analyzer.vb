Imports System.Collections.Immutable
Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.Diagnostics
Imports Microsoft.CodeAnalysis.VisualBasic
Imports Microsoft.CodeAnalysis.VisualBasic.Syntax

' Some utilities are in SymbolHelper
<DiagnosticAnalyzer(LanguageNames.VisualBasic)>
Public Class RefStructBCX31396Analyzer
    Inherits DiagnosticAnalyzer

    Public Const DiagnosticId = "BCX31396"

    ' You can change these strings in the Resources.resx file.
    Private Shared ReadOnly Title As New LocalizableResourceString(NameOf(My.Resources.TITLE_RestrictedType1), My.Resources.ResourceManager, GetType(My.Resources.Resources))
    Private Shared ReadOnly MessageFormat As New LocalizableResourceString(NameOf(My.Resources.ERR_RestrictedType1), My.Resources.ResourceManager, GetType(My.Resources.Resources))
    Private Shared ReadOnly Description As New LocalizableResourceString(NameOf(My.Resources.DESC_RestrictedType1), My.Resources.ResourceManager, GetType(My.Resources.Resources))
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
        ' Register for various syntax node actions to catch different scenarios
        context.RegisterSyntaxNodeAction(AddressOf AnalyzeNullableType, SyntaxKind.NullableType)
        context.RegisterSyntaxNodeAction(AddressOf AnalyzeArrayType, SyntaxKind.ArrayType)
        context.RegisterSyntaxNodeAction(AddressOf AnalyzeFieldDeclaration, SyntaxKind.FieldDeclaration)
        context.RegisterSyntaxNodeAction(AddressOf AnalyzePropertyDeclaration, SyntaxKind.PropertyStatement)
        context.RegisterSyntaxNodeAction(AddressOf AnalyzePropertyBlock, SyntaxKind.PropertyBlock)
        context.RegisterSyntaxNodeAction(AddressOf AnalyzeParameter, SyntaxKind.Parameter)
        context.RegisterSyntaxNodeAction(AddressOf AnalyzeAnonymousObjectCreation, SyntaxKind.AnonymousObjectCreationExpression)
        context.RegisterSyntaxNodeAction(AddressOf AnalyzeCollectionInitializer, SyntaxKind.CollectionInitializer)
        context.RegisterSyntaxNodeAction(AddressOf AnalyzeArrayLiteral, SyntaxKind.CollectionInitializer)
        context.RegisterSyntaxNodeAction(AddressOf AnalyzeLocalDeclaration, SyntaxKind.LocalDeclarationStatement)
        context.RegisterSyntaxNodeAction(AddressOf AnalyzeVariableDeclarator, SyntaxKind.VariableDeclarator)
        context.RegisterSyntaxNodeAction(AddressOf AnalyzeEqualsValue, SyntaxKind.EqualsValue)
        context.RegisterSyntaxNodeAction(AddressOf AnalyzeObjectCollectionInitializer, SyntaxKind.ObjectCollectionInitializer)
        context.RegisterSyntaxNodeAction(AddressOf AnalyzeFunctionReturn, SyntaxKind.FunctionStatement)
        context.RegisterSyntaxNodeAction(AddressOf AnalyzeOperatorReturn, SyntaxKind.OperatorStatement)
    End Sub

    Private Sub AnalyzeFunctionReturn(context As SyntaxNodeAnalysisContext)
        Dim functionNode = DirectCast(context.Node, MethodStatementSyntax)
        Dim semanticModel = context.SemanticModel
        Dim cancellationToken = context.CancellationToken

        ' Check function return type
        If functionNode.AsClause IsNot Nothing Then
            Dim returnType = semanticModel.GetTypeInfo(functionNode.AsClause.Type, cancellationToken).Type
            If returnType IsNot Nothing AndAlso IsRestrictedType(returnType) Then
                Dim diagnostic As Diagnostic = Diagnostic.Create(Rule, functionNode.AsClause.Type.GetLocation(), returnType.Name)
                context.ReportDiagnostic(diagnostic)
            End If
        End If
    End Sub

    Private Sub AnalyzeOperatorReturn(context As SyntaxNodeAnalysisContext)
        Dim operatorNode = DirectCast(context.Node, OperatorStatementSyntax)
        Dim semanticModel = context.SemanticModel
        Dim cancellationToken = context.CancellationToken

        ' Check operator return type
        If operatorNode.AsClause IsNot Nothing Then
            Dim returnType = semanticModel.GetTypeInfo(operatorNode.AsClause.Type, cancellationToken).Type
            If returnType IsNot Nothing AndAlso IsRestrictedType(returnType) Then
                Dim diagnostic As Diagnostic = Diagnostic.Create(Rule, operatorNode.AsClause.Type.GetLocation(), returnType.Name)
                context.ReportDiagnostic(diagnostic)
            End If
        End If
    End Sub


    Private Sub AnalyzeNullableType(context As SyntaxNodeAnalysisContext)
        Dim nullableTypeNode = DirectCast(context.Node, NullableTypeSyntax)
        Dim semanticModel = context.SemanticModel
        Dim cancellationToken = context.CancellationToken

        ' Get the underlying type
        Dim underlyingType = semanticModel.GetTypeInfo(nullableTypeNode.ElementType, cancellationToken).Type
        If underlyingType IsNot Nothing AndAlso IsRestrictedType(underlyingType) Then
            Dim diagnostic As Diagnostic = Diagnostic.Create(Rule, nullableTypeNode.GetLocation(), underlyingType.Name)
            context.ReportDiagnostic(diagnostic)
        End If
    End Sub

    Private Sub AnalyzeArrayType(context As SyntaxNodeAnalysisContext)
        Dim arrayTypeNode = DirectCast(context.Node, ArrayTypeSyntax)
        Dim semanticModel = context.SemanticModel
        Dim cancellationToken = context.CancellationToken

        ' Get the element type
        Dim elementType = semanticModel.GetTypeInfo(arrayTypeNode.ElementType, cancellationToken).Type
        If elementType IsNot Nothing AndAlso IsRestrictedType(elementType) Then
            Dim diagnostic As Diagnostic = Diagnostic.Create(Rule, arrayTypeNode.GetLocation(), elementType.Name)
            context.ReportDiagnostic(diagnostic)
        End If
    End Sub

    Private Sub AnalyzeFieldDeclaration(context As SyntaxNodeAnalysisContext)
        Dim fieldDeclNode = DirectCast(context.Node, FieldDeclarationSyntax)
        Dim semanticModel = context.SemanticModel
        Dim cancellationToken = context.CancellationToken

        ' Check each declarator in the field declaration
        For Each declarator In fieldDeclNode.Declarators
            If declarator.AsClause IsNot Nothing Then
                Dim fieldType = semanticModel.GetTypeInfo(declarator.AsClause.Type, cancellationToken).Type
                If fieldType IsNot Nothing AndAlso IsRestrictedType(fieldType) Then
                    ' Check if the containing type is a class (not a restricted struct)
                    Dim containingTypeSymbol = TryCast(semanticModel.GetDeclaredSymbol(fieldDeclNode.Parent, cancellationToken), INamedTypeSymbol)
                    If containingTypeSymbol IsNot Nothing Then
                        ' For classes, report diagnostic. For structs, only report if the struct is not restricted
                        If containingTypeSymbol.TypeKind = TypeKind.Class Then
                            Dim diagnostic As Diagnostic = Diagnostic.Create(Rule, declarator.AsClause.Type.GetLocation(), fieldType.Name)
                            context.ReportDiagnostic(diagnostic)
                        ElseIf containingTypeSymbol.TypeKind = TypeKind.Structure AndAlso Not IsRestrictedType(containingTypeSymbol) Then
                            Dim diagnostic As Diagnostic = Diagnostic.Create(Rule, declarator.AsClause.Type.GetLocation(), fieldType.Name)
                            context.ReportDiagnostic(diagnostic)
                        End If
                    End If
                End If
            End If
        Next
    End Sub

    Private Sub AnalyzePropertyDeclaration(context As SyntaxNodeAnalysisContext)
        Dim propertyNode = DirectCast(context.Node, PropertyStatementSyntax)
        Dim semanticModel = context.SemanticModel
        Dim cancellationToken = context.CancellationToken

        If propertyNode.AsClause IsNot Nothing Then
            Dim propertyType = semanticModel.GetTypeInfo(propertyNode.AsClause.Type, cancellationToken).Type
            If propertyType IsNot Nothing AndAlso IsRestrictedType(propertyType) Then
                ' Check if the containing type is a class (not a restricted struct)
                Dim containingTypeSymbol = TryCast(semanticModel.GetDeclaredSymbol(propertyNode.Parent, cancellationToken), INamedTypeSymbol)
                If containingTypeSymbol IsNot Nothing Then
                    ' For classes, report diagnostic. For structs, only report if the struct is not restricted
                    If containingTypeSymbol.TypeKind = TypeKind.Class Then
                        Dim diagnostic As Diagnostic = Diagnostic.Create(Rule, propertyNode.AsClause.Type.GetLocation(), propertyType.Name)
                        context.ReportDiagnostic(diagnostic)
                    ElseIf containingTypeSymbol.TypeKind = TypeKind.Structure AndAlso Not IsRestrictedType(containingTypeSymbol) Then
                        Dim diagnostic As Diagnostic = Diagnostic.Create(Rule, propertyNode.AsClause.Type.GetLocation(), propertyType.Name)
                        context.ReportDiagnostic(diagnostic)
                    End If
                End If
            End If
        End If
    End Sub

    Private Sub AnalyzePropertyBlock(context As SyntaxNodeAnalysisContext)
        Dim propertyBlockNode = DirectCast(context.Node, PropertyBlockSyntax)
        Dim semanticModel = context.SemanticModel
        Dim cancellationToken = context.CancellationToken

        ' Analyze the property statement
        If propertyBlockNode.PropertyStatement.AsClause IsNot Nothing Then
            Dim propertyType = semanticModel.GetTypeInfo(propertyBlockNode.PropertyStatement.AsClause.Type, cancellationToken).Type
            If propertyType IsNot Nothing AndAlso IsRestrictedType(propertyType) Then
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

    Private Sub AnalyzeParameter(context As SyntaxNodeAnalysisContext)
        Dim parameterNode = DirectCast(context.Node, ParameterSyntax)
        Dim semanticModel = context.SemanticModel
        Dim cancellationToken = context.CancellationToken

        If parameterNode.AsClause IsNot Nothing Then
            Dim parameterType = semanticModel.GetTypeInfo(parameterNode.AsClause.Type, cancellationToken).Type
            If parameterType IsNot Nothing AndAlso IsRestrictedType(parameterType) Then
                ' Check if it's a ByRef parameter
                If parameterNode.Modifiers.Any(Function(m) m.IsKind(SyntaxKind.ByRefKeyword)) Then
                    Dim diagnostic As Diagnostic = Diagnostic.Create(Rule, parameterNode.AsClause.Type.GetLocation(), parameterType.Name)
                    context.ReportDiagnostic(diagnostic)
                End If
            End If
        End If
    End Sub

    Private Sub AnalyzeAnonymousObjectCreation(context As SyntaxNodeAnalysisContext)
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
                    If initializerType IsNot Nothing AndAlso IsRestrictedType(initializerType) Then
                        Dim diagnostic As Diagnostic = Diagnostic.Create(Rule, fieldInitExpr.GetLocation(), initializerType.Name)
                        context.ReportDiagnostic(diagnostic)
                    End If
                End If
            Next
        End If
    End Sub

    Private Sub AnalyzeCollectionInitializer(context As SyntaxNodeAnalysisContext)
        Dim collectionInitNode = DirectCast(context.Node, CollectionInitializerSyntax)
        Dim semanticModel = context.SemanticModel
        Dim cancellationToken = context.CancellationToken

        ' Check each expression in the collection initializer
        For Each expr In collectionInitNode.Initializers
            Dim exprType = semanticModel.GetTypeInfo(expr, cancellationToken).Type
            If exprType IsNot Nothing AndAlso IsRestrictedType(exprType) Then
                Dim diagnostic As Diagnostic = Diagnostic.Create(Rule, expr.GetLocation(), exprType.Name)
                context.ReportDiagnostic(diagnostic)
            End If
        Next
    End Sub

    Private Sub AnalyzeArrayLiteral(context As SyntaxNodeAnalysisContext)
        Dim arrayLiteralNode = DirectCast(context.Node, CollectionInitializerSyntax)
        Dim semanticModel = context.SemanticModel
        Dim cancellationToken = context.CancellationToken

        ' Check each expression in the array literal
        For Each expr In arrayLiteralNode.Initializers
            Dim exprType = semanticModel.GetTypeInfo(expr, cancellationToken).Type
            If exprType IsNot Nothing AndAlso IsRestrictedType(exprType) Then
                Dim diagnostic As Diagnostic = Diagnostic.Create(Rule, expr.GetLocation(), exprType.Name)
                context.ReportDiagnostic(diagnostic)
            End If
        Next
    End Sub

    Private Sub AnalyzeLocalDeclaration(context As SyntaxNodeAnalysisContext)
        Dim localDeclNode = DirectCast(context.Node, LocalDeclarationStatementSyntax)
        Dim semanticModel = context.SemanticModel
        Dim cancellationToken = context.CancellationToken

        ' Check if this is a Static declaration - Static declarations are actually fields with thread synchronization
        If localDeclNode.Modifiers.Any(Function(m) m.IsKind(SyntaxKind.StaticKeyword)) Then
            ' Handle Static declarations as field declarations
            For Each declarator In localDeclNode.Declarators
                If declarator.AsClause IsNot Nothing Then
                    Dim fieldType = semanticModel.GetTypeInfo(declarator.AsClause.Type, cancellationToken).Type
                    If fieldType IsNot Nothing AndAlso IsRestrictedType(fieldType) Then
                        ' Static declarations in methods are compiled as fields in the class
                        ' Report diagnostic for restricted types in Static declarations
                        Dim diagnostic As Diagnostic = Diagnostic.Create(Rule, declarator.AsClause.Type.GetLocation(), fieldType.Name)
                        context.ReportDiagnostic(diagnostic)
                    End If
                End If
            Next
            Return ' Exit early for Static declarations
        End If

        ' 注意：Dim nullableSpan? As Span(Of Integer) 和 Dim nullableSpan As Span(Of Integer)? 是两码事！
        ' Dim nullableSpan? As Span(Of Integer) 的 As 语句的类型是 Span(Of Integer)
        ' Dim nullableSpan As Span(Of Integer)? 的 As 语句的类型是 Span(Of Integer)?
        ' Dim nullableSpan? As Span(Of Integer) 的 name.Nullable <> Nothing
        ' Dim nullableSpan As Span(Of Integer)? 的 name.Nullable = Nothing
        ' Dim nullableSpan() As Span(Of Integer) 的 name.ArrayRankSpecifiers.Count > 0
        ' Dim nullableSpan As Span(Of Integer)() 的 name.ArrayRankSpecifiers.Count = 0

        ' Check each declarator in the declaration
        For Each declarator In localDeclNode.Declarators
            ' Initializer is Nullable(Of RestrictedType)
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

                ' Only check for restricted types if the variable type is Nullable(Of T)
                If variableType IsNot Nothing AndAlso IsNullableOfType(variableType) Then
                    ' Get the underlying type T from Nullable(Of T)
                    Dim underlyingType = GetNullableUnderlyingType(variableType)
                    If underlyingType IsNot Nothing AndAlso IsRestrictedType(underlyingType) Then
                        Dim diagnostic As Diagnostic = Diagnostic.Create(Rule, declarator.GetLocation(), underlyingType.Name)
                        context.ReportDiagnostic(diagnostic)
                    End If
                End If
            End If

            ' Check for nullable type inference
            ' Look for the question token in the identifier
            For Each name In declarator.Names
                ' Check if the nullable token is a question mark
                If name.Nullable <> Nothing Then
                    ' For Dim nullableSpan? As Span(Of Integer) [= Something]
                    ' 当变量名有问号修饰时，需要检查 AsClause 的类型是否为受限类型
                    If declarator.AsClause IsNot Nothing Then
                        Dim asType = semanticModel.GetTypeInfo(declarator.AsClause.Type, cancellationToken).Type
                        If asType IsNot Nothing AndAlso IsRestrictedType(asType) Then
                            Dim diagnostic As Diagnostic = Diagnostic.Create(Rule, declarator.AsClause.Type.GetLocation(), asType.Name)
                            context.ReportDiagnostic(diagnostic)
                        End If
                    ElseIf declarator.Initializer IsNot Nothing Then
                        ' Check for Dim nullableSpan? = span (type inference from initializer)
                        Dim initializerType = semanticModel.GetTypeInfo(declarator.Initializer.Value, cancellationToken).Type
                        If initializerType IsNot Nothing AndAlso IsRestrictedType(initializerType) Then
                            Dim diagnostic As Diagnostic = Diagnostic.Create(Rule, name.GetLocation(), initializerType.Name)
                            context.ReportDiagnostic(diagnostic)
                        End If
                    End If
                End If
            Next

            ' Check for Dim nullableSpan() As Span(Of Integer)
            ' 检查数组声明：每个 name 的 ArrayRankSpecifiers.Count > 0
            For Each name In declarator.Names
                If name.ArrayRankSpecifiers.Count > 0 Then
                    ' 这是数组声明，检查 AsClause 的类型是否为受限类型
                    If declarator.AsClause IsNot Nothing Then
                        Dim asType = semanticModel.GetTypeInfo(declarator.AsClause.Type, cancellationToken).Type
                        If asType IsNot Nothing AndAlso IsRestrictedType(asType) Then
                            Dim diagnostic As Diagnostic = Diagnostic.Create(Rule, declarator.AsClause.Type.GetLocation(), asType.Name)
                            context.ReportDiagnostic(diagnostic)
                        End If
                    End If
                End If
            Next
        Next
    End Sub

    Private Sub AnalyzeVariableDeclarator(context As SyntaxNodeAnalysisContext)
        Dim variableDeclaratorNode = DirectCast(context.Node, VariableDeclaratorSyntax)
        Dim semanticModel = context.SemanticModel
        Dim cancellationToken = context.CancellationToken

        ' Check struct fields and properties for non-restricted structs containing restricted types
        Dim containingSymbol = semanticModel.GetDeclaredSymbol(variableDeclaratorNode.Parent?.Parent, cancellationToken)
        If containingSymbol IsNot Nothing AndAlso containingSymbol.Kind = SymbolKind.NamedType Then
            Dim containingType = TryCast(containingSymbol, INamedTypeSymbol)
            ' Check if containing type is a struct but not restricted
            If containingType IsNot Nothing AndAlso containingType.TypeKind = TypeKind.Structure AndAlso Not IsRestrictedType(containingType) Then
                If variableDeclaratorNode.AsClause IsNot Nothing Then
                    Dim fieldType = semanticModel.GetTypeInfo(variableDeclaratorNode.AsClause.Type, cancellationToken).Type
                    If fieldType IsNot Nothing AndAlso IsRestrictedType(fieldType) Then
                        Dim diagnostic As Diagnostic = Diagnostic.Create(Rule, variableDeclaratorNode.AsClause.Type.GetLocation(), fieldType.Name)
                        context.ReportDiagnostic(diagnostic)
                    End If
                End If
            End If
        End If
    End Sub

    Private Sub AnalyzeEqualsValue(context As SyntaxNodeAnalysisContext)
        Dim equalsValueNode = DirectCast(context.Node, EqualsValueSyntax)
        Dim semanticModel = context.SemanticModel
        Dim cancellationToken = context.CancellationToken

        ' Get the type of the value being assigned
        Dim valueType = semanticModel.GetTypeInfo(equalsValueNode.Value, cancellationToken).Type
        If valueType IsNot Nothing AndAlso IsRestrictedType(valueType) Then
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

    Private Sub AnalyzeObjectCollectionInitializer(context As SyntaxNodeAnalysisContext)
        Dim collectionInitNode = DirectCast(context.Node, ObjectCollectionInitializerSyntax)
        Dim semanticModel = context.SemanticModel
        Dim cancellationToken = context.CancellationToken

        ' Check each expression in the object collection initializer
        If collectionInitNode.Initializer IsNot Nothing Then
            For Each expr In collectionInitNode.Initializer.Initializers
                Dim exprType = semanticModel.GetTypeInfo(expr, cancellationToken).Type
                If exprType IsNot Nothing AndAlso IsRestrictedType(exprType) Then
                    Dim diagnostic As Diagnostic = Diagnostic.Create(Rule, expr.GetLocation(), exprType.Name)
                    context.ReportDiagnostic(diagnostic)
                End If
            Next
        End If
    End Sub

    ' 添加辅助方法来检查类型是否为 Nullable(Of T)
    Private Function IsNullableOfType(typeSymbol As ITypeSymbol) As Boolean
        If typeSymbol Is Nothing Then Return False

        ' 检查是否为命名类型且是泛型类型
        Dim namedType = TryCast(typeSymbol, INamedTypeSymbol)
        If namedType Is Nothing Then Return False

        ' 检查是否为 Nullable(Of T) 类型
        If namedType.IsGenericType AndAlso namedType.ConstructedFrom?.SpecialType = SpecialType.System_Nullable_T Then
            Return True
        End If

        Return False
    End Function

    ' 添加辅助方法来获取 Nullable(Of T) 中的 T 类型
    Private Function GetNullableUnderlyingType(nullableType As ITypeSymbol) As ITypeSymbol
        Dim namedType = TryCast(nullableType, INamedTypeSymbol)
        If namedType IsNot Nothing AndAlso namedType.IsGenericType Then
            Return namedType.TypeArguments.FirstOrDefault()
        End If
        Return Nothing
    End Function

End Class

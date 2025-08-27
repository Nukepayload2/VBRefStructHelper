Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.Diagnostics
Imports Microsoft.CodeAnalysis.VisualBasic

''' <summary>
''' 提供 OptionRestrict 选项检查的统一逻辑
''' </summary>
Public Module OptionRestrictChecker

    ''' <summary>
    ''' 检查是否启用了 OptionRestrict 选项
    ''' </summary>
    ''' <param name="context">编译启动分析上下文</param>
    ''' <returns>如果启用了 OptionRestrict 返回 True，否则返回 False</returns>
    Public Function IsOptionRestrictEnabled(context As CompilationStartAnalysisContext) As Boolean
        Return IsOptionRestrictEnabledInternal(context.Options.AnalyzerConfigOptionsProvider, context.Compilation)
    End Function

    ''' <summary>
    ''' 检查是否启用了 OptionRestrict 选项（重载版本用于不同的上下文）
    ''' </summary>
    ''' <param name="compilation">编译对象</param>
    ''' <param name="optionsProvider">分析器配置选项提供者（可选）</param>
    ''' <returns>如果启用了 OptionRestrict 返回 True，否则返回 False</returns>
    Public Function IsOptionRestrictEnabled(compilation As Compilation, Optional optionsProvider As AnalyzerConfigOptionsProvider = Nothing) As Boolean
        Return IsOptionRestrictEnabledInternal(optionsProvider, compilation)
    End Function

    ''' <summary>
    ''' 内部实现：检查 OptionRestrict 是否启用
    ''' </summary>
    Private Function IsOptionRestrictEnabledInternal(optionsProvider As AnalyzerConfigOptionsProvider, compilation As Compilation) As Boolean
        ' 阶段1：检查 MSBuild 属性 <OptionRestrict>On</OptionRestrict>
        If optionsProvider IsNot Nothing Then
            Dim globalOptions = optionsProvider.GlobalOptions

            ' 检查属性名
            Dim optionRestrictValue As String = Nothing
            If globalOptions.TryGetValue("build_property.OptionRestrict", optionRestrictValue) Then
                Return IsValueEnabled(optionRestrictValue)
            End If
        End If

        ' 默认关闭
        Return False
    End Function

    ''' <summary>
    ''' 检查特定类型是否选择加入了受限类型规则检查
    ''' 注意：ExtendRestrictedTypeRulesAttribute 需要用户在自己的项目中定义，
    ''' 因为分析器项目不能包含运行时类型
    ''' </summary>
    ''' <param name="typeSymbol">要检查的类型</param>
    ''' <param name="compilation">编译对象</param>
    ''' <returns>如果类型选择加入了检查返回 True，否则返回 False</returns>
    Public Function IsTypeOptInForRestrictions(typeSymbol As ITypeSymbol, compilation As Compilation) As Boolean
        If typeSymbol Is Nothing Then Return False

        ' 检查 ExtendRestrictedTypeRulesAttribute
        ' 用户需要在自己的项目中定义此属性：
        '
        ' Namespace Global.Nukepayload2.CompilerServices
        '   <AttributeUsage(AttributeTargets.Class Or AttributeTargets.Struct Or AttributeTargets.Interface Or AttributeTargets.Delegate, AllowMultiple:=False, Inherited:=False)>
        '   Friend Class ExtendRestrictedTypeRulesAttribute
        '   End Class
        ' End Namespace

        Dim attributeTypeName = "Nukepayload2.CompilerServices.ExtendRestrictedTypeRulesAttribute"
        Dim attributeType = compilation.GetTypeByMetadataName(attributeTypeName)
        
        If attributeType IsNot Nothing Then
            For Each attr In typeSymbol.GetAttributes()
                If SymbolEqualityComparer.Default.Equals(attr.AttributeClass, attributeType) Then
                    Return True
                End If
            Next
        End If
        
        Return False
    End Function

    ''' <summary>
    ''' 综合检查：OptionRestrict 全局启用 或 特定类型选择加入
    ''' </summary>
    ''' <param name="typeSymbol">要检查的类型</param>
    ''' <param name="context">编译启动分析上下文</param>
    ''' <returns>如果应该对此类型启用检查返回 True，否则返回 False</returns>
    Public Function ShouldAnalyzeType(typeSymbol As ITypeSymbol, context As CompilationStartAnalysisContext) As Boolean
        ' 全局启用
        If IsOptionRestrictEnabled(context) Then
            Return True
        End If
        
        ' 或者类型选择加入
        Return IsTypeOptInForRestrictions(typeSymbol, context.Compilation)
    End Function

    ''' <summary>
    ''' 辅助方法：判断配置值是否表示"启用"
    ''' </summary>
    Private Function IsValueEnabled(value As String) As Boolean
        If String.IsNullOrWhiteSpace(value) Then Return False
        
        Return String.Equals(value, "On", StringComparison.OrdinalIgnoreCase) OrElse
               String.Equals(value, "True", StringComparison.OrdinalIgnoreCase) OrElse
               String.Equals(value, "1", StringComparison.OrdinalIgnoreCase) OrElse
               String.Equals(value, "Yes", StringComparison.OrdinalIgnoreCase)
    End Function

End Module
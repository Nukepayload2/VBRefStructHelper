Imports System.Collections.Immutable
Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.Diagnostics
Imports Microsoft.CodeAnalysis.Text
Imports Microsoft.CodeAnalysis.VisualBasic

<Obsolete("Suppress default ref struct obsolete errors")>
Module TestCompilation

    ' 工具方法：编译代码并返回语法树文本和诊断结果

    Function GetSyntaxTreeTextAndDiagnostics(source As FormattableString) As (syntaxTreeText As String, diagnostics As ImmutableArray(Of Diagnostic))
        If source.ArgumentCount <> 1 Then Throw New ArgumentException("必须只有一个格式内插")
        ' 创建解析选项
        Dim parseOptions = New VisualBasicParseOptions()

        ' 创建语法树
        Dim sourceText = source.ToString
        Dim syntaxTreeSource = CStr(source.GetArgument(0))
        Dim syntaxTree = VisualBasicSyntaxTree.ParseText(sourceText, parseOptions)

        ' 创建引用（包括系统引用和我们的分析器）
        Dim references As MetadataReference() = {
            MetadataReference.CreateFromFile(GetType(Object).Assembly.Location),
            MetadataReference.CreateFromFile(GetType(Span(Of )).Assembly.Location),
            MetadataReference.CreateFromFile(GetType(ValueType).Assembly.Location),
            MetadataReference.CreateFromFile(GetType(System.Runtime.InteropServices.MemoryMarshal).Assembly.Location)
        }.DistinctBy(Function(it) it.FilePath).ToArray

        ' 创建编译选项，启用 OptionRestrict
        Dim compilationOptions = New VisualBasicCompilationOptions(OutputKind.DynamicallyLinkedLibrary)

        ' 创建编译
        Dim compilation = VisualBasicCompilation.Create(
            "TestAssembly",
            {syntaxTree},
            references,
            compilationOptions
        )

        ' 运行我们的分析器
        Dim analyzer1 As New RefStructBCX31394Analyzer
        Dim analyzer2 As New RefStructBCX31396Analyzer
        Dim analyzer3 As New RefStructBCX32061Analyzer
        Dim analyzer4 As New RefStructBCX36598Analyzer
        Dim analyzer5 As New RefStructBCX36640Analyzer
        Dim analyzer6 As New RefStructBCX37052Analyzer
        Dim analyzer7 As New RefStructBCX31393Analyzer
        Dim analyzers As ImmutableArray(Of DiagnosticAnalyzer) = ImmutableArray.Create(Of DiagnosticAnalyzer)({analyzer1, analyzer2, analyzer3, analyzer4, analyzer5, analyzer6, analyzer7})

        Dim analyzerOptions = New AnalyzerOptions(ImmutableArray(Of AdditionalText).Empty)

        Dim compilationWithAnalyzers = compilation.WithAnalyzers(analyzers, analyzerOptions)
        Dim diagnostics = compilationWithAnalyzers.GetAnalyzerDiagnosticsAsync().Result

        ' 获取语法树文本用于调试
        Dim syntaxTreeText As String
        If syntaxTreeSource = Nothing Then
            syntaxTreeText = SyntaxTreeVisualizer.ToVisualString(syntaxTree.GetRoot())
        Else
            ' 使用 IndexOf 找到位置（取第一个匹配）
            Dim position As Integer = source.Format.IndexOf("{0}")
            If position >= 0 Then
                ' 创建文本跨度
                Dim span As New TextSpan(position, syntaxTreeSource.Length)
                ' 在原始语法树中找到对应的节点
                Dim node As SyntaxNode = syntaxTree.GetRoot().FindNode(span)
                ' 可视化显示找到的节点
                syntaxTreeText = SyntaxTreeVisualizer.ToVisualString(node)
            Else
                ' 如果找不到，回退到原来的重新解析方式
                syntaxTree = VisualBasicSyntaxTree.ParseText(syntaxTreeSource, parseOptions)
                syntaxTreeText = SyntaxTreeVisualizer.ToVisualString(syntaxTree.GetRoot())
            End If
        End If

        Return (syntaxTreeText, diagnostics)
    End Function

    ' 工具方法：检查是否包含指定ID的诊断
    Function ContainsDiagnostic(diagnostics As ImmutableArray(Of Diagnostic), diagnosticId As String) As Boolean
        Return diagnostics.Any(Function(d) d.Id = diagnosticId)
    End Function

End Module

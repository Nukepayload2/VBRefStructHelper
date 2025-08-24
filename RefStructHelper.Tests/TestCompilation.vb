Imports System.Collections.Immutable
Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.Diagnostics
Imports Microsoft.CodeAnalysis.VisualBasic

<Obsolete("Suppress default ref struct obsolete errors")>
Module TestCompilation

    ' 工具方法：编译代码并返回语法树文本和诊断结果

    Function GetSyntaxTreeTextAndDiagnostics(source As String) As (syntaxTreeText As String, diagnostics As ImmutableArray(Of Diagnostic))
        ' 创建解析选项
        Dim parseOptions = New VisualBasicParseOptions()

        ' 创建语法树
        Dim syntaxTree = VisualBasicSyntaxTree.ParseText(source, parseOptions)

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
        Dim analyzer As New RefStructConvertToBoxedTypeAnalyzer()
        Dim analyzers As ImmutableArray(Of DiagnosticAnalyzer) = ImmutableArray.Create(Of DiagnosticAnalyzer)({analyzer})
        Dim compilationWithAnalyzers = compilation.WithAnalyzers(analyzers)
        Dim diagnostics = compilationWithAnalyzers.GetAnalyzerDiagnosticsAsync().Result

        ' 获取语法树文本用于调试
        Dim syntaxTreeText = SyntaxTreeVisualizer.ToVisualString(syntaxTree.GetRoot())

        Return (syntaxTreeText, diagnostics)
    End Function

    ' 工具方法：检查是否包含指定ID的诊断
    Function ContainsDiagnostic(diagnostics As ImmutableArray(Of Diagnostic), diagnosticId As String) As Boolean
        Return diagnostics.Any(Function(d) d.Id = diagnosticId)
    End Function

End Module

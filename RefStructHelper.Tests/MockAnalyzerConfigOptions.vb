Imports System.Collections.Generic
Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.Diagnostics

''' <summary>
''' 模拟的 AnalyzerConfigOptions 实现，用于测试 OptionRestrict 功能
''' </summary>
Public Class MockAnalyzerConfigOptions
    Inherits AnalyzerConfigOptions

    Private ReadOnly _options As Dictionary(Of String, String)

    Public Sub New(options As Dictionary(Of String, String))
        _options = options
    End Sub

    Public Overrides Function TryGetValue(key As String, ByRef value As String) As Boolean
        Return _options.TryGetValue(key, value)
    End Function

End Class

''' <summary>
''' 模拟的 AnalyzerConfigOptionsProvider 实现，用于测试 OptionRestrict 功能
''' </summary>
Public Class MockAnalyzerConfigOptionsProvider
    Inherits AnalyzerConfigOptionsProvider

    Private ReadOnly _globalOptions As AnalyzerConfigOptions

    Public Sub New(globalOptions As AnalyzerConfigOptions)
        _globalOptions = globalOptions
    End Sub

    Public Overrides ReadOnly Property GlobalOptions As AnalyzerConfigOptions
        Get
            Return _globalOptions
        End Get
    End Property

    Public Overrides Function GetOptions(tree As SyntaxTree) As AnalyzerConfigOptions
        Return _globalOptions
    End Function

    Public Overrides Function GetOptions(textFile As AdditionalText) As AnalyzerConfigOptions
        Return _globalOptions
    End Function

End Class
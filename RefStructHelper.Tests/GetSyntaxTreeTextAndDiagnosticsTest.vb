Imports Microsoft.VisualStudio.TestTools.UnitTesting
Imports System.Collections.Immutable
Imports Microsoft.CodeAnalysis

<TestClass>
Public Class GetSyntaxTreeTextAndDiagnosticsTest

    <TestMethod>
    Public Sub TestGetSyntaxTreeTextAndDiagnosticsWithSubstring()
        ' Arrange
        Dim snippetContent As String = "Dim x As Integer = 42"

        Dim source As FormattableString = $"
Class TestClass
    Sub TestMethod()
        {snippetContent}
        Dim y As String = ""Hello""
    End Sub
End Class
"
        ' Act
        Dim result = GetSyntaxTreeTextAndDiagnostics(source)

        ' Assert
        ' 验证语法树文本包含我们期望的节点
        Assert.IsTrue(result.syntaxTreeText.Contains("LocalDeclarationStatement"))
        Assert.IsTrue(result.syntaxTreeText.Contains("x"))
        Assert.IsTrue(result.syntaxTreeText.Contains("Integer"))

        ' 验证诊断结果是空的（因为这是有效的代码）
        Assert.AreEqual(0, result.diagnostics.Length)
    End Sub

    <TestMethod>
    Public Sub TestGetSyntaxTreeTextAndDiagnosticsWithoutSubstring()
        ' Arrange
        Dim source As FormattableString = $"
Class TestClass
    Sub TestMethod()
        Dim x As Integer = 42
    End Sub
End Class
"

        ' Act
        Dim result = GetSyntaxTreeTextAndDiagnostics(source)

        ' Assert
        ' 验证返回完整的语法树
        Assert.IsTrue(result.syntaxTreeText.Contains("CompilationUnit"))
        Assert.IsTrue(result.syntaxTreeText.Contains("ClassBlock"))
        Assert.IsTrue(result.syntaxTreeText.Contains("MethodBlock"))

        ' 验证诊断结果是空的（因为这是有效的代码）
        Assert.AreEqual(0, result.diagnostics.Length)
    End Sub

End Class

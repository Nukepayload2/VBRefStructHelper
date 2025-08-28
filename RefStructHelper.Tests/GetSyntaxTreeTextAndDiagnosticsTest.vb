Imports Microsoft.VisualStudio.TestTools.UnitTesting
Imports System.Collections.Immutable
Imports Microsoft.CodeAnalysis

<TestClass>
Public Class GetSyntaxTreeTextAndDiagnosticsTest

    <TestMethod>
    Public Sub TestGetSyntaxTreeTextAndDiagnosticsWithSubstring()
        ' Arrange
        Dim source As String = "
Class TestClass
    Sub TestMethod()
        Dim x As Integer = 42
        Dim y As String = ""Hello""
    End Sub
End Class
"
        Dim snippetContent As String = "Dim x As Integer = 42"

        ' Act
        Dim result = GetSyntaxTreeTextAndDiagnostics(source, snippetContent)

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
        Dim source As String = "
Class TestClass
    Sub TestMethod()
        Dim x As Integer = 42
    End Sub
End Class
"

        ' Act
        Dim result = GetSyntaxTreeTextAndDiagnostics(source, Nothing)

        ' Assert
        ' 验证返回完整的语法树
        Assert.IsTrue(result.syntaxTreeText.Contains("CompilationUnit"))
        Assert.IsTrue(result.syntaxTreeText.Contains("ClassBlock"))
        Assert.IsTrue(result.syntaxTreeText.Contains("MethodBlock"))
        
        ' 验证诊断结果是空的（因为这是有效的代码）
        Assert.AreEqual(0, result.diagnostics.Length)
    End Sub

    <TestMethod>
    Public Sub TestGetSyntaxTreeTextAndDiagnosticsWithNotFoundSubstring()
        ' Arrange
        Dim source As String = "
Class TestClass
    Sub TestMethod()
        Dim x As Integer = 42
    End Sub
End Class
"
        Dim snippetContent As String = "NonExistentCode"

        ' Act
        Dim result = GetSyntaxTreeTextAndDiagnostics(source, snippetContent)

        ' Assert
        ' 当找不到子字符串时，应该回退到重新解析
        Assert.IsTrue(result.syntaxTreeText.Contains("CompilationUnit"))
        
        ' 验证诊断结果是空的（因为这是有效的代码）
        Assert.AreEqual(0, result.diagnostics.Length)
    End Sub

End Class

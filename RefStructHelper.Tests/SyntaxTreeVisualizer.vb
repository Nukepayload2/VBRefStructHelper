Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.VisualBasic
Imports Microsoft.CodeAnalysis.VisualBasic.Syntax
Imports System.Text

''' <summary>
''' 语法树可视化工具类
''' </summary>
Public Class SyntaxTreeVisualizer
    ''' <summary>
    ''' 将语法树转换为可视化字符串
    ''' </summary>
    ''' <param name="root">语法树根节点</param>
    ''' <returns>可视化字符串</returns>
    Public Shared Function ToVisualString(root As SyntaxNode) As String
        Dim sb As New StringBuilder()
        BuildSyntaxString(root, sb, "", True)
        Return sb.ToString()
    End Function

    ''' <summary>
    ''' 递归构建语法树字符串表示
    ''' </summary>
    ''' <param name="node">当前要处理的节点</param>
    ''' <param name="sb">StringBuilder 用于构建字符串</param>
    ''' <param name="indent">当前层级的缩进字符串</param>
    ''' <param name="isLastChild">指示当前节点是否为其父节点的最后一个子节点</param>
    Private Shared Sub BuildSyntaxString(node As SyntaxNode, sb As StringBuilder, indent As String, isLastChild As Boolean)
        If node Is Nothing Then Return

        ' --- 构造当前节点的显示文本 ---
        Dim nodeName As String = node.GetType().Name ' 例如: CompilationUnitSyntax
        ' 移除 "Syntax" 后缀以简化显示 (可选)
        If nodeName.EndsWith("Syntax") Then
            nodeName = nodeName.Substring(0, nodeName.Length - "Syntax".Length)
        End If

        Dim nodeValue As String = ""
        ' 对于叶节点 (Token)，显示其文本值
        Dim identifier As IdentifierNameSyntax = TryCast(node, IdentifierNameSyntax)
        If identifier IsNot Nothing Then
            nodeValue = identifier.Identifier.Text
        Else
            Dim literal As LiteralExpressionSyntax = TryCast(node, LiteralExpressionSyntax)
            If literal IsNot Nothing Then
                ' 特殊处理字面量表达式，显示其值
                nodeValue = literal.Token.Text
            Else
                If node.ChildNodesAndTokens().Count = 0 AndAlso TypeOf node Is ExpressionSyntax Then
                    ' 其他没有子节点的表达式，尝试获取其文本
                    nodeValue = node.ToString().Trim()
                End If
            End If
        End If
        ' 可以根据需要添加更多特定类型的值提取

        ' --- 构建当前节点字符串 ---
        Dim marker As String = If(isLastChild, "└── ", "├── ")
        Dim valuePart As String = If(String.IsNullOrEmpty(nodeValue), "", $" [{nodeValue}]")
        sb.AppendLine($"{indent}{marker}{nodeName}{valuePart}")

        ' --- 准备递归构建子节点字符串 ---
        Dim children = node.ChildNodesAndTokens()
        If children.Count > 0 Then
            Dim childIndent As String = indent & If(isLastChild, "    ", "│   ")

            For i As Integer = 0 To children.Count - 1
                Dim child = children(i)
                Dim isLast As Boolean = (i = children.Count - 1)

                If child.IsNode Then
                    BuildSyntaxString(child.AsNode(), sb, childIndent, isLast)
                ElseIf child.IsToken Then
                    ' 也可以选择构建 Token 字符串
                    BuildTokenString(child.AsToken(), sb, childIndent, isLast)
                End If
            Next
        End If
    End Sub

    ''' <summary>
    ''' 构建 Token 字符串表示
    ''' </summary>
    Private Shared Sub BuildTokenString(token As SyntaxToken, sb As StringBuilder, indent As String, isLastChild As Boolean)
        ' 过滤掉琐碎的 Token (Trivia)，如空格、换行符，除非你特别想显示它们
        If token.IsKind(SyntaxKind.EndOfFileToken) Then ' 通常不显示 EOF
            Return
        End If

        Dim marker As String = If(isLastChild, "└── ", "├── ")
        Dim kindName As String = token.Kind().ToString() ' 例如: IdentifierToken
        Dim valuePart As String = If(String.IsNullOrEmpty(token.Text), "", $" [{token.Text}]")

        ' 可以选择性地过滤掉某些类型的 Token
        ' If kindName.EndsWith("Token") AndAlso Not kindName.Equals("IdentifierToken") AndAlso Not kindName.Equals("StringLiteralToken") Then ' ...
        '     Return ' 跳过大部分非关键 Token
        ' End If

        sb.AppendLine($"{indent}{marker}(Token) {kindName}{valuePart}")
    End Sub
End Class

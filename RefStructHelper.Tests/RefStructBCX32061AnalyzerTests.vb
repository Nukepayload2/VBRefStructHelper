Imports Microsoft.VisualStudio.TestTools.UnitTesting

<TestClass>
Public Class RefStructBCX32061AnalyzerTests

    Private Shared Sub AssertThatShouldHaveError(source As FormattableString)
        With GetSyntaxTreeTextAndDiagnostics(source)
            Assert.IsTrue(ContainsDiagnostic(.diagnostics, "BCX32061"), $"应该检测到 BCX32061 诊断。语法树内容:  {vbCrLf}{ .syntaxTreeText}")
        End With
    End Sub

    Private Shared Sub AssertThatShouldNotHaveError(source As FormattableString)
        With GetSyntaxTreeTextAndDiagnostics(source)
            Assert.IsFalse(ContainsDiagnostic(.diagnostics, "BCX32061"), $"不应该检测到 BCX32061 诊断。语法树内容:  {vbCrLf}{ .syntaxTreeText}")
        End With
    End Sub

    Private Shared Sub AssertThatDiagTriggeredInSub(snippetContent As String)
        Dim source As FormattableString = $"
Imports System
Imports System.Runtime.InteropServices

<Obsolete(""Suppress default ref struct obsolete errors"")>
Class TestClass
    Sub TestMethod()
        Dim arr As Integer() = {{1, 2, 3, 4, 5}}
        Dim span As Span(Of Integer) = arr.AsSpan()
        {snippetContent}
    End Sub
End Class
"
        AssertThatShouldHaveError(source)
    End Sub

    Private Shared Sub AssertThatDiagTriggeredInClass(snippetContent As String)
        Dim source As FormattableString = $"
Imports System
Imports System.Runtime.InteropServices

<Obsolete(""Suppress default ref struct obsolete errors"")>
Class TestClass
    {snippetContent}
End Class
"
        AssertThatShouldHaveError(source)
    End Sub

    ' ====================
    ' 泛型类型参数测试
    ' ====================

    <TestMethod>
    Public Sub TestRestrictedTypeAsGenericParameter()
        Dim snippetContent = "Dim list As New List(Of Span(Of Integer))"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    <TestMethod>
    Public Sub TestRestrictedTypeAsGenericParameter2()
        Dim snippetContent = "Dim dict As New Dictionary(Of String, Span(Of Integer))"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    <TestMethod>
    Public Sub TestRestrictedTypeAsGenericParameter3()
        Dim snippetContent = "Dim list As List(Of Span(Of Integer))"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    <TestMethod>
    Public Sub TestRestrictedTypeAsGenericParameter4()
        Dim snippetContent = "Dim dict As Dictionary(Of String, Span(Of Integer))"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    <TestMethod>
    Public Sub TestRestrictedTypeAsGenericParameter5()
        Dim snippetContent = "Dim list = New List(Of Span(Of Integer))"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    <TestMethod>
    Public Sub TestRestrictedTypeAsGenericParameter6()
        Dim snippetContent = "Dim dict = New Dictionary(Of String, Span(Of Integer))"
        AssertThatDiagTriggeredInSub(snippetContent)
    End Sub

    <TestMethod>
    Public Sub TestRestrictedTypeAsGenericParameterInClass()
        Dim snippetContent = "Private field As List(Of Span(Of Integer))"
        AssertThatDiagTriggeredInClass(snippetContent)
    End Sub

    <TestMethod>
    Public Sub TestRestrictedTypeAsGenericParameterInProperty()
        Dim snippetContent = "Public Property MyProperty As List(Of Span(Of Integer))"
        AssertThatDiagTriggeredInClass(snippetContent)
    End Sub

    <TestMethod>
    Public Sub TestRestrictedTypeAsGenericParameterInMethod()
        Dim snippetContent = "Sub TestMethod(param As List(Of Span(Of Integer)))
End Sub"
        AssertThatDiagTriggeredInClass(snippetContent)
    End Sub

    <TestMethod>
    Public Sub TestRestrictedTypeAsGenericParameterInFunction()
        Dim snippetContent = "Function TestFunction() As List(Of Span(Of Integer))
End Function"
        AssertThatDiagTriggeredInClass(snippetContent)
    End Sub

    <TestMethod>
    Public Sub TestRestrictedTypeAsGenericParameterInDelegate()
        Dim snippetContent = "Delegate Sub TestDelegate(param As List(Of Span(Of Integer)))"
        AssertThatDiagTriggeredInClass(snippetContent)
    End Sub

    ' ====================
    ' 泛型约束测试
    ' ====================

    <TestMethod>
    Public Sub TestRestrictedTypeAsTypeConstraint()
        Dim source As FormattableString = $"
Imports System
Imports System.Runtime.InteropServices

<Obsolete(""Suppress default ref struct obsolete errors"")>
Class TestClass
    Sub TestMethod(Of T As Span(Of Integer))()
    End Sub
End Class
"
        AssertThatShouldHaveError(source)
    End Sub

    <TestMethod>
    Public Sub TestRestrictedTypeAsTypeConstraint2()
        Dim source As FormattableString = $"
Imports System
Imports System.Runtime.InteropServices

<Obsolete(""Suppress default ref struct obsolete errors"")>
Class TestClass
    Sub TestMethod(Of T As {{Span(Of Integer), IDisposable}})()
    End Sub
End Class
"
        AssertThatShouldHaveError(source)
    End Sub

    ' ====================
    ' Inherits 声明签名测试
    ' ====================

    <TestMethod>
    Public Sub TestRestrictedTypeInInheritsClause()
        Dim snippetContent = "Inherits List(Of Span(Of Integer))"
        AssertThatDiagTriggeredInClass(snippetContent)
    End Sub

    <TestMethod>
    Public Sub TestRestrictedTypeInInheritsClause2()
        Dim snippetContent = "Inherits Dictionary(Of String, Span(Of Integer))"
        AssertThatDiagTriggeredInClass(snippetContent)
    End Sub

    ' ====================
    ' Implements 声明签名测试
    ' ====================

    <TestMethod>
    Public Sub TestRestrictedTypeInImplementsClause()
        Dim snippetContent = "Implements IEnumerable(Of Span(Of Integer))"
        AssertThatDiagTriggeredInClass(snippetContent)
    End Sub

    <TestMethod>
    Public Sub TestRestrictedTypeInImplementsClause2()
        Dim snippetContent = "Implements IDictionary(Of String, Span(Of Integer))"
        AssertThatDiagTriggeredInClass(snippetContent)
    End Sub

    ' ====================
    ' Event 声明测试
    ' ====================

    <TestMethod>
    Public Sub TestRestrictedTypeInEventDeclaration()
        Dim snippetContent = "Event MyEvent As EventHandler(Of Span(Of Integer))"
        AssertThatDiagTriggeredInClass(snippetContent)
    End Sub

    <TestMethod>
    Public Sub TestRestrictedTypeInEventDeclaration2()
        Dim snippetContent = "Event MyEvent As Action(Of Span(Of Integer))"
        AssertThatDiagTriggeredInClass(snippetContent)
    End Sub

    <TestMethod>
    Public Sub TestRestrictedTypeInCustomEventDeclaration()
        Dim snippetContent = "Custom Event MyEvent As EventHandler(Of Span(Of Integer))"
        AssertThatDiagTriggeredInClass(snippetContent)
    End Sub

    ' ====================
    ' 字段声明测试
    ' ====================

    <TestMethod>
    Public Sub TestRestrictedTypeInFieldDeclaration()
        Dim snippetContent = "Private _field As List(Of Span(Of Integer))"
        AssertThatDiagTriggeredInClass(snippetContent)
    End Sub

    <TestMethod>
    Public Sub TestRestrictedTypeInFieldDeclaration2()
        Dim snippetContent = "Public Field As Dictionary(Of String, Span(Of Integer))"
        AssertThatDiagTriggeredInClass(snippetContent)
    End Sub

    <TestMethod>
    Public Sub TestRestrictedTypeInFieldDeclaration3()
        Dim snippetContent = "Protected Shared s_field As IEnumerable(Of Span(Of Integer))"
        AssertThatDiagTriggeredInClass(snippetContent)
    End Sub

    ' ====================
    ' 属性声明测试
    ' ====================

    <TestMethod>
    Public Sub TestRestrictedTypeInPropertyDeclaration()
        Dim snippetContent = "Public Property MyProp As List(Of Span(Of Integer))"
        AssertThatDiagTriggeredInClass(snippetContent)
    End Sub

    <TestMethod>
    Public Sub TestRestrictedTypeInPropertyDeclaration2()
        Dim snippetContent = "Private Property MyProp As Dictionary(Of String, Span(Of Integer))"
        AssertThatDiagTriggeredInClass(snippetContent)
    End Sub

    <TestMethod>
    Public Sub TestRestrictedTypeInReadOnlyProperty()
        Dim snippetContent = "Public ReadOnly Property MyProp As IEnumerable(Of Span(Of Integer))"
        AssertThatDiagTriggeredInClass(snippetContent)
    End Sub

    <TestMethod>
    Public Sub TestRestrictedTypeInWriteOnlyProperty()
        Dim snippetContent = "Public WriteOnly Property MyProp As ICollection(Of Span(Of Integer))"
        AssertThatDiagTriggeredInClass(snippetContent)
    End Sub

    <TestMethod>
    Public Sub TestRestrictedTypeInPropertyWithGetter()
        Dim snippetContent = "Public Property MyProp As List(Of Span(Of Integer))"
        AssertThatDiagTriggeredInClass(snippetContent)
    End Sub

    ' ====================
    ' Sub 参数/返回值测试
    ' ====================

    <TestMethod>
    Public Sub TestRestrictedTypeInSubParameter()
        Dim snippetContent = "Sub TestMethod(param As List(Of Span(Of Integer)))"
        AssertThatDiagTriggeredInClass(snippetContent)
    End Sub

    <TestMethod>
    Public Sub TestRestrictedTypeInSubParameter2()
        Dim snippetContent = "Sub TestMethod(param As Dictionary(Of String, Span(Of Integer)))"
        AssertThatDiagTriggeredInClass(snippetContent)
    End Sub

    <TestMethod>
    Public Sub TestRestrictedTypeInSubParameter3()
        Dim snippetContent = "Sub TestMethod(Optional param As IEnumerable(Of Span(Of Integer)) = Nothing)"
        AssertThatDiagTriggeredInClass(snippetContent)
    End Sub

    ' ====================
    ' Function 参数/返回值测试
    ' ====================

    <TestMethod>
    Public Sub TestRestrictedTypeInFunctionParameter()
        Dim snippetContent = "Function TestMethod(param As List(Of Span(Of Integer)))"
        AssertThatDiagTriggeredInClass(snippetContent)
    End Sub

    <TestMethod>
    Public Sub TestRestrictedTypeInFunctionParameter2()
        Dim snippetContent = "Function TestMethod(param As Dictionary(Of String, Span(Of Integer))) As Integer"
        AssertThatDiagTriggeredInClass(snippetContent)
    End Sub

    <TestMethod>
    Public Sub TestRestrictedTypeInFunctionReturnValue()
        Dim snippetContent = "Function TestMethod() As List(Of Span(Of Integer))"
        AssertThatDiagTriggeredInClass(snippetContent)
    End Sub

    <TestMethod>
    Public Sub TestRestrictedTypeInFunctionReturnValue2()
        Dim snippetContent = "Function TestMethod() As Dictionary(Of String, Span(Of Integer))"
        AssertThatDiagTriggeredInClass(snippetContent)
    End Sub

    <TestMethod>
    Public Sub TestRestrictedTypeInFunctionParameterAndReturnValue()
        Dim snippetContent = "Function TestMethod(param As IEnumerable(Of Span(Of Integer))) As List(Of Span(Of Integer))"
        AssertThatDiagTriggeredInClass(snippetContent)
    End Sub

    ' ====================
    ' 委托声明测试
    ' ====================

    <TestMethod>
    Public Sub TestRestrictedTypeInDelegateDeclaration()
        Dim snippetContent = "Delegate Function TestDelegate() As List(Of Span(Of Integer))"
        AssertThatDiagTriggeredInClass(snippetContent)
    End Sub

    <TestMethod>
    Public Sub TestRestrictedTypeInDelegateDeclaration2()
        Dim snippetContent = "Delegate Sub TestDelegate(param As Dictionary(Of String, Span(Of Integer)))"
        AssertThatDiagTriggeredInClass(snippetContent)
    End Sub

    <TestMethod>
    Public Sub TestRestrictedTypeInDelegateDeclaration3()
        Dim snippetContent = "Delegate Function TestDelegate(param As IEnumerable(Of Span(Of Integer))) As Dictionary(Of String, Span(Of Integer))"
        AssertThatDiagTriggeredInClass(snippetContent)
    End Sub

    ' ====================
    ' 受限类型的结构体中泛型参数测试
    ' ====================

    <TestMethod>
    Public Sub TestRestrictedTypeInRestrictedStructGenericParameter()
        Dim source As FormattableString = $"
Imports System
Imports System.Runtime.InteropServices

<Obsolete(""Suppress default ref struct obsolete errors"")>
<System.Runtime.CompilerServices.IsByRefLike>
Structure RestrictedStruct
    Public field As List(Of Span(Of Integer))
End Structure
"
        AssertThatShouldHaveError(source)
    End Sub

    <TestMethod>
    Public Sub TestRestrictedTypeInRestrictedStructGenericProperty()
        Dim source As FormattableString = $"
Imports System
Imports System.Runtime.InteropServices

<Obsolete(""Suppress default ref struct obsolete errors"")>
<System.Runtime.CompilerServices.IsByRefLike>
Structure RestrictedStruct
    Public Property MyProp As Dictionary(Of String, Span(Of Integer))
End Structure
"
        AssertThatShouldHaveError(source)
    End Sub

    <TestMethod>
    Public Sub TestRestrictedTypeInRestrictedStructGenericMethod()
        Dim source As FormattableString = $"
Imports System
Imports System.Runtime.InteropServices

<Obsolete(""Suppress default ref struct obsolete errors"")>
<System.Runtime.CompilerServices.IsByRefLike>
Structure RestrictedStruct
    Sub TestMethod(param As List(Of Span(Of Integer)))
    End Sub
End Structure
"
        AssertThatShouldHaveError(source)
    End Sub

    <TestMethod>
    Public Sub TestRestrictedTypeInRestrictedStructGenericFunction()
        Dim source As FormattableString = $"
Imports System
Imports System.Runtime.InteropServices

<Obsolete(""Suppress default ref struct obsolete errors"")>
<System.Runtime.CompilerServices.IsByRefLike>
Structure RestrictedStruct
    Function TestFunction() As List(Of Span(Of Integer))
        Return Nothing
    End Function
End Structure
"
        AssertThatShouldHaveError(source)
    End Sub

    ' ====================
    ' 正确用法测试
    ' ====================

    <TestMethod>
    Public Sub TestNormalGenericUsage()
        Dim source As FormattableString = $"
Imports System
Imports System.Collections.Generic

Class TestClass
    Sub TestMethod()
        Dim list As New List(Of Integer)
        Dim dict As New Dictionary(Of String, Integer)
    End Sub
End Class
"
        AssertThatShouldNotHaveError(source)
    End Sub

    <TestMethod>
    Public Sub TestNormalGenericConstraint()
        Dim source As FormattableString = $"
Imports System

Class TestClass
    Sub TestMethod(Of T As IDisposable)()
    End Sub
End Class
"
        AssertThatShouldNotHaveError(source)
    End Sub

End Class

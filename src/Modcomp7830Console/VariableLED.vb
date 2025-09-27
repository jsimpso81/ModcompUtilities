Option Strict On



Imports System.Windows.Forms
Imports System.Drawing
Imports System.Drawing.Drawing2D

Public Class VariableLED

    Inherits Panel

    Private _brightness As Integer = 0

    Sub New()
        SetStyle(ControlStyles.AllPaintingInWmPaint Or
                 ControlStyles.OptimizedDoubleBuffer Or
                 ControlStyles.ResizeRedraw Or
                 ControlStyles.UserPaint, True)
        UpdateStyles()
    End Sub

    Public Property Brightness As Integer
        Get
            Return _brightness
        End Get

        Set(value As Integer)
            Dim _loc_value As Integer = value
            If (_loc_value > 100) Then
                _loc_value = 100
            ElseIf (_loc_value < 0) Then
                _loc_value = 0
            End If
            If _loc_value <> _brightness Then
                _brightness = _loc_value
                Invalidate()
            End If
        End Set
    End Property


    Protected Overrides Sub OnPaint(e As PaintEventArgs)
        Dim rec As New Rectangle(2, 2, Height - 5, Height - 5)
        '--Dim recText As New Rectangle(Height + 2, 1, Width - (Height - 2), Height)

        Dim G As Graphics = e.Graphics

        G.SmoothingMode = SmoothingMode.AntiAlias

        '--G.Clear(Parent.BackColor)

        'TODO: make this variable
        Dim DblBright As Double = CDbl(_brightness) * 0.01    ' 0 to 1
        Dim Red0 As Integer = CInt(DblBright * (255.0# - 30.0#) + 30.0#)
        Dim Red1 As Integer = Red0
        Dim Red2 As Integer = Red0

        ' --------normal orange is 165.  make it more red for now...
        Dim Grn0 As Integer = CInt(DblBright * (80.0# - 30.0#) + 30.0#)
        Dim Grn1 As Integer = CInt(DblBright * (110.0# - 30.0#) + 30.0#)
        Dim Grn2 As Integer = CInt(DblBright * (145.0# - 30.0#) + 30.0#)

        '--------was 100.   This would normally be 0 for red / orange.  This makes it a little white..
        Dim Blu0 As Integer = CInt(DblBright * (40.0# - 30.0#) + 30.0#)
        Dim Blu1 As Integer = Blu0
        Dim Blu2 As Integer = Blu0

        Dim cb As New ColorBlend With {
        .Colors = {Color.FromArgb(Red0, Grn0, Blu0), Color.FromArgb(Red1, Grn1, Blu1), Color.FromArgb(Red2, Grn2, Blu2)},
        .Positions = {0, 0.4, 1.0}
        }
        Using lgb As New LinearGradientBrush(rec, Color.Empty, Color.Empty, 90.0F) With {.InterpolationColors = cb}
            G.FillEllipse(lgb, rec)
        End Using

        G.TextRenderingHint = Drawing.Text.TextRenderingHint.ClearTypeGridFit

        'Using br As New SolidBrush(ForeColor)
        'Using sf As New StringFormat With {.Alignment = StringAlignment.Near, .LineAlignment = StringAlignment.Center}
        'G.DrawString(If(_brightness > 50, _onText, _offText), Font, br, recText, sf)
        'End Using
        'End Using

    End Sub




End Class

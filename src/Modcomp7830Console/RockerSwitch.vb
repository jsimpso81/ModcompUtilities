Option Strict On

Imports System.Windows.Forms
Imports System.Drawing
Imports System.Drawing.Drawing2D


Public Class RockerSwitch

    Inherits UserControl

    Private _isOn As Boolean = False


    Public Property IsOn As Boolean
        Get
            Return _isOn
        End Get
        Set(value As Boolean)
            _isOn = value
            Me.Invalidate() ' Redraw the control
        End Set
    End Property

    Public Sub Toggle()
        _isOn = Not _isOn
        Debug.WriteLine(" isON = " & _isOn.ToString)
        Me.Invalidate()
    End Sub

    Public Event Toggled(ByVal sender As Object, ByVal e As EventArgs)


    Public Sub New()
        Me.DoubleBuffered = True
        Me.Size = New Size(40, 80) ' Default size
    End Sub


    Protected Overrides Sub OnPaint(e As PaintEventArgs)
        MyBase.OnPaint(e)

        Dim g As Graphics = e.Graphics
        g.SmoothingMode = Drawing2D.SmoothingMode.AntiAlias

        ' Background
        Dim bgColor As Color = If(_isOn, Color.Orange, Color.DarkOrange)
        g.FillRectangle(New SolidBrush(bgColor), Me.ClientRectangle)

        ' Rocker switch
        Dim switchRect As Rectangle
        If _isOn Then
            switchRect = New Rectangle(2, 2, Me.Width - 4, Me.Height \ 2 - 4)
        Else
            switchRect = New Rectangle(2, Me.Height \ 2 + 2, Me.Width - 4, Me.Height \ 2 - 4)
        End If
        g.FillRectangle(Brushes.White, switchRect)
        g.DrawRectangle(Pens.Black, switchRect)

        ' Border
        g.DrawRectangle(Pens.Black, 0, 0, Me.Width - 1, Me.Height - 1)
    End Sub

    Private Sub InitializeComponent()
        SuspendLayout()
        ' 
        ' RockerSwitch
        ' 
        BackColor = Color.Black
        Name = "RockerSwitch"
        Size = New Size(47, 70)
        ResumeLayout(False)

    End Sub

    '--Protected Overrides Sub OnMouseClick(e As MouseEventArgs)
    '--MyBase.OnMouseClick(e)

    '--    ' Toggle the state
    '--    _isOn = Not _isOn
    '--RaiseEvent Toggled(Me, EventArgs.Empty)
    '--Me.Invalidate() ' Redraw the control
    '--End Sub
End Class


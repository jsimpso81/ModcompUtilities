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

        ' -------- Background
        '--Dim bgColor As Color = If(_isOn, Color.Orange, Color.DarkOrange)
        '--g.FillRectangle(New SolidBrush(bgColor), Me.ClientRectangle)
        Dim bgColor0 As Color = Color.Goldenrod
        Dim bgColor1 As Color = If(_isOn, Color.Gold, Color.Goldenrod)          ' light     medium
        Dim bgColor2 As Color = If(_isOn, Color.DarkGoldenrod, Color.Gold)               ' dark      light
        Dim bgColor3 As Color = If(_isOn, Color.Goldenrod, Color.DarkGoldenrod)         ' medium,   dark
        g.FillRectangle(New SolidBrush(bgColor0), Me.ClientRectangle)

        ' -------- Rocker switch
        ' -------- 47 x 70                      1,12,35,57,69
        Dim switchRect1 As Rectangle
        Dim switchRect2 As Rectangle
        Dim switchRect3 As Rectangle
        '-------- ON
        If _isOn Then
            ' -------                  x y wid height
            switchRect1 = New Rectangle(1, 1, Me.Width - 3, 11)
            switchRect2 = New Rectangle(1, 12, Me.Width - 3, 23)
            switchRect3 = New Rectangle(1, 36, Me.Width - 3, 35)
        Else
            ' -------                  x y wid height
            switchRect1 = New Rectangle(1, 1, Me.Width - 3, 35)
            switchRect2 = New Rectangle(1, 36, Me.Width - 3, 23)
            switchRect3 = New Rectangle(1, 58, Me.Width - 3, 11)
        End If
        g.FillRectangle(New SolidBrush(bgColor1), switchRect1)
        g.FillRectangle(New SolidBrush(bgColor2), switchRect2)
        g.FillRectangle(New SolidBrush(bgColor3), switchRect3)
        g.DrawRectangle(Pens.Black, switchRect1)
        g.DrawRectangle(Pens.Black, switchRect2)
        g.DrawRectangle(Pens.Black, switchRect3)

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


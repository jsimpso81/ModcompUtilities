Public Class SimpleLED
    Inherits Control

    ' Public property to control the LED's state (On/Off)
    Private _isOn As Boolean = False
    Public Property IsOn() As Boolean
        Get
            Return _isOn
        End Get
        Set(value As Boolean)
            If _isOn <> value Then
                _isOn = value
                Me.Invalidate() ' Redraw the control when the state changes
            End If
        End Set
    End Property

    ' Public property to set the LED's color when On
    Private _ledColor As Color = Color.Red
    Public Property LEDColor() As Color
        Get
            Return _ledColor
        End Get
        Set(value As Color)
            If _ledColor <> value Then
                _ledColor = value
                Me.Invalidate() ' Redraw the control when the color changes
            End If
        End Set
    End Property

    Protected Overrides Sub OnPaint(e As PaintEventArgs)
        MyBase.OnPaint(e)

        ' Define the bounds for the LED circle
        Dim rect As New Rectangle(0, 0, Me.Width - 1, Me.Height - 1)

        ' Draw the LED
        If _isOn Then
            ' Draw a filled circle with the specified LEDColor
            Using brush As New SolidBrush(_ledColor)
                e.Graphics.FillEllipse(brush, rect)
            End Using
            ' Optionally, draw a border for a more defined look
            Using pen As New Pen(Color.DarkGray, 1)
                e.Graphics.DrawEllipse(pen, rect)
            End Using
        Else
            ' Draw an empty circle or a dark gray filled circle for the "off" state
            Using brush As New SolidBrush(Color.DarkGray)
                e.Graphics.FillEllipse(brush, rect)
            End Using
            ' Optionally, draw a border
            Using pen As New Pen(Color.Gray, 1)
                e.Graphics.DrawEllipse(pen, rect)
            End Using
        End If
    End Sub

    ' Constructor to set default size
    Public Sub New()
        Me.Size = New Size(20, 20) ' Default size for the LED
    End Sub



End Class

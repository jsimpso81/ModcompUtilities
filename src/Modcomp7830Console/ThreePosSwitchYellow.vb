Imports System.Runtime.InteropServices.JavaScript.JSType

Public Class ThreePosSwitchYellow

    Private Enum SwitchPositionValue
        PosLatched
        PosOff
        PosOn
    End Enum

    Private _value As Boolean = False
    Private _latched As Boolean = False
    Private _enableLatch As Boolean = True
    Private _position As SwitchPositionValue = SwitchPositionValue.PosOff

    'Private color_B As Color = Color.LightGoldenrodYellow
    'Private color_N As Color = Color.Goldenrod
    'Private color_D As Color = Color.DarkGoldenrod
    'Private color_G As Color = Color.FromArgb(40, 40, 40)
    Private color_B As Color = Color.FromArgb(250, 250, 32)
    Private color_N As Color = Color.FromArgb(200, 200, 0)
    Private color_D As Color = Color.FromArgb(160, 160, 0)
    Private color_G As Color = Color.FromArgb(40, 40, 40)


    ' -------- rectangles   47 x 70    1,12,35,57,69
    Private switchRect1 As Rectangle
    Private switchRect2 As Rectangle
    Private switchRect3 As Rectangle
    Private switchRect4 As Rectangle

    '-------- colores
    Private bgColor0 As Color = color_N                        ' medium
    Private bgColor1 As Color ' = If(_value, color_B, color_N)    ' light     medium
    Private bgColor2 As Color ' = If(_value, color_D, color_B)    ' dark      light
    Private bgColor3 As Color ' = If(_value, color_N, color_D)    ' medium,   dark
    Private bgColor4 As Color ' = If(_value, color_N, color_D)    ' medium,   dark

    Public Event LatchClick As EventHandler
    Public Event ToggleClick As EventHandler

    ''' <summary>
    ''' Value - value of this switch true or false.   The value can only
    ''' be set to false if the switch is not latched.
    ''' </summary>
    ''' <returns></returns>
    Public Property Value As Boolean
        Get
            Return _value
        End Get
        Set(value As Boolean)
            _value = value Or _latched
            Me.Invalidate() ' Redraw the control
        End Set
    End Property

    Public Sub New()
        InitializeComponent()
        '--------Set up areas (e.g., split control in half vertically)
        UpdateRectangles()
    End Sub


    ''' <summary>
    ''' Latch - Latch the switch in the on state.   Latched also sets the
    ''' value to true
    ''' </summary>
    ''' <returns></returns>
    Public Property Latch As Boolean
        Get
            Return _latched
        End Get
        Set(latched As Boolean)
            _latched = latched And _enableLatch
            If (_latched) Then _value = True
            Me.Invalidate() ' Redraw the control
        End Set
    End Property

    ''' <summary>
    ''' LatchEnable - allows latching the switch.  When latched the switch is shown
    ''' in the up position.  When disabled, the latch value is also turned off.
    ''' </summary>
    ''' <returns></returns>
    Public Property LatchEnable As Boolean
        Get
            Return _enableLatch
        End Get
        Set(enablelatch As Boolean)
            _enableLatch = enablelatch
            If (Not _enableLatch) Then _latched = False
            Me.Invalidate() ' Redraw the control
        End Set
    End Property

    Public Sub SetLatched()
        If Me.LatchEnable Then
            Me.Latch = True
            Me.Value = True     '--just in case
            _position = SwitchPositionValue.PosLatched
            Me.Invalidate() ' Redraw the control
        End If
    End Sub

    ''' <summary>
    ''' Display switch in off position.  The value could still be on.
    ''' This doesn't update value.   It clears latched and sets the position to off.
    ''' </summary>
    Public Sub SetOff()
        If Me.Latch Then Me.Latch = False
        _position = SwitchPositionValue.PosOff
        Me.Invalidate() ' Redraw the control
    End Sub

    ''' <summary>
    ''' Display switch in on position.  This sets the value to on and clears the latched state.
    ''' </summary>
    Public Sub SetOn()
        Me.Latch = False
        Me.Value = True
        _position = SwitchPositionValue.PosOn
        Me.Invalidate() ' Redraw the control
    End Sub


    Protected Overrides Sub OnPaint(ByVal e As System.Windows.Forms.PaintEventArgs)

        MyBase.OnPaint(e)

        Dim g As Graphics = e.Graphics
        g.SmoothingMode = Drawing2D.SmoothingMode.AntiAlias

        g.FillRectangle(New SolidBrush(bgColor0), Me.ClientRectangle)

        '--------update rectangles
        Call UpdateRectangles()

        ' -------- draw...
        g.FillRectangle(New SolidBrush(bgColor1), switchRect1)
        g.FillRectangle(New SolidBrush(bgColor2), switchRect2)
        If (_position <> SwitchPositionValue.PosOff) Then g.FillRectangle(New SolidBrush(bgColor3), switchRect3)
        g.FillRectangle(New SolidBrush(bgColor4), switchRect4)

        g.DrawRectangle(Pens.Black, switchRect1)
        g.DrawRectangle(Pens.Black, switchRect2)
        If (_position <> SwitchPositionValue.PosOff) Then g.DrawRectangle(Pens.Black, switchRect3)
        g.DrawRectangle(Pens.Black, switchRect4)

        ' -------- Border
        g.DrawRectangle(Pens.Black, 0, 0, Me.Width - 1, Me.Height - 1)

        Return

    End Sub


    Private Sub UpdateRectangles()

        Dim ctlWidth As Integer = Me.Width
        Dim ctrRectLeft As Integer = 1
        Dim ctrRectRight As Integer = ctlWidth - 3
        Dim ctlHeight As Integer = Me.Height
        Dim ctlH1 As Integer
        Dim ctlH2 As Integer
        Dim ctlH3 As Integer
        Dim ctlH4 As Integer
        Dim ctlS1 As Integer
        Dim ctlS2 As Integer
        Dim ctlS3 As Integer
        Dim ctlS4 As Integer
        Dim ctlE1 As Integer
        Dim ctlE2 As Integer
        Dim ctlE3 As Integer
        Dim ctlE4 As Integer

        Select Case (_position)
            '--------latched - switch is up....
            Case SwitchPositionValue.PosLatched
                ctlH1 = ctlHeight * 0.15
                If ctlH1 <= 0 Then ctlH1 = 1
                ctlS1 = 1
                ctlE1 = ctlS1 - 1 + ctlH1

                ctlH2 = ctlHeight * 0.025
                If ctlH2 <= 0 Then ctlH2 = 1
                ctlS2 = ctlE1 + 2
                ctlE2 = ctlS2 - 1 + ctlH2

                ctlH3 = ctlHeight * 0.275
                If ctlH3 <= 0 Then ctlH3 = 1
                ctlS3 = ctlE2 + 2
                ctlE3 = ctlS3 - 1 + ctlH3

                ctlH4 = ctlHeight - ctlH1 - ctlH2 - ctlH3 - 5
                If ctlH4 <= 0 Then ctlH4 = 1
                ctlS4 = ctlE3 + 1
                ctlE4 = ctlS4 - 1 + ctlH4

                bgColor1 = color_B
                bgColor2 = color_G
                bgColor3 = color_D
                bgColor4 = color_N
                switchRect1 = New Rectangle(ctrRectLeft, ctlS1, ctrRectRight, ctlH1)
                switchRect2 = New Rectangle(ctrRectLeft, ctlS2, ctrRectRight, ctlH2)
                switchRect3 = New Rectangle(ctrRectLeft, ctlS3, ctrRectRight, ctlH3)
                switchRect4 = New Rectangle(ctrRectLeft, ctlS4, ctrRectRight, ctlH4)
                Exit Select

            ' --------off - switch is in middle.
            Case SwitchPositionValue.PosOff
                ctlH1 = ctlHeight * 0.45
                If ctlH1 <= 0 Then ctlH1 = 1
                ctlS1 = 1
                ctlE1 = ctlS1 - 1 + ctlH1

                ctlH2 = ctlHeight * 0.025
                If ctlH2 <= 0 Then ctlH2 = 1
                ctlS2 = ctlE1 + 2
                ctlE2 = ctlS2 - 1 + ctlH2

                ctlH3 = ctlHeight - ctlH1 - ctlH2 - 4
                If ctlH3 <= 0 Then ctlH3 = 1
                ctlS3 = ctlE2 + 2
                ctlE3 = ctlS3 - 1 + ctlH3

                bgColor1 = color_N
                bgColor2 = color_G
                bgColor3 = color_N
                switchRect1 = New Rectangle(ctrRectLeft, ctlS1, ctrRectRight, ctlH1)
                switchRect2 = New Rectangle(ctrRectLeft, ctlS2, ctrRectRight, ctlH2)
                switchRect4 = New Rectangle(ctrRectLeft, ctlS3, ctrRectRight, ctlH3)
                Exit Select

            Case SwitchPositionValue.PosOn
                ctlH1 = ctlHeight * 0.45
                If ctlH1 <= 0 Then ctlH1 = 1
                ctlS1 = 1
                ctlE1 = ctlS1 - 1 + ctlH1

                ctlH2 = ctlHeight * 0.275
                If ctlH2 <= 0 Then ctlH2 = 1
                ctlS2 = ctlE1 + 2
                ctlE2 = ctlS2 - 1 + ctlH2

                ctlH3 = ctlHeight * 0.025
                If ctlH3 <= 0 Then ctlH3 = 1
                ctlS3 = ctlE2 + 2
                ctlE3 = ctlS3 - 1 + ctlH3

                ctlH4 = ctlHeight - ctlH1 - ctlH2 - ctlH3 - 5
                If ctlH4 <= 0 Then ctlH4 = 1
                ctlS4 = ctlE3 + 1
                ctlE4 = ctlS4 - 1 + ctlH4

                bgColor1 = color_D
                bgColor2 = color_B
                bgColor3 = color_G
                bgColor4 = color_D
                switchRect1 = New Rectangle(ctrRectLeft, ctlS1, ctrRectRight, ctlH1)
                switchRect2 = New Rectangle(ctrRectLeft, ctlS2, ctrRectRight, ctlH2)
                switchRect3 = New Rectangle(ctrRectLeft, ctlS3, ctrRectRight, ctlH3)
                switchRect4 = New Rectangle(ctrRectLeft, ctlS4, ctrRectRight, ctlH4)
                Exit Select

        End Select


    End Sub


    Protected Overrides Sub OnMouseClick(e As MouseEventArgs)
        MyBase.OnMouseClick(e)

        ''If Not switchRect1 Is Nothing And Not switchRect4 Is Nothing Then
        If switchRect1.Contains(e.Location) Then
            RaiseEvent LatchClick(Me, EventArgs.Empty)
        ElseIf switchRect4.Contains(e.Location) Then
            RaiseEvent ToggleClick(Me, EventArgs.Empty)
        End If
        ''End If
    End Sub



End Class

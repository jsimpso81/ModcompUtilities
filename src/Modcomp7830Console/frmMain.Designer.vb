<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()>
Partial Class frmMain
    Inherits System.Windows.Forms.Form

    'Form overrides dispose to clean up the component list.
    <System.Diagnostics.DebuggerNonUserCode()>
    Protected Overrides Sub Dispose(disposing As Boolean)
        Try
            If disposing AndAlso components IsNot Nothing Then
                components.Dispose()
            End If
        Finally
            MyBase.Dispose(disposing)
        End Try
    End Sub

    'Required by the Windows Form Designer
    Private components As System.ComponentModel.IContainer

    'NOTE: The following procedure is required by the Windows Form Designer
    'It can be modified using the Windows Form Designer.  
    'Do not modify it using the code editor.
    <System.Diagnostics.DebuggerStepThrough()>
    Private Sub InitializeComponent()
        components = New ComponentModel.Container()
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(frmMain))
        StatusStrip1 = New StatusStrip()
        lblComStatus = New ToolStripStatusLabel()
        lblComBytes = New ToolStripStatusLabel()
        MenuStrip1 = New MenuStrip()
        FileToolStripMenuItem = New ToolStripMenuItem()
        ExitToolStripMenuItem = New ToolStripMenuItem()
        ExitToolStripMenuItem1 = New ToolStripMenuItem()
        HelpToolStripMenuItem = New ToolStripMenuItem()
        HelpToolStripMenuItem1 = New ToolStripMenuItem()
        AboutToolStripMenuItem = New ToolStripMenuItem()
        ListView1 = New ListView()
        PictureBox1 = New PictureBox()
        Timer1 = New Timer(components)
        LED_Power = New VariableLED()
        LED_Addr15 = New VariableLED()
        LED_Addr14 = New VariableLED()
        LED_Addr12 = New VariableLED()
        LED_Addr13 = New VariableLED()
        LED_Addr08 = New VariableLED()
        LED_Addr09 = New VariableLED()
        LED_Addr10 = New VariableLED()
        LED_Addr11 = New VariableLED()
        LED_Addr00 = New VariableLED()
        LED_Addr01 = New VariableLED()
        LED_Addr02 = New VariableLED()
        LED_Addr03 = New VariableLED()
        LED_Addr04 = New VariableLED()
        LED_Addr05 = New VariableLED()
        LED_Addr06 = New VariableLED()
        LED_Addr07 = New VariableLED()
        LED_Data00 = New VariableLED()
        LED_Data01 = New VariableLED()
        LED_Data02 = New VariableLED()
        LED_Data03 = New VariableLED()
        LED_Data04 = New VariableLED()
        LED_Data05 = New VariableLED()
        LED_Data06 = New VariableLED()
        LED_Data07 = New VariableLED()
        LED_Data08 = New VariableLED()
        LED_Data09 = New VariableLED()
        LED_Data10 = New VariableLED()
        LED_Data11 = New VariableLED()
        LED_Data12 = New VariableLED()
        LED_Data13 = New VariableLED()
        LED_Data14 = New VariableLED()
        LED_Data15 = New VariableLED()
        LED_Switch00 = New VariableLED()
        LED_Switch01 = New VariableLED()
        LED_Switch02 = New VariableLED()
        LED_Switch03 = New VariableLED()
        LED_Switch04 = New VariableLED()
        LED_Switch05 = New VariableLED()
        LED_Switch06 = New VariableLED()
        LED_Switch07 = New VariableLED()
        LED_Switch08 = New VariableLED()
        LED_Switch09 = New VariableLED()
        LED_Switch10 = New VariableLED()
        LED_Switch11 = New VariableLED()
        LED_Switch12 = New VariableLED()
        LED_Switch13 = New VariableLED()
        LED_Switch14 = New VariableLED()
        LED_Switch15 = New VariableLED()
        LED_Standby = New VariableLED()
        LED_BackupFailure = New VariableLED()
        LED_Run = New VariableLED()
        LED_Virt = New VariableLED()
        LED_PM = New VariableLED()
        LED_Priv = New VariableLED()
        LED_MemProt = New VariableLED()
        LED_EMA00 = New VariableLED()
        LED_EMA01 = New VariableLED()
        LED_EMA02 = New VariableLED()
        LED_EMA03 = New VariableLED()
        LED_CC_C = New VariableLED()
        LED_CC_O = New VariableLED()
        LED_CC_Z = New VariableLED()
        LED_CC_N = New VariableLED()
        LED_MemErr = New VariableLED()
        LED_TaskInt = New VariableLED()
        LED_IoInt = New VariableLED()
        RegSelSwitch08 = New RockerSwitch()
        RegSelSwitch07 = New RockerSwitch()
        RegSelSwitch06 = New RockerSwitch()
        RegSelSwitch05 = New RockerSwitch()
        RegSelSwitch04 = New RockerSwitch()
        RegSelSwitch03 = New RockerSwitch()
        RegSelSwitch02 = New RockerSwitch()
        RegSelSwitch01 = New RockerSwitch()
        StatusStrip1.SuspendLayout()
        MenuStrip1.SuspendLayout()
        CType(PictureBox1, ComponentModel.ISupportInitialize).BeginInit()
        SuspendLayout()
        ' 
        ' StatusStrip1
        ' 
        StatusStrip1.Items.AddRange(New ToolStripItem() {lblComStatus, lblComBytes})
        StatusStrip1.Location = New Point(0, 642)
        StatusStrip1.Name = "StatusStrip1"
        StatusStrip1.Size = New Size(1297, 22)
        StatusStrip1.TabIndex = 0
        StatusStrip1.Text = "StatusStrip1"
        ' 
        ' lblComStatus
        ' 
        lblComStatus.Name = "lblComStatus"
        lblComStatus.Size = New Size(118, 17)
        lblComStatus.Text = "No Communications"
        ' 
        ' lblComBytes
        ' 
        lblComBytes.Name = "lblComBytes"
        lblComBytes.Size = New Size(13, 17)
        lblComBytes.Text = "0"
        ' 
        ' MenuStrip1
        ' 
        MenuStrip1.Items.AddRange(New ToolStripItem() {FileToolStripMenuItem, HelpToolStripMenuItem})
        MenuStrip1.Location = New Point(0, 0)
        MenuStrip1.Name = "MenuStrip1"
        MenuStrip1.Size = New Size(1297, 24)
        MenuStrip1.TabIndex = 2
        MenuStrip1.Text = "MenuStrip1"
        ' 
        ' FileToolStripMenuItem
        ' 
        FileToolStripMenuItem.DropDownItems.AddRange(New ToolStripItem() {ExitToolStripMenuItem, ExitToolStripMenuItem1})
        FileToolStripMenuItem.Name = "FileToolStripMenuItem"
        FileToolStripMenuItem.Size = New Size(37, 20)
        FileToolStripMenuItem.Text = "File"
        ' 
        ' ExitToolStripMenuItem
        ' 
        ExitToolStripMenuItem.Name = "ExitToolStripMenuItem"
        ExitToolStripMenuItem.Size = New Size(116, 22)
        ExitToolStripMenuItem.Text = "Settings"
        ' 
        ' ExitToolStripMenuItem1
        ' 
        ExitToolStripMenuItem1.Name = "ExitToolStripMenuItem1"
        ExitToolStripMenuItem1.Size = New Size(116, 22)
        ExitToolStripMenuItem1.Text = "Exit"
        ' 
        ' HelpToolStripMenuItem
        ' 
        HelpToolStripMenuItem.DropDownItems.AddRange(New ToolStripItem() {HelpToolStripMenuItem1, AboutToolStripMenuItem})
        HelpToolStripMenuItem.Name = "HelpToolStripMenuItem"
        HelpToolStripMenuItem.Size = New Size(44, 20)
        HelpToolStripMenuItem.Text = "Help"
        ' 
        ' HelpToolStripMenuItem1
        ' 
        HelpToolStripMenuItem1.Name = "HelpToolStripMenuItem1"
        HelpToolStripMenuItem1.Size = New Size(107, 22)
        HelpToolStripMenuItem1.Text = "Help"
        ' 
        ' AboutToolStripMenuItem
        ' 
        AboutToolStripMenuItem.Name = "AboutToolStripMenuItem"
        AboutToolStripMenuItem.Size = New Size(107, 22)
        AboutToolStripMenuItem.Text = "About"
        ' 
        ' ListView1
        ' 
        ListView1.Location = New Point(97, 174)
        ListView1.Name = "ListView1"
        ListView1.Size = New Size(8, 8)
        ListView1.TabIndex = 3
        ListView1.UseCompatibleStateImageBehavior = False
        ' 
        ' PictureBox1
        ' 
        PictureBox1.Image = CType(resources.GetObject("PictureBox1.Image"), Image)
        PictureBox1.Location = New Point(0, 27)
        PictureBox1.Name = "PictureBox1"
        PictureBox1.Size = New Size(1290, 603)
        PictureBox1.TabIndex = 4
        PictureBox1.TabStop = False
        ' 
        ' Timer1
        ' 
        Timer1.Enabled = True
        Timer1.Interval = 50
        ' 
        ' LED_Power
        ' 
        LED_Power.BackColor = Color.FromArgb(CByte(30), CByte(30), CByte(30))
        LED_Power.Brightness = 100
        LED_Power.Location = New Point(94, 106)
        LED_Power.Name = "LED_Power"
        LED_Power.Size = New Size(24, 24)
        LED_Power.TabIndex = 5
        ' 
        ' LED_Addr15
        ' 
        LED_Addr15.BackColor = Color.FromArgb(CByte(30), CByte(30), CByte(30))
        LED_Addr15.Brightness = 100
        LED_Addr15.Location = New Point(168, 162)
        LED_Addr15.Name = "LED_Addr15"
        LED_Addr15.Size = New Size(24, 24)
        LED_Addr15.TabIndex = 6
        ' 
        ' LED_Addr14
        ' 
        LED_Addr14.BackColor = Color.FromArgb(CByte(30), CByte(30), CByte(30))
        LED_Addr14.Brightness = 100
        LED_Addr14.Location = New Point(217, 162)
        LED_Addr14.Name = "LED_Addr14"
        LED_Addr14.Size = New Size(24, 24)
        LED_Addr14.TabIndex = 7
        ' 
        ' LED_Addr12
        ' 
        LED_Addr12.BackColor = Color.FromArgb(CByte(30), CByte(30), CByte(30))
        LED_Addr12.Brightness = 100
        LED_Addr12.Location = New Point(315, 162)
        LED_Addr12.Name = "LED_Addr12"
        LED_Addr12.Size = New Size(24, 24)
        LED_Addr12.TabIndex = 9
        ' 
        ' LED_Addr13
        ' 
        LED_Addr13.BackColor = Color.FromArgb(CByte(30), CByte(30), CByte(30))
        LED_Addr13.Brightness = 100
        LED_Addr13.Location = New Point(266, 162)
        LED_Addr13.Name = "LED_Addr13"
        LED_Addr13.Size = New Size(24, 24)
        LED_Addr13.TabIndex = 8
        ' 
        ' LED_Addr08
        ' 
        LED_Addr08.BackColor = Color.FromArgb(CByte(30), CByte(30), CByte(30))
        LED_Addr08.Brightness = 100
        LED_Addr08.Location = New Point(532, 162)
        LED_Addr08.Name = "LED_Addr08"
        LED_Addr08.Size = New Size(24, 24)
        LED_Addr08.TabIndex = 13
        ' 
        ' LED_Addr09
        ' 
        LED_Addr09.BackColor = Color.FromArgb(CByte(30), CByte(30), CByte(30))
        LED_Addr09.Brightness = 100
        LED_Addr09.Location = New Point(483, 162)
        LED_Addr09.Name = "LED_Addr09"
        LED_Addr09.Size = New Size(24, 24)
        LED_Addr09.TabIndex = 12
        ' 
        ' LED_Addr10
        ' 
        LED_Addr10.BackColor = Color.FromArgb(CByte(30), CByte(30), CByte(30))
        LED_Addr10.Brightness = 100
        LED_Addr10.Location = New Point(434, 162)
        LED_Addr10.Name = "LED_Addr10"
        LED_Addr10.Size = New Size(24, 24)
        LED_Addr10.TabIndex = 11
        ' 
        ' LED_Addr11
        ' 
        LED_Addr11.BackColor = Color.FromArgb(CByte(30), CByte(30), CByte(30))
        LED_Addr11.Brightness = 100
        LED_Addr11.Location = New Point(385, 162)
        LED_Addr11.Name = "LED_Addr11"
        LED_Addr11.Size = New Size(24, 24)
        LED_Addr11.TabIndex = 10
        ' 
        ' LED_Addr00
        ' 
        LED_Addr00.BackColor = Color.FromArgb(CByte(30), CByte(30), CByte(30))
        LED_Addr00.Brightness = 100
        LED_Addr00.Location = New Point(967, 162)
        LED_Addr00.Name = "LED_Addr00"
        LED_Addr00.Size = New Size(24, 24)
        LED_Addr00.TabIndex = 21
        ' 
        ' LED_Addr01
        ' 
        LED_Addr01.BackColor = Color.FromArgb(CByte(30), CByte(30), CByte(30))
        LED_Addr01.Brightness = 100
        LED_Addr01.Location = New Point(918, 162)
        LED_Addr01.Name = "LED_Addr01"
        LED_Addr01.Size = New Size(24, 24)
        LED_Addr01.TabIndex = 20
        ' 
        ' LED_Addr02
        ' 
        LED_Addr02.BackColor = Color.FromArgb(CByte(30), CByte(30), CByte(30))
        LED_Addr02.Brightness = 100
        LED_Addr02.Location = New Point(869, 162)
        LED_Addr02.Name = "LED_Addr02"
        LED_Addr02.Size = New Size(24, 24)
        LED_Addr02.TabIndex = 19
        ' 
        ' LED_Addr03
        ' 
        LED_Addr03.BackColor = Color.FromArgb(CByte(30), CByte(30), CByte(30))
        LED_Addr03.Brightness = 100
        LED_Addr03.Location = New Point(820, 162)
        LED_Addr03.Name = "LED_Addr03"
        LED_Addr03.Size = New Size(24, 24)
        LED_Addr03.TabIndex = 18
        ' 
        ' LED_Addr04
        ' 
        LED_Addr04.BackColor = Color.FromArgb(CByte(30), CByte(30), CByte(30))
        LED_Addr04.Brightness = 100
        LED_Addr04.Location = New Point(750, 162)
        LED_Addr04.Name = "LED_Addr04"
        LED_Addr04.Size = New Size(24, 24)
        LED_Addr04.TabIndex = 17
        ' 
        ' LED_Addr05
        ' 
        LED_Addr05.BackColor = Color.FromArgb(CByte(30), CByte(30), CByte(30))
        LED_Addr05.Brightness = 100
        LED_Addr05.Location = New Point(701, 162)
        LED_Addr05.Name = "LED_Addr05"
        LED_Addr05.Size = New Size(24, 24)
        LED_Addr05.TabIndex = 16
        ' 
        ' LED_Addr06
        ' 
        LED_Addr06.BackColor = Color.FromArgb(CByte(30), CByte(30), CByte(30))
        LED_Addr06.Brightness = 100
        LED_Addr06.Location = New Point(652, 162)
        LED_Addr06.Name = "LED_Addr06"
        LED_Addr06.Size = New Size(24, 24)
        LED_Addr06.TabIndex = 15
        ' 
        ' LED_Addr07
        ' 
        LED_Addr07.BackColor = Color.FromArgb(CByte(30), CByte(30), CByte(30))
        LED_Addr07.Brightness = 100
        LED_Addr07.Location = New Point(603, 162)
        LED_Addr07.Name = "LED_Addr07"
        LED_Addr07.Size = New Size(24, 24)
        LED_Addr07.TabIndex = 14
        ' 
        ' LED_Data00
        ' 
        LED_Data00.BackColor = Color.FromArgb(CByte(30), CByte(30), CByte(30))
        LED_Data00.Brightness = 100
        LED_Data00.Location = New Point(967, 215)
        LED_Data00.Name = "LED_Data00"
        LED_Data00.Size = New Size(24, 24)
        LED_Data00.TabIndex = 37
        ' 
        ' LED_Data01
        ' 
        LED_Data01.BackColor = Color.FromArgb(CByte(30), CByte(30), CByte(30))
        LED_Data01.Brightness = 100
        LED_Data01.Location = New Point(918, 215)
        LED_Data01.Name = "LED_Data01"
        LED_Data01.Size = New Size(24, 24)
        LED_Data01.TabIndex = 36
        ' 
        ' LED_Data02
        ' 
        LED_Data02.BackColor = Color.FromArgb(CByte(30), CByte(30), CByte(30))
        LED_Data02.Brightness = 100
        LED_Data02.Location = New Point(869, 215)
        LED_Data02.Name = "LED_Data02"
        LED_Data02.Size = New Size(24, 24)
        LED_Data02.TabIndex = 35
        ' 
        ' LED_Data03
        ' 
        LED_Data03.BackColor = Color.FromArgb(CByte(30), CByte(30), CByte(30))
        LED_Data03.Brightness = 100
        LED_Data03.Location = New Point(820, 215)
        LED_Data03.Name = "LED_Data03"
        LED_Data03.Size = New Size(24, 24)
        LED_Data03.TabIndex = 34
        ' 
        ' LED_Data04
        ' 
        LED_Data04.BackColor = Color.FromArgb(CByte(30), CByte(30), CByte(30))
        LED_Data04.Brightness = 100
        LED_Data04.Location = New Point(750, 215)
        LED_Data04.Name = "LED_Data04"
        LED_Data04.Size = New Size(24, 24)
        LED_Data04.TabIndex = 33
        ' 
        ' LED_Data05
        ' 
        LED_Data05.BackColor = Color.FromArgb(CByte(30), CByte(30), CByte(30))
        LED_Data05.Brightness = 100
        LED_Data05.Location = New Point(701, 215)
        LED_Data05.Name = "LED_Data05"
        LED_Data05.Size = New Size(24, 24)
        LED_Data05.TabIndex = 32
        ' 
        ' LED_Data06
        ' 
        LED_Data06.BackColor = Color.FromArgb(CByte(30), CByte(30), CByte(30))
        LED_Data06.Brightness = 100
        LED_Data06.Location = New Point(652, 215)
        LED_Data06.Name = "LED_Data06"
        LED_Data06.Size = New Size(24, 24)
        LED_Data06.TabIndex = 31
        ' 
        ' LED_Data07
        ' 
        LED_Data07.BackColor = Color.FromArgb(CByte(30), CByte(30), CByte(30))
        LED_Data07.Brightness = 100
        LED_Data07.Location = New Point(603, 215)
        LED_Data07.Name = "LED_Data07"
        LED_Data07.Size = New Size(24, 24)
        LED_Data07.TabIndex = 30
        ' 
        ' LED_Data08
        ' 
        LED_Data08.BackColor = Color.FromArgb(CByte(30), CByte(30), CByte(30))
        LED_Data08.Brightness = 100
        LED_Data08.Location = New Point(532, 215)
        LED_Data08.Name = "LED_Data08"
        LED_Data08.Size = New Size(24, 24)
        LED_Data08.TabIndex = 29
        ' 
        ' LED_Data09
        ' 
        LED_Data09.BackColor = Color.FromArgb(CByte(30), CByte(30), CByte(30))
        LED_Data09.Brightness = 100
        LED_Data09.Location = New Point(483, 215)
        LED_Data09.Name = "LED_Data09"
        LED_Data09.Size = New Size(24, 24)
        LED_Data09.TabIndex = 28
        ' 
        ' LED_Data10
        ' 
        LED_Data10.BackColor = Color.FromArgb(CByte(30), CByte(30), CByte(30))
        LED_Data10.Brightness = 100
        LED_Data10.Location = New Point(434, 215)
        LED_Data10.Name = "LED_Data10"
        LED_Data10.Size = New Size(24, 24)
        LED_Data10.TabIndex = 27
        ' 
        ' LED_Data11
        ' 
        LED_Data11.BackColor = Color.FromArgb(CByte(30), CByte(30), CByte(30))
        LED_Data11.Brightness = 100
        LED_Data11.Location = New Point(385, 215)
        LED_Data11.Name = "LED_Data11"
        LED_Data11.Size = New Size(24, 24)
        LED_Data11.TabIndex = 26
        ' 
        ' LED_Data12
        ' 
        LED_Data12.BackColor = Color.FromArgb(CByte(30), CByte(30), CByte(30))
        LED_Data12.Brightness = 100
        LED_Data12.Location = New Point(315, 215)
        LED_Data12.Name = "LED_Data12"
        LED_Data12.Size = New Size(24, 24)
        LED_Data12.TabIndex = 25
        ' 
        ' LED_Data13
        ' 
        LED_Data13.BackColor = Color.FromArgb(CByte(30), CByte(30), CByte(30))
        LED_Data13.Brightness = 100
        LED_Data13.Location = New Point(266, 215)
        LED_Data13.Name = "LED_Data13"
        LED_Data13.Size = New Size(24, 24)
        LED_Data13.TabIndex = 24
        ' 
        ' LED_Data14
        ' 
        LED_Data14.BackColor = Color.FromArgb(CByte(30), CByte(30), CByte(30))
        LED_Data14.Brightness = 100
        LED_Data14.Location = New Point(217, 215)
        LED_Data14.Name = "LED_Data14"
        LED_Data14.Size = New Size(24, 24)
        LED_Data14.TabIndex = 23
        ' 
        ' LED_Data15
        ' 
        LED_Data15.BackColor = Color.FromArgb(CByte(30), CByte(30), CByte(30))
        LED_Data15.Brightness = 100
        LED_Data15.Location = New Point(168, 215)
        LED_Data15.Name = "LED_Data15"
        LED_Data15.Size = New Size(24, 24)
        LED_Data15.TabIndex = 22
        ' 
        ' LED_Switch00
        ' 
        LED_Switch00.BackColor = Color.FromArgb(CByte(30), CByte(30), CByte(30))
        LED_Switch00.Brightness = 100
        LED_Switch00.Location = New Point(967, 270)
        LED_Switch00.Name = "LED_Switch00"
        LED_Switch00.Size = New Size(24, 24)
        LED_Switch00.TabIndex = 53
        ' 
        ' LED_Switch01
        ' 
        LED_Switch01.BackColor = Color.FromArgb(CByte(30), CByte(30), CByte(30))
        LED_Switch01.Brightness = 100
        LED_Switch01.Location = New Point(918, 270)
        LED_Switch01.Name = "LED_Switch01"
        LED_Switch01.Size = New Size(24, 24)
        LED_Switch01.TabIndex = 52
        ' 
        ' LED_Switch02
        ' 
        LED_Switch02.BackColor = Color.FromArgb(CByte(30), CByte(30), CByte(30))
        LED_Switch02.Brightness = 100
        LED_Switch02.Location = New Point(869, 270)
        LED_Switch02.Name = "LED_Switch02"
        LED_Switch02.Size = New Size(24, 24)
        LED_Switch02.TabIndex = 51
        ' 
        ' LED_Switch03
        ' 
        LED_Switch03.BackColor = Color.FromArgb(CByte(30), CByte(30), CByte(30))
        LED_Switch03.Brightness = 100
        LED_Switch03.Location = New Point(820, 270)
        LED_Switch03.Name = "LED_Switch03"
        LED_Switch03.Size = New Size(24, 24)
        LED_Switch03.TabIndex = 50
        ' 
        ' LED_Switch04
        ' 
        LED_Switch04.BackColor = Color.FromArgb(CByte(30), CByte(30), CByte(30))
        LED_Switch04.Brightness = 100
        LED_Switch04.Location = New Point(750, 270)
        LED_Switch04.Name = "LED_Switch04"
        LED_Switch04.Size = New Size(24, 24)
        LED_Switch04.TabIndex = 49
        ' 
        ' LED_Switch05
        ' 
        LED_Switch05.BackColor = Color.FromArgb(CByte(30), CByte(30), CByte(30))
        LED_Switch05.Brightness = 100
        LED_Switch05.Location = New Point(701, 270)
        LED_Switch05.Name = "LED_Switch05"
        LED_Switch05.Size = New Size(24, 24)
        LED_Switch05.TabIndex = 48
        ' 
        ' LED_Switch06
        ' 
        LED_Switch06.BackColor = Color.FromArgb(CByte(30), CByte(30), CByte(30))
        LED_Switch06.Brightness = 100
        LED_Switch06.Location = New Point(652, 270)
        LED_Switch06.Name = "LED_Switch06"
        LED_Switch06.Size = New Size(24, 24)
        LED_Switch06.TabIndex = 47
        ' 
        ' LED_Switch07
        ' 
        LED_Switch07.BackColor = Color.FromArgb(CByte(30), CByte(30), CByte(30))
        LED_Switch07.Brightness = 100
        LED_Switch07.Location = New Point(603, 270)
        LED_Switch07.Name = "LED_Switch07"
        LED_Switch07.Size = New Size(24, 24)
        LED_Switch07.TabIndex = 46
        ' 
        ' LED_Switch08
        ' 
        LED_Switch08.BackColor = Color.FromArgb(CByte(30), CByte(30), CByte(30))
        LED_Switch08.Brightness = 100
        LED_Switch08.Location = New Point(532, 270)
        LED_Switch08.Name = "LED_Switch08"
        LED_Switch08.Size = New Size(24, 24)
        LED_Switch08.TabIndex = 45
        ' 
        ' LED_Switch09
        ' 
        LED_Switch09.BackColor = Color.FromArgb(CByte(30), CByte(30), CByte(30))
        LED_Switch09.Brightness = 100
        LED_Switch09.Location = New Point(483, 270)
        LED_Switch09.Name = "LED_Switch09"
        LED_Switch09.Size = New Size(24, 24)
        LED_Switch09.TabIndex = 44
        ' 
        ' LED_Switch10
        ' 
        LED_Switch10.BackColor = Color.FromArgb(CByte(30), CByte(30), CByte(30))
        LED_Switch10.Brightness = 100
        LED_Switch10.Location = New Point(434, 270)
        LED_Switch10.Name = "LED_Switch10"
        LED_Switch10.Size = New Size(24, 24)
        LED_Switch10.TabIndex = 43
        ' 
        ' LED_Switch11
        ' 
        LED_Switch11.BackColor = Color.FromArgb(CByte(30), CByte(30), CByte(30))
        LED_Switch11.Brightness = 100
        LED_Switch11.Location = New Point(385, 270)
        LED_Switch11.Name = "LED_Switch11"
        LED_Switch11.Size = New Size(24, 24)
        LED_Switch11.TabIndex = 42
        ' 
        ' LED_Switch12
        ' 
        LED_Switch12.BackColor = Color.FromArgb(CByte(30), CByte(30), CByte(30))
        LED_Switch12.Brightness = 100
        LED_Switch12.Location = New Point(315, 270)
        LED_Switch12.Name = "LED_Switch12"
        LED_Switch12.Size = New Size(24, 24)
        LED_Switch12.TabIndex = 41
        ' 
        ' LED_Switch13
        ' 
        LED_Switch13.BackColor = Color.FromArgb(CByte(30), CByte(30), CByte(30))
        LED_Switch13.Brightness = 100
        LED_Switch13.Location = New Point(266, 270)
        LED_Switch13.Name = "LED_Switch13"
        LED_Switch13.Size = New Size(24, 24)
        LED_Switch13.TabIndex = 40
        ' 
        ' LED_Switch14
        ' 
        LED_Switch14.BackColor = Color.FromArgb(CByte(30), CByte(30), CByte(30))
        LED_Switch14.Brightness = 100
        LED_Switch14.Location = New Point(217, 270)
        LED_Switch14.Name = "LED_Switch14"
        LED_Switch14.Size = New Size(24, 24)
        LED_Switch14.TabIndex = 39
        ' 
        ' LED_Switch15
        ' 
        LED_Switch15.BackColor = Color.FromArgb(CByte(30), CByte(30), CByte(30))
        LED_Switch15.Brightness = 100
        LED_Switch15.Location = New Point(168, 270)
        LED_Switch15.Name = "LED_Switch15"
        LED_Switch15.Size = New Size(24, 24)
        LED_Switch15.TabIndex = 38
        ' 
        ' LED_Standby
        ' 
        LED_Standby.BackColor = Color.FromArgb(CByte(30), CByte(30), CByte(30))
        LED_Standby.Brightness = 100
        LED_Standby.Location = New Point(94, 144)
        LED_Standby.Name = "LED_Standby"
        LED_Standby.Size = New Size(24, 24)
        LED_Standby.TabIndex = 54
        ' 
        ' LED_BackupFailure
        ' 
        LED_BackupFailure.BackColor = Color.FromArgb(CByte(30), CByte(30), CByte(30))
        LED_BackupFailure.Brightness = 100
        LED_BackupFailure.Location = New Point(94, 190)
        LED_BackupFailure.Name = "LED_BackupFailure"
        LED_BackupFailure.Size = New Size(24, 24)
        LED_BackupFailure.TabIndex = 7
        ' 
        ' LED_Run
        ' 
        LED_Run.BackColor = Color.FromArgb(CByte(30), CByte(30), CByte(30))
        LED_Run.Brightness = 100
        LED_Run.Location = New Point(94, 270)
        LED_Run.Name = "LED_Run"
        LED_Run.Size = New Size(24, 24)
        LED_Run.TabIndex = 55
        ' 
        ' LED_Virt
        ' 
        LED_Virt.BackColor = Color.FromArgb(CByte(30), CByte(30), CByte(30))
        LED_Virt.Brightness = 100
        LED_Virt.Location = New Point(1184, 105)
        LED_Virt.Name = "LED_Virt"
        LED_Virt.Size = New Size(24, 24)
        LED_Virt.TabIndex = 59
        ' 
        ' LED_PM
        ' 
        LED_PM.BackColor = Color.FromArgb(CByte(30), CByte(30), CByte(30))
        LED_PM.Brightness = 100
        LED_PM.Location = New Point(1135, 105)
        LED_PM.Name = "LED_PM"
        LED_PM.Size = New Size(24, 24)
        LED_PM.TabIndex = 58
        ' 
        ' LED_Priv
        ' 
        LED_Priv.BackColor = Color.FromArgb(CByte(30), CByte(30), CByte(30))
        LED_Priv.Brightness = 100
        LED_Priv.Location = New Point(1086, 105)
        LED_Priv.Name = "LED_Priv"
        LED_Priv.Size = New Size(24, 24)
        LED_Priv.TabIndex = 57
        ' 
        ' LED_MemProt
        ' 
        LED_MemProt.BackColor = Color.FromArgb(CByte(30), CByte(30), CByte(30))
        LED_MemProt.Brightness = 100
        LED_MemProt.Location = New Point(1037, 105)
        LED_MemProt.Name = "LED_MemProt"
        LED_MemProt.Size = New Size(24, 24)
        LED_MemProt.TabIndex = 56
        ' 
        ' LED_EMA00
        ' 
        LED_EMA00.BackColor = Color.FromArgb(CByte(30), CByte(30), CByte(30))
        LED_EMA00.Brightness = 100
        LED_EMA00.Location = New Point(1184, 161)
        LED_EMA00.Name = "LED_EMA00"
        LED_EMA00.Size = New Size(24, 24)
        LED_EMA00.TabIndex = 63
        ' 
        ' LED_EMA01
        ' 
        LED_EMA01.BackColor = Color.FromArgb(CByte(30), CByte(30), CByte(30))
        LED_EMA01.Brightness = 100
        LED_EMA01.Location = New Point(1135, 161)
        LED_EMA01.Name = "LED_EMA01"
        LED_EMA01.Size = New Size(24, 24)
        LED_EMA01.TabIndex = 62
        ' 
        ' LED_EMA02
        ' 
        LED_EMA02.BackColor = Color.FromArgb(CByte(30), CByte(30), CByte(30))
        LED_EMA02.Brightness = 100
        LED_EMA02.Location = New Point(1086, 161)
        LED_EMA02.Name = "LED_EMA02"
        LED_EMA02.Size = New Size(24, 24)
        LED_EMA02.TabIndex = 61
        ' 
        ' LED_EMA03
        ' 
        LED_EMA03.BackColor = Color.FromArgb(CByte(30), CByte(30), CByte(30))
        LED_EMA03.Brightness = 100
        LED_EMA03.Location = New Point(1037, 161)
        LED_EMA03.Name = "LED_EMA03"
        LED_EMA03.Size = New Size(24, 24)
        LED_EMA03.TabIndex = 60
        ' 
        ' LED_CC_C
        ' 
        LED_CC_C.BackColor = Color.FromArgb(CByte(30), CByte(30), CByte(30))
        LED_CC_C.Brightness = 100
        LED_CC_C.Location = New Point(1184, 214)
        LED_CC_C.Name = "LED_CC_C"
        LED_CC_C.Size = New Size(24, 24)
        LED_CC_C.TabIndex = 67
        ' 
        ' LED_CC_O
        ' 
        LED_CC_O.BackColor = Color.FromArgb(CByte(30), CByte(30), CByte(30))
        LED_CC_O.Brightness = 100
        LED_CC_O.Location = New Point(1135, 214)
        LED_CC_O.Name = "LED_CC_O"
        LED_CC_O.Size = New Size(24, 24)
        LED_CC_O.TabIndex = 66
        ' 
        ' LED_CC_Z
        ' 
        LED_CC_Z.BackColor = Color.FromArgb(CByte(30), CByte(30), CByte(30))
        LED_CC_Z.Brightness = 100
        LED_CC_Z.Location = New Point(1086, 214)
        LED_CC_Z.Name = "LED_CC_Z"
        LED_CC_Z.Size = New Size(24, 24)
        LED_CC_Z.TabIndex = 65
        ' 
        ' LED_CC_N
        ' 
        LED_CC_N.BackColor = Color.FromArgb(CByte(30), CByte(30), CByte(30))
        LED_CC_N.Brightness = 100
        LED_CC_N.Location = New Point(1037, 214)
        LED_CC_N.Name = "LED_CC_N"
        LED_CC_N.Size = New Size(24, 24)
        LED_CC_N.TabIndex = 64
        ' 
        ' LED_MemErr
        ' 
        LED_MemErr.BackColor = Color.FromArgb(CByte(30), CByte(30), CByte(30))
        LED_MemErr.Brightness = 100
        LED_MemErr.Location = New Point(1135, 270)
        LED_MemErr.Name = "LED_MemErr"
        LED_MemErr.Size = New Size(24, 24)
        LED_MemErr.TabIndex = 70
        ' 
        ' LED_TaskInt
        ' 
        LED_TaskInt.BackColor = Color.FromArgb(CByte(30), CByte(30), CByte(30))
        LED_TaskInt.Brightness = 100
        LED_TaskInt.Location = New Point(1086, 270)
        LED_TaskInt.Name = "LED_TaskInt"
        LED_TaskInt.Size = New Size(24, 24)
        LED_TaskInt.TabIndex = 69
        ' 
        ' LED_IoInt
        ' 
        LED_IoInt.BackColor = Color.FromArgb(CByte(30), CByte(30), CByte(30))
        LED_IoInt.Brightness = 100
        LED_IoInt.Location = New Point(1037, 270)
        LED_IoInt.Name = "LED_IoInt"
        LED_IoInt.Size = New Size(24, 24)
        LED_IoInt.TabIndex = 68
        ' 
        ' RegSelSwitch08
        ' 
        RegSelSwitch08.BackColor = Color.FromArgb(CByte(192), CByte(192), CByte(0))
        RegSelSwitch08.IsOn = False
        RegSelSwitch08.Location = New Point(589, 478)
        RegSelSwitch08.Name = "RegSelSwitch08"
        RegSelSwitch08.Size = New Size(45, 70)
        RegSelSwitch08.TabIndex = 71
        ' 
        ' RegSelSwitch07
        ' 
        RegSelSwitch07.BackColor = Color.FromArgb(CByte(192), CByte(192), CByte(0))
        RegSelSwitch07.IsOn = False
        RegSelSwitch07.Location = New Point(640, 478)
        RegSelSwitch07.Name = "RegSelSwitch07"
        RegSelSwitch07.Size = New Size(45, 70)
        RegSelSwitch07.TabIndex = 72
        ' 
        ' RegSelSwitch06
        ' 
        RegSelSwitch06.BackColor = Color.FromArgb(CByte(192), CByte(192), CByte(0))
        RegSelSwitch06.IsOn = False
        RegSelSwitch06.Location = New Point(691, 478)
        RegSelSwitch06.Name = "RegSelSwitch06"
        RegSelSwitch06.Size = New Size(45, 70)
        RegSelSwitch06.TabIndex = 73
        ' 
        ' RegSelSwitch05
        ' 
        RegSelSwitch05.BackColor = Color.FromArgb(CByte(192), CByte(192), CByte(0))
        RegSelSwitch05.IsOn = False
        RegSelSwitch05.Location = New Point(742, 478)
        RegSelSwitch05.Name = "RegSelSwitch05"
        RegSelSwitch05.Size = New Size(45, 70)
        RegSelSwitch05.TabIndex = 74
        ' 
        ' RegSelSwitch04
        ' 
        RegSelSwitch04.BackColor = Color.FromArgb(CByte(192), CByte(192), CByte(0))
        RegSelSwitch04.IsOn = False
        RegSelSwitch04.Location = New Point(809, 478)
        RegSelSwitch04.Name = "RegSelSwitch04"
        RegSelSwitch04.Size = New Size(45, 70)
        RegSelSwitch04.TabIndex = 75
        ' 
        ' RegSelSwitch03
        ' 
        RegSelSwitch03.BackColor = Color.FromArgb(CByte(192), CByte(192), CByte(0))
        RegSelSwitch03.IsOn = False
        RegSelSwitch03.Location = New Point(860, 478)
        RegSelSwitch03.Name = "RegSelSwitch03"
        RegSelSwitch03.Size = New Size(45, 70)
        RegSelSwitch03.TabIndex = 76
        ' 
        ' RegSelSwitch02
        ' 
        RegSelSwitch02.BackColor = Color.FromArgb(CByte(192), CByte(192), CByte(0))
        RegSelSwitch02.IsOn = False
        RegSelSwitch02.Location = New Point(909, 478)
        RegSelSwitch02.Name = "RegSelSwitch02"
        RegSelSwitch02.Size = New Size(45, 70)
        RegSelSwitch02.TabIndex = 77
        ' 
        ' RegSelSwitch01
        ' 
        RegSelSwitch01.BackColor = Color.FromArgb(CByte(192), CByte(192), CByte(0))
        RegSelSwitch01.IsOn = False
        RegSelSwitch01.Location = New Point(959, 477)
        RegSelSwitch01.Name = "RegSelSwitch01"
        RegSelSwitch01.Size = New Size(45, 70)
        RegSelSwitch01.TabIndex = 78
        ' 
        ' frmMain
        ' 
        AutoScaleDimensions = New SizeF(7F, 15F)
        AutoScaleMode = AutoScaleMode.Font
        ClientSize = New Size(1297, 664)
        Controls.Add(RegSelSwitch01)
        Controls.Add(RegSelSwitch02)
        Controls.Add(RegSelSwitch03)
        Controls.Add(RegSelSwitch04)
        Controls.Add(RegSelSwitch05)
        Controls.Add(RegSelSwitch06)
        Controls.Add(RegSelSwitch07)
        Controls.Add(RegSelSwitch08)
        Controls.Add(LED_MemErr)
        Controls.Add(LED_TaskInt)
        Controls.Add(LED_IoInt)
        Controls.Add(LED_CC_C)
        Controls.Add(LED_CC_O)
        Controls.Add(LED_CC_Z)
        Controls.Add(LED_CC_N)
        Controls.Add(LED_EMA00)
        Controls.Add(LED_EMA01)
        Controls.Add(LED_EMA02)
        Controls.Add(LED_EMA03)
        Controls.Add(LED_Virt)
        Controls.Add(LED_PM)
        Controls.Add(LED_Priv)
        Controls.Add(LED_MemProt)
        Controls.Add(LED_Run)
        Controls.Add(LED_BackupFailure)
        Controls.Add(LED_Standby)
        Controls.Add(LED_Switch00)
        Controls.Add(LED_Switch01)
        Controls.Add(LED_Switch02)
        Controls.Add(LED_Switch03)
        Controls.Add(LED_Switch04)
        Controls.Add(LED_Switch05)
        Controls.Add(LED_Switch06)
        Controls.Add(LED_Switch07)
        Controls.Add(LED_Switch08)
        Controls.Add(LED_Switch09)
        Controls.Add(LED_Switch10)
        Controls.Add(LED_Switch11)
        Controls.Add(LED_Switch12)
        Controls.Add(LED_Switch13)
        Controls.Add(LED_Switch14)
        Controls.Add(LED_Switch15)
        Controls.Add(LED_Data00)
        Controls.Add(LED_Data01)
        Controls.Add(LED_Data02)
        Controls.Add(LED_Data03)
        Controls.Add(LED_Data04)
        Controls.Add(LED_Data05)
        Controls.Add(LED_Data06)
        Controls.Add(LED_Data07)
        Controls.Add(LED_Data08)
        Controls.Add(LED_Data09)
        Controls.Add(LED_Data10)
        Controls.Add(LED_Data11)
        Controls.Add(LED_Data12)
        Controls.Add(LED_Data13)
        Controls.Add(LED_Data14)
        Controls.Add(LED_Data15)
        Controls.Add(LED_Addr00)
        Controls.Add(LED_Addr01)
        Controls.Add(LED_Addr02)
        Controls.Add(LED_Addr03)
        Controls.Add(LED_Addr04)
        Controls.Add(LED_Addr05)
        Controls.Add(LED_Addr06)
        Controls.Add(LED_Addr07)
        Controls.Add(LED_Addr08)
        Controls.Add(LED_Addr09)
        Controls.Add(LED_Addr10)
        Controls.Add(LED_Addr11)
        Controls.Add(LED_Addr12)
        Controls.Add(LED_Addr13)
        Controls.Add(LED_Addr14)
        Controls.Add(LED_Addr15)
        Controls.Add(LED_Power)
        Controls.Add(ListView1)
        Controls.Add(StatusStrip1)
        Controls.Add(MenuStrip1)
        Controls.Add(PictureBox1)
        MainMenuStrip = MenuStrip1
        Name = "frmMain"
        Text = "Modcomp 7830 Front Panel"
        StatusStrip1.ResumeLayout(False)
        StatusStrip1.PerformLayout()
        MenuStrip1.ResumeLayout(False)
        MenuStrip1.PerformLayout()
        CType(PictureBox1, ComponentModel.ISupportInitialize).EndInit()
        ResumeLayout(False)
        PerformLayout()
    End Sub

    Friend WithEvents StatusStrip1 As StatusStrip
    Friend WithEvents MenuStrip1 As MenuStrip
    Friend WithEvents FileToolStripMenuItem As ToolStripMenuItem
    Friend WithEvents ExitToolStripMenuItem As ToolStripMenuItem
    Friend WithEvents HelpToolStripMenuItem As ToolStripMenuItem
    Friend WithEvents HelpToolStripMenuItem1 As ToolStripMenuItem
    Friend WithEvents AboutToolStripMenuItem As ToolStripMenuItem
    Friend WithEvents ListView1 As ListView
    Friend WithEvents PictureBox1 As PictureBox
    Friend WithEvents Timer1 As Timer
    Friend WithEvents LED_Power As VariableLED
    Friend WithEvents LED_Addr15 As VariableLED
    Friend WithEvents LED_Addr14 As VariableLED
    Friend WithEvents LED_Addr12 As VariableLED
    Friend WithEvents LED_Addr13 As VariableLED
    Friend WithEvents LED_Addr08 As VariableLED
    Friend WithEvents LED_Addr09 As VariableLED
    Friend WithEvents LED_Addr10 As VariableLED
    Friend WithEvents LED_Addr11 As VariableLED
    Friend WithEvents LED_Addr00 As VariableLED
    Friend WithEvents LED_Addr01 As VariableLED
    Friend WithEvents LED_Addr02 As VariableLED
    Friend WithEvents LED_Addr03 As VariableLED
    Friend WithEvents LED_Addr04 As VariableLED
    Friend WithEvents LED_Addr05 As VariableLED
    Friend WithEvents LED_Addr06 As VariableLED
    Friend WithEvents LED_Addr07 As VariableLED
    Friend WithEvents LED_Data00 As VariableLED
    Friend WithEvents LED_Data01 As VariableLED
    Friend WithEvents LED_Data02 As VariableLED
    Friend WithEvents LED_Data03 As VariableLED
    Friend WithEvents LED_Data04 As VariableLED
    Friend WithEvents LED_Data05 As VariableLED
    Friend WithEvents LED_Data06 As VariableLED
    Friend WithEvents LED_Data07 As VariableLED
    Friend WithEvents LED_Data08 As VariableLED
    Friend WithEvents LED_Data09 As VariableLED
    Friend WithEvents LED_Data10 As VariableLED
    Friend WithEvents LED_Data11 As VariableLED
    Friend WithEvents LED_Data12 As VariableLED
    Friend WithEvents LED_Data13 As VariableLED
    Friend WithEvents LED_Data14 As VariableLED
    Friend WithEvents LED_Data15 As VariableLED
    Friend WithEvents LED_Switch00 As VariableLED
    Friend WithEvents LED_Switch01 As VariableLED
    Friend WithEvents LED_Switch02 As VariableLED
    Friend WithEvents LED_Switch03 As VariableLED
    Friend WithEvents LED_Switch04 As VariableLED
    Friend WithEvents LED_Switch05 As VariableLED
    Friend WithEvents LED_Switch06 As VariableLED
    Friend WithEvents LED_Switch07 As VariableLED
    Friend WithEvents LED_Switch08 As VariableLED
    Friend WithEvents LED_Switch09 As VariableLED
    Friend WithEvents LED_Switch10 As VariableLED
    Friend WithEvents LED_Switch11 As VariableLED
    Friend WithEvents LED_Switch12 As VariableLED
    Friend WithEvents LED_Switch13 As VariableLED
    Friend WithEvents LED_Switch14 As VariableLED
    Friend WithEvents LED_Switch15 As VariableLED
    Friend WithEvents LED_Standby As VariableLED
    Friend WithEvents LED_BackupFailure As VariableLED
    Friend WithEvents LED_Run As VariableLED
    Friend WithEvents LED_Virt As VariableLED
    Friend WithEvents LED_PM As VariableLED
    Friend WithEvents LED_Priv As VariableLED
    Friend WithEvents LED_MemProt As VariableLED
    Friend WithEvents LED_EMA00 As VariableLED
    Friend WithEvents LED_EMA01 As VariableLED
    Friend WithEvents LED_EMA02 As VariableLED
    Friend WithEvents LED_EMA03 As VariableLED
    Friend WithEvents LED_CC_C As VariableLED
    Friend WithEvents LED_CC_O As VariableLED
    Friend WithEvents LED_CC_Z As VariableLED
    Friend WithEvents LED_CC_N As VariableLED
    Friend WithEvents LED_MemErr As VariableLED
    Friend WithEvents LED_TaskInt As VariableLED
    Friend WithEvents LED_IoInt As VariableLED
    Friend WithEvents ExitToolStripMenuItem1 As ToolStripMenuItem
    Friend WithEvents lblComStatus As ToolStripStatusLabel
    Friend WithEvents lblComBytes As ToolStripStatusLabel
    Friend WithEvents RegSelSwitch08 As RockerSwitch
    Friend WithEvents RegSelSwitch07 As RockerSwitch
    Friend WithEvents RegSelSwitch06 As RockerSwitch
    Friend WithEvents RegSelSwitch05 As RockerSwitch
    Friend WithEvents RegSelSwitch04 As RockerSwitch
    Friend WithEvents RegSelSwitch03 As RockerSwitch
    Friend WithEvents RegSelSwitch02 As RockerSwitch
    Friend WithEvents RegSelSwitch01 As RockerSwitch

End Class

Option Strict On

Imports System
Imports System.ComponentModel
Imports System.Drawing.Text
Imports System.Net
Imports System.Net.Sockets
Imports System.Numerics
Imports System.Text
Imports System.Threading



Public Class FrmMain

    Private DemoIndex As Integer = 0

    Private WithEvents UdpSendClient As UdpClient
    'Private WithEvents RemoteSendEndPoint As IPEndPoint
    Private SendPort As Integer = 57831
    Private SendHostName As String = ""
    Private SendLastError As Integer = 0

    Private WithEvents UdpRecvClient As UdpClient
    Private WithEvents RemoteRecvEndPoint As IPEndPoint
    Private ListenPort As Integer = 57830

    Private CloseNetwork As Boolean = False
    Private UdpRecvUpdateCnt As UInteger = 0
    Private LastUdpRecvUpdateCnt As UInteger = 0
    Private NoComCounter As UInteger = 0
    Private ComIsGood As Boolean = False
    Private LocLastByteArray As Byte() = {255, 255, 255, 255, 255, 255, 255, 255, 255, 255}
    Private LocByteArray As Byte() = {255, 255, 255, 255, 255, 255, 255, 255, 255, 255}

    '--------local copies of data
    Private locRun As Boolean = False
    Private locSwitches As UInt16 = 0
    Private locRegDisplaySelect As UInt16 = 0
    Private locMemoryMode As UInt16 = 0
    Private locLock As Boolean = False

    '--------exit program -- from recv data
    Private locExit As Boolean = False

    Delegate Sub typProcSimData(ByRef loc_recv_bytes As Byte())
    Public MyDelProc As New typProcSimData(AddressOf ProcSimData)


    Private Sub FrmMain_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        Call UDP_Recv_Init()
        Call UDP_Send_Init()

    End Sub

    Private Sub FrmMain_Closed(sender As Object, e As EventArgs) Handles Me.Closed
        Debug.WriteLine("closed")

    End Sub

    Private Sub FrmMain_Closing(sender As Object, e As CancelEventArgs) Handles Me.Closing
        Debug.WriteLine("closing")

    End Sub

    Private Sub FrmMain_Disposed(sender As Object, e As EventArgs) Handles Me.Disposed
        UdpRecvClient.Close()
        UdpSendClient.Close()
        Debug.WriteLine("disposed")
    End Sub

    Private Sub FrmMain_FormClosed(sender As Object, e As FormClosedEventArgs) Handles Me.FormClosed
        Debug.WriteLine("form closed")

    End Sub

    Private Sub FrmMain_FormClosing(sender As Object, e As FormClosingEventArgs) Handles Me.FormClosing
        Debug.WriteLine("form closing")

    End Sub
    Private Sub ProcSimData(ByRef parm_recv_bytes As Byte())

        Const BRIGHT_ADDR_START As Integer = 10
        Const BRIGHT_DATA_START As Integer = BRIGHT_ADDR_START + 16
        Const BRIGHT_SWITCH_START As Integer = BRIGHT_DATA_START + 16
        Const BRIGHT_MISC3_START As Integer = BRIGHT_SWITCH_START + 16
        Const BRIGHT_MISC4_START As Integer = BRIGHT_MISC3_START + 16

        '--------Say we got an update...
        UdpRecvUpdateCnt += 1UI

        lblComBytes.Text = CStr(parm_recv_bytes.Length)
        '--------ensure we have some bytes and copy to local.
        If parm_recv_bytes.Length > 9 Then
            If parm_recv_bytes.Length <> LocByteArray.Length Then
                ReDim Preserve LocByteArray(parm_recv_bytes.Length - 1)
            End If
            If parm_recv_bytes.Length <> LocLastByteArray.Length Then
                ReDim Preserve LocLastByteArray(parm_recv_bytes.Length - 1)
            End If
            Buffer.BlockCopy(parm_recv_bytes, 0, LocByteArray, 0, parm_recv_bytes.Length)
        End If

        '--------old version, just on or off...
        If parm_recv_bytes.Length >= 9 And parm_recv_bytes.Length <= 11 Then

            '--------BYTES are backwards....

            '--------16 bit address
            If (LocByteArray(1) <> LocLastByteArray(1)) Then
                LED_Addr15.Brightness = If((LocByteArray(1) And &H80) <> 0, 100, 0)
                LED_Addr14.Brightness = If((LocByteArray(1) And &H40) <> 0, 100, 0)
                LED_Addr13.Brightness = If((LocByteArray(1) And &H20) <> 0, 100, 0)
                LED_Addr12.Brightness = If((LocByteArray(1) And &H10) <> 0, 100, 0)

                LED_Addr11.Brightness = If((LocByteArray(1) And &H8) <> 0, 100, 0)
                LED_Addr10.Brightness = If((LocByteArray(1) And &H4) <> 0, 100, 0)
                LED_Addr09.Brightness = If((LocByteArray(1) And &H2) <> 0, 100, 0)
                LED_Addr08.Brightness = If((LocByteArray(1) And &H1) <> 0, 100, 0)
                LocLastByteArray(1) = LocByteArray(1)
            End If


            If (LocByteArray(0) <> LocLastByteArray(0)) Then
                LED_Addr07.Brightness = If((LocByteArray(0) And &H80) <> 0, 100, 0)
                LED_Addr06.Brightness = If((LocByteArray(0) And &H40) <> 0, 100, 0)
                LED_Addr05.Brightness = If((LocByteArray(0) And &H20) <> 0, 100, 0)
                LED_Addr04.Brightness = If((LocByteArray(0) And &H10) <> 0, 100, 0)

                LED_Addr03.Brightness = If((LocByteArray(0) And &H8) <> 0, 100, 0)
                LED_Addr02.Brightness = If((LocByteArray(0) And &H4) <> 0, 100, 0)
                LED_Addr01.Brightness = If((LocByteArray(0) And &H2) <> 0, 100, 0)
                LED_Addr00.Brightness = If((LocByteArray(0) And &H1) <> 0, 100, 0)
                LocLastByteArray(0) = LocByteArray(0)
            End If


            '--------16 bit data
            If (LocByteArray(3) <> LocLastByteArray(3)) Then
                LED_Data15.Brightness = If((LocByteArray(3) And &H80) <> 0, 100, 0)
                LED_Data14.Brightness = If((LocByteArray(3) And &H40) <> 0, 100, 0)
                LED_Data13.Brightness = If((LocByteArray(3) And &H20) <> 0, 100, 0)
                LED_Data12.Brightness = If((LocByteArray(3) And &H10) <> 0, 100, 0)

                LED_Data11.Brightness = If((LocByteArray(3) And &H8) <> 0, 100, 0)
                LED_Data10.Brightness = If((LocByteArray(3) And &H4) <> 0, 100, 0)
                LED_Data09.Brightness = If((LocByteArray(3) And &H2) <> 0, 100, 0)
                LED_Data08.Brightness = If((LocByteArray(3) And &H1) <> 0, 100, 0)
                LocLastByteArray(3) = LocByteArray(3)
            End If

            If (LocByteArray(2) <> LocLastByteArray(2)) Then
                LED_Data07.Brightness = If((LocByteArray(2) And &H80) <> 0, 100, 0)
                LED_Data06.Brightness = If((LocByteArray(2) And &H40) <> 0, 100, 0)
                LED_Data05.Brightness = If((LocByteArray(2) And &H20) <> 0, 100, 0)
                LED_Data04.Brightness = If((LocByteArray(2) And &H10) <> 0, 100, 0)

                LED_Data03.Brightness = If((LocByteArray(2) And &H8) <> 0, 100, 0)
                LED_Data02.Brightness = If((LocByteArray(2) And &H4) <> 0, 100, 0)
                LED_Data01.Brightness = If((LocByteArray(2) And &H2) <> 0, 100, 0)
                LED_Data00.Brightness = If((LocByteArray(2) And &H1) <> 0, 100, 0)
                LocLastByteArray(2) = LocByteArray(2)
            End If

            '--------16 bit switch register.
            If (LocByteArray(5) <> LocLastByteArray(5)) Then
                If (LocByteArray(5) And &H80) <> 0 Then
                    LED_Switch15.Brightness = 100
                    locSwitches = locSwitches Or &H8000US
                Else
                    LED_Switch15.Brightness = 0
                    locSwitches = locSwitches And &H7FFFUS
                End If
                If (LocByteArray(5) And &H40) <> 0 Then
                    LED_Switch14.Brightness = 100
                    locSwitches = locSwitches Or &H4000US
                Else
                    LED_Switch14.Brightness = 0
                    locSwitches = locSwitches And &HBFFFUS
                End If
                If (LocByteArray(5) And &H20) <> 0 Then
                    LED_Switch13.Brightness = 100
                    locSwitches = locSwitches Or &H2000US
                Else
                    LED_Switch13.Brightness = 0
                    locSwitches = locSwitches And &HDFFFUS
                End If
                If (LocByteArray(5) And &H10) <> 0 Then
                    LED_Switch12.Brightness = 100
                    locSwitches = locSwitches Or &H1000US
                Else
                    LED_Switch12.Brightness = 0
                    locSwitches = locSwitches And &HEFFFUS
                End If

                If (LocByteArray(5) And &H8) <> 0 Then
                    LED_Switch11.Brightness = 100
                    locSwitches = locSwitches Or &H800US
                Else
                    LED_Switch11.Brightness = 0
                    locSwitches = locSwitches And &HF7FFUS
                End If
                If (LocByteArray(5) And &H4) <> 0 Then
                    LED_Switch10.Brightness = 100
                    locSwitches = locSwitches Or &H400US
                Else
                    LED_Switch10.Brightness = 0
                    locSwitches = locSwitches And &HFBFFUS
                End If
                If (LocByteArray(5) And &H2) <> 0 Then
                    LED_Switch09.Brightness = 100
                    locSwitches = locSwitches Or &H200US
                Else
                    LED_Switch09.Brightness = 0
                    locSwitches = locSwitches And &HFDFFUS
                End If
                If (LocByteArray(5) And &H1) <> 0 Then
                    LED_Switch08.Brightness = 100
                    locSwitches = locSwitches Or &H100US
                Else
                    LED_Switch08.Brightness = 0
                    locSwitches = locSwitches And &HFEFFUS
                End If
                LocLastByteArray(5) = LocByteArray(5)
            End If

            If (LocByteArray(4) <> LocLastByteArray(4)) Then
                If (LocByteArray(4) And &H80) <> 0 Then
                    LED_Switch07.Brightness = 100
                    locSwitches = locSwitches Or &H80US
                Else
                    LED_Switch07.Brightness = 0
                    locSwitches = locSwitches And &HFF7FUS
                End If
                If (LocByteArray(4) And &H40) <> 0 Then
                    LED_Switch06.Brightness = 100
                    locSwitches = locSwitches Or &H40US
                Else
                    LED_Switch06.Brightness = 0
                    locSwitches = locSwitches And &HFFBFUS
                End If
                If (LocByteArray(4) And &H20) <> 0 Then
                    LED_Switch05.Brightness = 100
                    locSwitches = locSwitches Or &H20US
                Else
                    LED_Switch05.Brightness = 0
                    locSwitches = locSwitches And &HFFDFUS
                End If
                If (LocByteArray(4) And &H10) <> 0 Then
                    LED_Switch04.Brightness = 100
                    locSwitches = locSwitches Or &H10US
                Else
                    LED_Switch04.Brightness = 0
                    locSwitches = locSwitches And &HFFEFUS
                End If

                If (LocByteArray(4) And &H8) <> 0 Then
                    LED_Switch03.Brightness = 100
                    locSwitches = locSwitches Or &H8US
                Else
                    LED_Switch03.Brightness = 0
                    locSwitches = locSwitches And &HFFF7US
                End If
                If (LocByteArray(4) And &H4) <> 0 Then
                    LED_Switch02.Brightness = 100
                    locSwitches = locSwitches Or &H4US
                Else
                    LED_Switch02.Brightness = 0
                    locSwitches = locSwitches And &HFFFBUS
                End If
                If (LocByteArray(4) And &H2) <> 0 Then
                    LED_Switch01.Brightness = 100
                    locSwitches = locSwitches Or &H2US
                Else
                    LED_Switch01.Brightness = 0
                    locSwitches = locSwitches And &HFFFDUS
                End If
                If (LocByteArray(4) And &H1) <> 0 Then
                    LED_Switch00.Brightness = 100
                    locSwitches = locSwitches Or &H1US
                Else
                    LED_Switch00.Brightness = 0
                    locSwitches = locSwitches And &HFFFEUS
                End If
                LocLastByteArray(4) = LocByteArray(4)
            End If

            '--------Misc
            If (LocByteArray(7) <> LocLastByteArray(7)) Then
                LED_CC_N.Brightness = If((LocByteArray(7) And &H80) <> 0, 100, 0)
                LED_CC_Z.Brightness = If((LocByteArray(7) And &H40) <> 0, 100, 0)
                LED_CC_O.Brightness = If((LocByteArray(7) And &H20) <> 0, 100, 0)
                LED_CC_C.Brightness = If((LocByteArray(7) And &H10) <> 0, 100, 0)

                LED_MemProt.Brightness = If((LocByteArray(7) And &H8) <> 0, 100, 0)
                LED_Priv.Brightness = If((LocByteArray(7) And &H4) <> 0, 100, 0)
                LED_PM.Brightness = If((LocByteArray(7) And &H2) <> 0, 100, 0)
                LED_Virt.Brightness = If((LocByteArray(7) And &H1) <> 0, 100, 0)
                LocLastByteArray(7) = LocByteArray(7)
            End If

            If (LocByteArray(6) <> LocLastByteArray(6)) Then
                LED_IoInt.Brightness = If((LocByteArray(6) And &H80) <> 0, 100, 0)
                LED_TaskInt.Brightness = If((LocByteArray(6) And &H40) <> 0, 100, 0)
                LED_MemErr.Brightness = If((LocByteArray(6) And &H20) <> 0, 100, 0)
                '--LED_EMA04.Brightness = If((locByteArray(6) And &H10) <> 0, 100, 0) '-- reserved for 7860

                LED_EMA03.Brightness = If((LocByteArray(6) And &H8) <> 0, 100, 0)
                LED_EMA02.Brightness = If((LocByteArray(6) And &H4) <> 0, 100, 0)
                LED_EMA01.Brightness = If((LocByteArray(6) And &H2) <> 0, 100, 0)
                LED_EMA00.Brightness = If((LocByteArray(6) And &H1) <> 0, 100, 0)
                LocLastByteArray(6) = LocByteArray(6)
            End If

            If (LocByteArray(9) <> LocLastByteArray(9)) Then
                LED_Power.Brightness = If((LocByteArray(9) And &H80) <> 0, 100, 0)
                LED_Standby.Brightness = If((LocByteArray(9) And &H40) <> 0, 100, 0)
                LED_BackupFailure.Brightness = If((LocByteArray(9) And &H20) <> 0, 100, 0)
                '--------update LED and switch
                If (LocByteArray(9) And &H10) <> 0 Then
                    LED_Run.Brightness = 100
                    RunHaltSwitch.IsOn = True
                    locRun = True
                Else
                    LED_Run.Brightness = 0
                    RunHaltSwitch.IsOn = False
                    locRun = False
                End If

                '--unused--LED_xx.Brightness = If((locByteArray(9) And &H8) <> 0, 100, 0)
                '--unused--LED_xx.Brightness = If((locByteArray(9) And &H4) <> 0, 100, 0)
                '--unused--LED_xx.Brightness = If((locByteArray(9) And &H2) <> 0, 100, 0)
                '--------exit command....
                If (LocByteArray(9) And &H1) <> 0 Then
                    System.Windows.Forms.Application.Exit()
                End If
                LocLastByteArray(9) = LocByteArray(9)
            End If

            '--------nothing defined in byte 8 yet.



            '--------new version, has brightness...
        ElseIf parm_recv_bytes.Length >= 90 Then

            '--------BYTES are backwards....

            '--------16 bit address
            LED_Addr15.Brightness = CInt(LocByteArray(BRIGHT_ADDR_START + 1))
            LED_Addr14.Brightness = CInt(LocByteArray(BRIGHT_ADDR_START + 0))
            LED_Addr13.Brightness = CInt(LocByteArray(BRIGHT_ADDR_START + 3))
            LED_Addr12.Brightness = CInt(LocByteArray(BRIGHT_ADDR_START + 2))

            LED_Addr11.Brightness = CInt(LocByteArray(BRIGHT_ADDR_START + 5))
            LED_Addr10.Brightness = CInt(LocByteArray(BRIGHT_ADDR_START + 4))
            LED_Addr09.Brightness = CInt(LocByteArray(BRIGHT_ADDR_START + 7))
            LED_Addr08.Brightness = CInt(LocByteArray(BRIGHT_ADDR_START + 6))


            LED_Addr07.Brightness = CInt(LocByteArray(BRIGHT_ADDR_START + 9))
            LED_Addr06.Brightness = CInt(LocByteArray(BRIGHT_ADDR_START + 8))
            LED_Addr05.Brightness = CInt(LocByteArray(BRIGHT_ADDR_START + 11))
            LED_Addr04.Brightness = CInt(LocByteArray(BRIGHT_ADDR_START + 10))

            LED_Addr03.Brightness = CInt(LocByteArray(BRIGHT_ADDR_START + 13))
            LED_Addr02.Brightness = CInt(LocByteArray(BRIGHT_ADDR_START + 12))
            LED_Addr01.Brightness = CInt(LocByteArray(BRIGHT_ADDR_START + 15))
            LED_Addr00.Brightness = CInt(LocByteArray(BRIGHT_ADDR_START + 14))

            '--------debug
            '--LED_Data15.Brightness = 0
            '--LED_Data14.Brightness = 10
            '--LED_Data13.Brightness = 20
            '--LED_Data12.Brightness = 30

            '--LED_Data11.Brightness = 40
            '--LED_Data10.Brightness = 50
            '--LED_Data09.Brightness = 60
            '--LED_Data08.Brightness = 70

            '--LED_Data07.Brightness = 80
            '--LED_Data06.Brightness = 90
            '--LED_Data05.Brightness = 100
            '--LED_Data04.Brightness = 110

            '--LED_Data03.Brightness = 95
            '--LED_Data02.Brightness = 85
            '--LED_Data01.Brightness = 75
            '--LED_Data00.Brightness = 65

            '--------16 bit data
            LED_Data15.Brightness = CInt(LocByteArray(BRIGHT_DATA_START + 1))
            LED_Data14.Brightness = CInt(LocByteArray(BRIGHT_DATA_START + 0))
            LED_Data13.Brightness = CInt(LocByteArray(BRIGHT_DATA_START + 3))
            LED_Data12.Brightness = CInt(LocByteArray(BRIGHT_DATA_START + 2))

            LED_Data11.Brightness = CInt(LocByteArray(BRIGHT_DATA_START + 5))
            LED_Data10.Brightness = CInt(LocByteArray(BRIGHT_DATA_START + 4))
            LED_Data09.Brightness = CInt(LocByteArray(BRIGHT_DATA_START + 7))
            LED_Data08.Brightness = CInt(LocByteArray(BRIGHT_DATA_START + 6))


            LED_Data07.Brightness = CInt(LocByteArray(BRIGHT_DATA_START + 9))
            LED_Data06.Brightness = CInt(LocByteArray(BRIGHT_DATA_START + 8))
            LED_Data05.Brightness = CInt(LocByteArray(BRIGHT_DATA_START + 11))
            LED_Data04.Brightness = CInt(LocByteArray(BRIGHT_DATA_START + 10))

            LED_Data03.Brightness = CInt(LocByteArray(BRIGHT_DATA_START + 13))
            LED_Data02.Brightness = CInt(LocByteArray(BRIGHT_DATA_START + 12))
            LED_Data01.Brightness = CInt(LocByteArray(BRIGHT_DATA_START + 15))
            LED_Data00.Brightness = CInt(LocByteArray(BRIGHT_DATA_START + 14))

            '--------16 bit switch register.   It doesn't change much so just use off / on
            If (LocByteArray(5) <> LocLastByteArray(5)) Then
                If (LocByteArray(5) And &H80) <> 0 Then
                    LED_Switch15.Brightness = 100
                    locSwitches = locSwitches Or &H8000US
                Else
                    LED_Switch15.Brightness = 0
                    locSwitches = locSwitches And &H7FFFUS
                End If
                If (LocByteArray(5) And &H40) <> 0 Then
                    LED_Switch14.Brightness = 100
                    locSwitches = locSwitches Or &H4000US
                Else
                    LED_Switch14.Brightness = 0
                    locSwitches = locSwitches And &HBFFFUS
                End If
                If (LocByteArray(5) And &H20) <> 0 Then
                    LED_Switch13.Brightness = 100
                    locSwitches = locSwitches Or &H2000US
                Else
                    LED_Switch13.Brightness = 0
                    locSwitches = locSwitches And &HDFFFUS
                End If
                If (LocByteArray(5) And &H10) <> 0 Then
                    LED_Switch12.Brightness = 100
                    locSwitches = locSwitches Or &H1000US
                Else
                    LED_Switch12.Brightness = 0
                    locSwitches = locSwitches And &HEFFFUS
                End If

                If (LocByteArray(5) And &H8) <> 0 Then
                    LED_Switch11.Brightness = 100
                    locSwitches = locSwitches Or &H800US
                Else
                    LED_Switch11.Brightness = 0
                    locSwitches = locSwitches And &HF7FFUS
                End If
                If (LocByteArray(5) And &H4) <> 0 Then
                    LED_Switch10.Brightness = 100
                    locSwitches = locSwitches Or &H400US
                Else
                    LED_Switch10.Brightness = 0
                    locSwitches = locSwitches And &HFBFFUS
                End If
                If (LocByteArray(5) And &H2) <> 0 Then
                    LED_Switch09.Brightness = 100
                    locSwitches = locSwitches Or &H200US
                Else
                    LED_Switch09.Brightness = 0
                    locSwitches = locSwitches And &HFDFFUS
                End If
                If (LocByteArray(5) And &H1) <> 0 Then
                    LED_Switch08.Brightness = 100
                    locSwitches = locSwitches Or &H100US
                Else
                    LED_Switch08.Brightness = 0
                    locSwitches = locSwitches And &HFEFFUS
                End If
                LocLastByteArray(5) = LocByteArray(5)
            End If

            If (LocByteArray(4) <> LocLastByteArray(4)) Then
                If (LocByteArray(4) And &H80) <> 0 Then
                    LED_Switch07.Brightness = 100
                    locSwitches = locSwitches Or &H80US
                Else
                    LED_Switch07.Brightness = 0
                    locSwitches = locSwitches And &HFF7FUS
                End If
                If (LocByteArray(4) And &H40) <> 0 Then
                    LED_Switch06.Brightness = 100
                    locSwitches = locSwitches Or &H40US
                Else
                    LED_Switch06.Brightness = 0
                    locSwitches = locSwitches And &HFFBFUS
                End If
                If (LocByteArray(4) And &H20) <> 0 Then
                    LED_Switch05.Brightness = 100
                    locSwitches = locSwitches Or &H20US
                Else
                    LED_Switch05.Brightness = 0
                    locSwitches = locSwitches And &HFFDFUS
                End If
                If (LocByteArray(4) And &H10) <> 0 Then
                    LED_Switch04.Brightness = 100
                    locSwitches = locSwitches Or &H10US
                Else
                    LED_Switch04.Brightness = 0
                    locSwitches = locSwitches And &HFFEFUS
                End If

                If (LocByteArray(4) And &H8) <> 0 Then
                    LED_Switch03.Brightness = 100
                    locSwitches = locSwitches Or &H8US
                Else
                    LED_Switch03.Brightness = 0
                    locSwitches = locSwitches And &HFFF7US
                End If
                If (LocByteArray(4) And &H4) <> 0 Then
                    LED_Switch02.Brightness = 100
                    locSwitches = locSwitches Or &H4US
                Else
                    LED_Switch02.Brightness = 0
                    locSwitches = locSwitches And &HFFFBUS
                End If
                If (LocByteArray(4) And &H2) <> 0 Then
                    LED_Switch01.Brightness = 100
                    locSwitches = locSwitches Or &H2US
                Else
                    LED_Switch01.Brightness = 0
                    locSwitches = locSwitches And &HFFFDUS
                End If
                If (LocByteArray(4) And &H1) <> 0 Then
                    LED_Switch00.Brightness = 100
                    locSwitches = locSwitches Or &H1US
                Else
                    LED_Switch00.Brightness = 0
                    locSwitches = locSwitches And &HFFFEUS
                End If
                LocLastByteArray(4) = LocByteArray(4)
            End If

            '--------Misc 3
            LED_CC_N.Brightness = CInt(LocByteArray(BRIGHT_MISC3_START + 1)) '--If((locByteArray(7) And &H80) <> 0, 100, 0)
            LED_CC_Z.Brightness = CInt(LocByteArray(BRIGHT_MISC3_START + 0)) '--If((locByteArray(7) And &H40) <> 0, 100, 0)
            LED_CC_O.Brightness = CInt(LocByteArray(BRIGHT_MISC3_START + 3)) '--If((locByteArray(7) And &H20) <> 0, 100, 0)
            LED_CC_C.Brightness = CInt(LocByteArray(BRIGHT_MISC3_START + 2)) '--If((locByteArray(7) And &H10) <> 0, 100, 0)

            LED_MemProt.Brightness = CInt(LocByteArray(BRIGHT_MISC3_START + 5)) '--If((locByteArray(7) And &H8) <> 0, 100, 0)
            LED_Priv.Brightness = CInt(LocByteArray(BRIGHT_MISC3_START + 4)) '--If((locByteArray(7) And &H4) <> 0, 100, 0)
            LED_PM.Brightness = CInt(LocByteArray(BRIGHT_MISC3_START + 7)) '--If((locByteArray(7) And &H2) <> 0, 100, 0)
            LED_Virt.Brightness = CInt(LocByteArray(BRIGHT_MISC3_START + 6)) '--If((locByteArray(7) And &H1) <> 0, 100, 0)

            LED_IoInt.Brightness = CInt(LocByteArray(BRIGHT_MISC3_START + 9)) '--If((locByteArray(6) And &H80) <> 0, 100, 0)
            LED_TaskInt.Brightness = CInt(LocByteArray(BRIGHT_MISC3_START + 8)) '--If((locByteArray(6) And &H40) <> 0, 100, 0)
            LED_MemErr.Brightness = CInt(LocByteArray(BRIGHT_MISC3_START + 11)) '--If((locByteArray(6) And &H20) <> 0, 100, 0)
            '--LED_EMA04.Brightness = CInt(locByteArray(BRIGHT_MISC3_START + 10)) '--If((locByteArray(6) And &H10) <> 0, 100, 0) '-- reserved for 7860

            LED_EMA03.Brightness = CInt(LocByteArray(BRIGHT_MISC3_START + 13)) '--If((locByteArray(6) And &H8) <> 0, 100, 0)
            LED_EMA02.Brightness = CInt(LocByteArray(BRIGHT_MISC3_START + 12)) '--If((locByteArray(6) And &H4) <> 0, 100, 0)
            LED_EMA01.Brightness = CInt(LocByteArray(BRIGHT_MISC3_START + 15)) '--If((locByteArray(6) And &H2) <> 0, 100, 0)
            LED_EMA00.Brightness = CInt(LocByteArray(BRIGHT_MISC3_START + 14)) '--If((locByteArray(6) And &H1) <> 0, 100, 0)

            ' --------MISC4.   These values don't change quickly.   Just use on/off.
            If (LocByteArray(9) <> LocLastByteArray(9)) Then
                LED_Power.Brightness = If((LocByteArray(9) And &H80) <> 0, 100, 0)
                LED_Standby.Brightness = If((LocByteArray(9) And &H40) <> 0, 100, 0)
                LED_BackupFailure.Brightness = If((LocByteArray(9) And &H20) <> 0, 100, 0)
                '--------update led and switch
                If (LocByteArray(9) And &H10) <> 0 Then
                    LED_Run.Brightness = 100
                    RunHaltSwitch.IsOn = True
                    locRun = True
                Else
                    LED_Run.Brightness = 0
                    RunHaltSwitch.IsOn = False
                    locRun = False
                End If

                '--unused--LED_xx.Brightness = If((locByteArray(9) And &H8) <> 0, 100, 0)
                '--unused--LED_xx.Brightness = If((locByteArray(9) And &H4) <> 0, 100, 0)
                '--unused--LED_xx.Brightness = If((locByteArray(9) And &H2) <> 0, 100, 0)
                '--unused--LED_xx.Brightness = If((locByteArray(9) And &H1) <> 0, 100, 0)
                '--------exit command....
                If (LocByteArray(9) And &H1) <> 0 Then
                    System.Windows.Forms.Application.Exit()
                End If
            End If

            '--------nothing defined in byte 8 yet.

        End If


    End Sub

    '--------general timer -- 50 milliseconds, check com status.
    Private Sub Timer1_Tick(sender As Object, e As EventArgs) Handles Timer1.Tick

        '--------did we get any UDP updates
        ' --------no updates received.
        If LastUdpRecvUpdateCnt = UdpRecvUpdateCnt Then

            '--------no updates recieved, inc counter (2 seconds)
            If NoComCounter < 40 Then
                NoComCounter += 1UI
            End If
            If NoComCounter = 40 Then
                NoComCounter = 100
                ComIsGood = False
                lblComStatus.Text = "No Communications"
            End If

            '-------- updates received.
        Else
            '--------save last known value for comparision...
            LastUdpRecvUpdateCnt = UdpRecvUpdateCnt
            '--------zero the no communications counter.
            NoComCounter = 0
            '--------set com is good.
            If Not ComIsGood Then
                ComIsGood = True
                lblComStatus.Text = "Communications Good"
            End If
        End If

        '--------update switch value for debug
        SwitchValue.Text = locSwitches.ToString("x4")

        '--------update register display select for debug
        RegDisplayValue.Text = locRegDisplaySelect.ToString("x3")


        Return

    End Sub

    Private Sub MenuStrip1_ItemClicked(sender As Object, e As ToolStripItemClickedEventArgs) Handles MenuStrip1.ItemClicked

    End Sub


    Private Sub UDP_Recv_Thread()

        '--Debug.Print(" UDP_Recv_Thread started.")

        Do While Not CloseNetwork

            Try
                Dim UdpRecvBytes As Byte()
                '---Debug.Print("starting udp read")
                UdpRecvBytes = UdpRecvClient.Receive(RemoteRecvEndPoint)
                '---Dim receivedData As String = Encoding.ASCII.GetString(receivedBytes)
                '---------Process the received data (e.g., display in a TextBox)
                '---------Example:  Me.Invoke(Sub() TextBox1.AppendText($"Received from {remoteIpEndPoint.Address}: {receivedData}{Environment.NewLine}"))
                '--Debug.Print("recieved " & UdpRecvBytes.Length.ToString & " bytes")
                '--Debug.Print("calling delegate to process")
                Me.Invoke(Sub() MyDelProc(UdpRecvBytes))

            Catch ex As Exception
                ' Handle exceptions (e.g., if the client is closed)
                If Not CloseNetwork Then
                    ' Log or display the error if it's not due to intentional closure
                    ' Example: Me.Invoke(Sub() TextBox1.AppendText($"Error: {ex.Message}{Environment.NewLine}"))
                End If
                '--Debug.Print("Udp recv err" & Err.ToString)
            End Try
        Loop
    End Sub

    Private Sub UDP_Recv_Init()
        '--------Open UDP recieve client.  Listen on any address..
        UdpRecvClient = New UdpClient(ListenPort)

        '--------receive from any
        RemoteRecvEndPoint = New IPEndPoint(IPAddress.Any, 0)

        '--------Start a new thread for receiving to prevent UI blocking
        Dim receiveThread As New Threading.Thread(AddressOf UDP_Recv_Thread)
        receiveThread.IsBackground = True ' Allow the application to exit even if the thread is running
        receiveThread.Start()

    End Sub

    Private Sub UDP_Send_Init()
        '--------Open UDP send client.  Send to local address...
        UdpSendClient = New UdpClient() ' SendPort, AddressFamily.InterNetwork)

        '--------Get the host name of the current machine
        Dim hostName As String = Dns.GetHostName()
        Debug.WriteLine(" host name " & hostName)
        SendHostName = hostName

        ' Retrieve all IP addresses associated with the host
        Dim ipAddresses = Dns.GetHostEntry(hostName).AddressList

        Dim useIp As New IPAddress(0)
        Dim done As Boolean = False
        '--------Loop through and find the IPv4 address
        For Each ip As IPAddress In ipAddresses
            If ip.AddressFamily = Sockets.AddressFamily.InterNetwork Then
                Debug.WriteLine("IPv4 Address: " & ip.ToString())
                If Not done Then
                    useIp = ip
                    done = True
                End If
            End If
        Next

        'RemoteSendEndPoint = New IPEndPoint(useIp, SendPort)

        '--------Start a new thread for sending to prevent UI blocking
        '--Dim sendThread As New Threading.Thread(AddressOf UDP_Send_Thread)
        '--sendThread.IsBackground = True ' Allow the application to exit even if the thread is running
        '--sendThread.Start()

    End Sub

    Private Sub AboutToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles AboutToolStripMenuItem.Click
        frmAboutBox.ShowDialog()
    End Sub

    Private Sub ExitToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles ExitToolStripMenuItem.Click
        frmSettings.ShowDialog()
    End Sub

    Private Sub RegSelSwitch01_MouseClick(sender As Object, e As MouseEventArgs) Handles RegSelSwitch01.MouseClick
        Debug.WriteLine("got mouseclick")
        RegSelSwitch01.Toggle()
        '--------update switch value
        If RegSelSwitch01.IsOn Then
            locRegDisplaySelect = locRegDisplaySelect Or &H1US
        Else
            locRegDisplaySelect = locRegDisplaySelect And &HFFFEUS
        End If
        '--------send new value to simulator..
        Call SendCmdRegDispSel()
    End Sub

    Private Sub RegSelSwitch02_MouseClick(sender As Object, e As MouseEventArgs) Handles RegSelSwitch02.MouseClick
        Debug.WriteLine("got mouseclick")
        RegSelSwitch02.Toggle()
        '--------update switch value
        If RegSelSwitch02.IsOn Then
            locRegDisplaySelect = locRegDisplaySelect Or &H2US
        Else
            locRegDisplaySelect = locRegDisplaySelect And &HFFFDUS
        End If
        '--------send new value to simulator..
        Call SendCmdRegDispSel()
    End Sub

    Private Sub RegSelSwitch03_MouseClick(sender As Object, e As MouseEventArgs) Handles RegSelSwitch03.MouseClick
        Debug.WriteLine("got mouseclick")
        RegSelSwitch03.Toggle()
        '--------update switch value
        If RegSelSwitch03.IsOn Then
            locRegDisplaySelect = locRegDisplaySelect Or &H4US
        Else
            locRegDisplaySelect = locRegDisplaySelect And &HFFFBUS
        End If
        '--------send new value to simulator..
        Call SendCmdRegDispSel()
    End Sub

    Private Sub RegSelSwitch04_MouseClick(sender As Object, e As MouseEventArgs) Handles RegSelSwitch04.MouseClick
        Debug.WriteLine("got mouseclick")
        RegSelSwitch04.Toggle()
        '--------update switch value
        If RegSelSwitch04.IsOn Then
            locRegDisplaySelect = locRegDisplaySelect Or &H8US
        Else
            locRegDisplaySelect = locRegDisplaySelect And &HFFF7US
        End If
        '--------send new value to simulator..
        Call SendCmdRegDispSel()
    End Sub

    Private Sub RegSelSwitch05_MouseClick(sender As Object, e As MouseEventArgs) Handles RegSelSwitch05.MouseClick
        Debug.WriteLine("got mouseclick")
        RegSelSwitch05.Toggle()
        '--------update switch value
        If RegSelSwitch05.IsOn Then
            locRegDisplaySelect = locRegDisplaySelect Or &H10US
        Else
            locRegDisplaySelect = locRegDisplaySelect And &HFFEFUS
        End If
        '--------send new value to simulator..
        Call SendCmdRegDispSel()
    End Sub

    Private Sub RegSelSwitch06_MouseClick(sender As Object, e As MouseEventArgs) Handles RegSelSwitch06.MouseClick
        Debug.WriteLine("got mouseclick")
        RegSelSwitch06.Toggle()
        '--------update switch value
        If RegSelSwitch06.IsOn Then
            locRegDisplaySelect = locRegDisplaySelect Or &H20US
        Else
            locRegDisplaySelect = locRegDisplaySelect And &HFFDFUS
        End If
        '--------send new value to simulator..
        Call SendCmdRegDispSel()
    End Sub

    Private Sub RegSelSwitch07_MouseClick(sender As Object, e As MouseEventArgs) Handles RegSelSwitch07.MouseClick
        Debug.WriteLine("got mouseclick")
        RegSelSwitch07.Toggle()
        '--------update switch value
        If RegSelSwitch07.IsOn Then
            locRegDisplaySelect = locRegDisplaySelect Or &H40US
        Else
            locRegDisplaySelect = locRegDisplaySelect And &HFFBFUS
        End If
        '--------send new value to simulator..
        Call SendCmdRegDispSel()
    End Sub

    Private Sub RegSelSwitch08_MouseClick(sender As Object, e As MouseEventArgs) Handles RegSelSwitch08.MouseClick
        Debug.WriteLine("got mouseclick")
        RegSelSwitch08.Toggle()
        '--------update switch value
        If RegSelSwitch08.IsOn Then
            locRegDisplaySelect = locRegDisplaySelect Or &H80US
        Else
            locRegDisplaySelect = locRegDisplaySelect And &HFF7FUS
        End If
        '--------send new value to simulator..
        Call SendCmdRegDispSel()
    End Sub


    Private Sub RegSelSwitch09_MouseClick(sender As Object, e As MouseEventArgs) Handles RegSelSwitch09.MouseClick
        Debug.WriteLine("got mouseclick")
        RegSelSwitch09.Toggle()
        '--------update switch value
        If RegSelSwitch09.IsOn Then
            locRegDisplaySelect = locRegDisplaySelect Or &H100US
        Else
            locRegDisplaySelect = locRegDisplaySelect And &HFEFFUS
        End If
        '--------send new value to simulator..
        Call SendCmdRegDispSel()
    End Sub

    Private Sub RegSelSwitch10_MouseClick(sender As Object, e As MouseEventArgs) Handles RegSelSwitch10.MouseClick
        Debug.WriteLine("got mouseclick")
        RegSelSwitch10.Toggle()
        '--------update switch value
        If RegSelSwitch10.IsOn Then
            locRegDisplaySelect = locRegDisplaySelect Or &H200US
        Else
            locRegDisplaySelect = locRegDisplaySelect And &HFDFFUS
        End If
        '--------send new value to simulator..
        Call SendCmdRegDispSel()
    End Sub

    Private Sub RegSelSwitch11_MouseClick(sender As Object, e As MouseEventArgs) Handles RegSelSwitch11.MouseClick
        Debug.WriteLine("got mouseclick")
        RegSelSwitch11.Toggle()
        '--------update switch value
        If RegSelSwitch11.IsOn Then
            locRegDisplaySelect = locRegDisplaySelect Or &H400US
        Else
            locRegDisplaySelect = locRegDisplaySelect And &HFBFFUS
        End If
        '--------send new value to simulator..
        Call SendCmdRegDispSel()
    End Sub

    Private Sub RegSelSwitch12_MouseClick(sender As Object, e As MouseEventArgs) Handles RegSelSwitch12.MouseClick
        Debug.WriteLine("got mouseclick")
        RegSelSwitch12.Toggle()
        '--------update switch value
        If RegSelSwitch12.IsOn Then
            locRegDisplaySelect = locRegDisplaySelect Or &H800US
        Else
            locRegDisplaySelect = locRegDisplaySelect And &HF7FFUS
        End If
        '--------send new value to simulator..
        Call SendCmdRegDispSel()
    End Sub

    Private Sub SingleStepSwitch_MouseClick(sender As Object, e As MouseEventArgs) Handles SingleStepSwitch.MouseClick
        Debug.WriteLine("got mouseclick - single step")
        If Not locLock And Not locRun Then
            SingleStepSwitch.IsOn = True
            SingleStepSwitch.Refresh()
            Call SendCmdSingleStep()
            Thread.Sleep(200)
            SingleStepSwitch.IsOn = False
            SingleStepSwitch.Refresh()
        End If
    End Sub

    Private Sub RunHaltSwitch_MouseClick(sender As Object, e As MouseEventArgs) Handles RunHaltSwitch.MouseClick
        Debug.WriteLine("got mouseclick - run/halt")
        If Not locLock Then
            RunHaltSwitch.Toggle()
            If RunHaltSwitch.IsOn Then
                Call SendCmdRun()
            Else
                Call SendCmdHalt()
            End If
        End If
    End Sub

    Private Sub FillSwitch_MouseClick(sender As Object, e As MouseEventArgs) Handles FillSwitch.MouseClick
        Debug.WriteLine("got mouseclick - fill")
        If Not locLock Then
            FillSwitch.IsOn = True
            FillSwitch.Refresh()
            Call SendCmdFill()
            Thread.Sleep(200)
            FillSwitch.IsOn = False
            FillSwitch.Refresh()
        End If
    End Sub

    Private Sub MasterClearSwitch_MouseClick(sender As Object, e As MouseEventArgs) Handles MasterClearSwitch.MouseClick
        Debug.WriteLine("got mouseclick - master clear")
        If Not locLock Then
            MasterClearSwitch.IsOn = True
            MasterClearSwitch.Refresh()
            Call SendCmdMc()
            Thread.Sleep(200)
            MasterClearSwitch.IsOn = False
            MasterClearSwitch.Refresh()
        End If
    End Sub

    Private Sub EntRegCslIntSwitch_MouseClick(sender As Object, e As MouseEventArgs) Handles EntRegCslIntSwitch.MouseClick
        Debug.WriteLine("got mouseclick - console int")
        If locRun Or (Not locRun And Not locLock) Then
            EntRegCslIntSwitch.IsOn = True
            EntRegCslIntSwitch.Refresh()
            If locRun Then
                Call SendCmdCI()
            Else
                If Not locLock Then
                    Call SendCmdEntReg()
                End If
            End If
            Thread.Sleep(200)
            EntRegCslIntSwitch.IsOn = False
            EntRegCslIntSwitch.Refresh()
        End If
    End Sub

    Private Sub ClearBpHaltSwitch_MouseClick(sender As Object, e As MouseEventArgs) Handles ClearBpHaltSwitch.MouseClick
        Debug.WriteLine("got mouseclick - ClearBpHalt")
        ClearBpHaltSwitch.IsOn = True
        ClearBpHaltSwitch.Refresh()
        Call SendCmdClrBp()
        Thread.Sleep(200)
        ClearBpHaltSwitch.IsOn = False
        ClearBpHaltSwitch.Refresh()

    End Sub

    Private Sub InstOperSwitch_MouseClick(sender As Object, e As MouseEventArgs) Handles InstOperSwitch.MouseClick
        Debug.WriteLine("got mouseclick - inst/oper")
        InstOperSwitch.Toggle()
        If InstOperSwitch.IsOn Then
            locMemoryMode = locMemoryMode Or 1US
        Else
            locMemoryMode = locMemoryMode And &HFFFEUS
        End If
        Call SendCmdSetMemMode()
    End Sub

    Private Sub VirtualActualSwitch_MouseClick(sender As Object, e As MouseEventArgs) Handles VirtualActualSwitch.MouseClick
        Debug.WriteLine("got mouseclick - virt/act")
        VirtualActualSwitch.Toggle()
        If VirtualActualSwitch.IsOn Then
            locMemoryMode = locMemoryMode Or 2US
        Else
            locMemoryMode = locMemoryMode And &HFFFDUS
        End If
        Call SendCmdSetMemMode()
    End Sub

    ''' <summary>
    ''' Send NOOP command to modcomp simulator. Cmd=0
    ''' </summary>
    Private Sub SendCmdNoop()
        Dim msg(2) As UInt16
        msg(0) = 0
        msg(1) = 0
        SendCmdMessage(msg)
    End Sub

    ''' <summary>
    ''' If unlocked, send Master Clear command to modcomp simulator.  Cmd=1
    ''' </summary>
    Private Sub SendCmdMc()
        Dim msg(2) As UInt16
        If Not locLock Then
            msg(0) = 1
            msg(1) = 0
            SendCmdMessage(msg)
        End If
    End Sub

    ''' <summary>
    ''' If unlocked, send Fill command to modcomp simulator. Cmd=2
    ''' Data is current switch settings.
    ''' </summary>
    Private Sub SendCmdFill()
        Dim msg(2) As UInt16
        If Not locLock Then
            msg(0) = 2
            msg(1) = locSwitches
            SendCmdMessage(msg)
        End If
    End Sub

    ''' <summary>
    ''' If unlocked, send Run command to modcomp simulator. Cmd=3
    ''' </summary>
    Private Sub SendCmdRun()
        Dim msg(2) As UInt16
        If Not locLock Then
            msg(0) = 3
            msg(1) = 0
            SendCmdMessage(msg)
        End If
    End Sub

    ''' <summary>
    ''' If unlocked, send command to halt cpu.  Cmd=4
    ''' </summary>
    Private Sub SendCmdHalt()
        Dim msg(2) As UInt16
        If Not locLock Then
            msg(0) = 4
            msg(1) = 0
            SendCmdMessage(msg)
        End If
    End Sub

    ''' <summary>
    ''' 
    ''' </summary>
    Private Sub SendCmdSetSwitches()
        Dim msg(2) As UInt16
        msg(0) = 5
        msg(1) = locSwitches
        SendCmdMessage(msg)
    End Sub

    ''' <summary>
    ''' 
    ''' </summary>
    Private Sub SendCmdRegDispSel()
        Dim msg(2) As UInt16
        msg(0) = 6
        msg(1) = locRegDisplaySelect
        SendCmdMessage(msg)
    End Sub

    ''' <summary>
    ''' 
    ''' </summary>
    Private Sub SendCmdSetMemMode()
        Dim msg(2) As UInt16
        msg(0) = 7
        msg(1) = locMemoryMode
        SendCmdMessage(msg)
    End Sub

    ''' <summary>
    ''' 
    ''' </summary>
    Private Sub SendCmdClrBp()
        Dim msg(2) As UInt16
        msg(0) = 8
        msg(1) = 0
        SendCmdMessage(msg)
    End Sub

    ''' <summary>
    ''' 
    ''' </summary>
    Private Sub SendCmdCI()
        Dim msg(2) As UInt16
        msg(0) = 9
        msg(1) = 0
        SendCmdMessage(msg)
    End Sub

    ''' <summary>
    ''' 
    ''' </summary>
    Private Sub SendCmdEntReg()
        Dim msg(2) As UInt16
        If Not locLock And Not locRun Then
            '--------just to be certain
            Call SendCmdSetSwitches()
            Call SendCmdRegDispSel()
            msg(0) = 10
            msg(1) = locSwitches
            SendCmdMessage(msg)
        End If
    End Sub

    ''' <summary>
    ''' 
    ''' </summary>
    Private Sub SendCmdEntNext()
        Dim msg(2) As UInt16
        If Not locLock And Not locRun Then
            '--------just to be certain
            Call SendCmdSetSwitches()
            msg(0) = 11
            msg(1) = locSwitches
            SendCmdMessage(msg)
        End If
    End Sub

    ''' <summary>
    ''' 
    ''' </summary>
    Private Sub SendCmdEntMem()
        Dim msg(2) As UInt16
        If Not locLock And Not locRun Then
            '--------just to be certain
            Call SendCmdSetSwitches()
            msg(0) = 12
            msg(1) = locSwitches
            SendCmdMessage(msg)
        End If
    End Sub

    ''' <summary>
    ''' 
    ''' </summary>
    Private Sub SendCmdEntPC()
        Dim msg(2) As UInt16
        If Not locLock And Not locRun Then
            '--------just to be certain
            Call SendCmdSetSwitches()
            msg(0) = 13
            msg(1) = locSwitches
            SendCmdMessage(msg)
        End If
    End Sub

    ''' <summary>
    ''' 
    ''' </summary>
    Private Sub SendCmdNextPc()
        Dim msg(2) As UInt16
        If Not locLock And Not locRun Then
            msg(0) = 14
            msg(1) = 0
            SendCmdMessage(msg)
        End If
    End Sub

    ''' <summary>
    ''' 
    ''' </summary>
    Private Sub SendCmdSingleStep()
        Dim msg(2) As UInt16
        If Not locLock And Not locRun Then
            msg(0) = 15
            msg(1) = 0
            SendCmdMessage(msg)
        End If
    End Sub

    ''' <summary>
    ''' Send a command message to the modcomp simulator...  
    ''' </summary>
    Private Sub SendCmdMessage(msg As UInt16())
        Dim byteMsg(4) As Byte
        Dim bytesSent As Integer
        Buffer.BlockCopy(msg, 0, byteMsg, 0, 4)
        ' bytesSent = UdpSendClient.Send(byteMsg, RemoteSendEndPoint)
        bytesSent = UdpSendClient.Send(byteMsg, SendHostName, SendPort)
        If bytesSent = 4 Then
            SendLastError = 0
        Else
            SendLastError = 1
        End If

    End Sub

End Class


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

    '--------bit 15 is high bit....
    Private Const WordClearBit15 As UInt16 = &H7FFFUS
    Private Const WordClearBit14 As UInt16 = &HBFFFUS
    Private Const WordClearBit13 As UInt16 = &HDFFFUS
    Private Const WordClearBit12 As UInt16 = &HEFFFUS
    Private Const WordClearBit11 As UInt16 = &HF7FFUS
    Private Const WordClearBit10 As UInt16 = &HFBFFUS
    Private Const WordClearBit09 As UInt16 = &HFDFFUS
    Private Const WordClearBit08 As UInt16 = &HFEFFUS
    Private Const WordClearBit07 As UInt16 = &HFF7FUS
    Private Const WordClearBit06 As UInt16 = &HFFBFUS
    Private Const WordClearBit05 As UInt16 = &HFFDFUS
    Private Const WordClearBit04 As UInt16 = &HFFEFUS
    Private Const WordClearBit03 As UInt16 = &HFFF7US
    Private Const WordClearBit02 As UInt16 = &HFFFBUS
    Private Const WordClearBit01 As UInt16 = &HFFFDUS
    Private Const WordClearBit00 As UInt16 = &HFFFEUS

    Private Const WordSetBit15 As UInt16 = &H8000US
    Private Const WordSetBit14 As UInt16 = &H4000US
    Private Const WordSetBit13 As UInt16 = &H2000US
    Private Const WordSetBit12 As UInt16 = &H1000US
    Private Const WordSetBit11 As UInt16 = &H800US
    Private Const WordSetBit10 As UInt16 = &H400US
    Private Const WordSetBit09 As UInt16 = &H200US
    Private Const WordSetBit08 As UInt16 = &H100US
    Private Const WordSetBit07 As UInt16 = &H80US
    Private Const WordSetBit06 As UInt16 = &H40US
    Private Const WordSetBit05 As UInt16 = &H20US
    Private Const WordSetBit04 As UInt16 = &H10US
    Private Const WordSetBit03 As UInt16 = &H8US
    Private Const WordSetBit02 As UInt16 = &H4US
    Private Const WordSetBit01 As UInt16 = &H2US
    Private Const WordSetBit00 As UInt16 = &H1US


    '--------bit 7 is high bit....
    Private Const ByteClearBit07 As Byte = &H7F
    Private Const ByteClearBit06 As Byte = &HBF
    Private Const ByteClearBit05 As Byte = &HDF
    Private Const ByteClearBit04 As Byte = &HEF
    Private Const ByteClearBit03 As Byte = &HF7
    Private Const ByteClearBit02 As Byte = &HFB
    Private Const ByteClearBit01 As Byte = &HFD
    Private Const ByteClearBit00 As Byte = &HFE

    Private Const ByteSetBit07 As Byte = &H80
    Private Const ByteSetBit06 As Byte = &H40
    Private Const ByteSetBit05 As Byte = &H20
    Private Const ByteSetBit04 As Byte = &H10
    Private Const ByteSetBit03 As Byte = &H8
    Private Const ByteSetBit02 As Byte = &H4
    Private Const ByteSetBit01 As Byte = &H2
    Private Const ByteSetBit00 As Byte = &H1

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
                LED_Addr15.Brightness = If((LocByteArray(1) And ByteSetBit07) <> 0, 100, 0)
                LED_Addr14.Brightness = If((LocByteArray(1) And ByteSetBit06) <> 0, 100, 0)
                LED_Addr13.Brightness = If((LocByteArray(1) And ByteSetBit05) <> 0, 100, 0)
                LED_Addr12.Brightness = If((LocByteArray(1) And ByteSetBit04) <> 0, 100, 0)

                LED_Addr11.Brightness = If((LocByteArray(1) And ByteSetBit03) <> 0, 100, 0)
                LED_Addr10.Brightness = If((LocByteArray(1) And ByteSetBit02) <> 0, 100, 0)
                LED_Addr09.Brightness = If((LocByteArray(1) And ByteSetBit01) <> 0, 100, 0)
                LED_Addr08.Brightness = If((LocByteArray(1) And ByteSetBit00) <> 0, 100, 0)
                LocLastByteArray(1) = LocByteArray(1)
            End If


            If (LocByteArray(0) <> LocLastByteArray(0)) Then
                LED_Addr07.Brightness = If((LocByteArray(0) And ByteSetBit07) <> 0, 100, 0)
                LED_Addr06.Brightness = If((LocByteArray(0) And ByteSetBit06) <> 0, 100, 0)
                LED_Addr05.Brightness = If((LocByteArray(0) And ByteSetBit05) <> 0, 100, 0)
                LED_Addr04.Brightness = If((LocByteArray(0) And ByteSetBit04) <> 0, 100, 0)

                LED_Addr03.Brightness = If((LocByteArray(0) And ByteSetBit03) <> 0, 100, 0)
                LED_Addr02.Brightness = If((LocByteArray(0) And ByteSetBit02) <> 0, 100, 0)
                LED_Addr01.Brightness = If((LocByteArray(0) And ByteSetBit01) <> 0, 100, 0)
                LED_Addr00.Brightness = If((LocByteArray(0) And ByteSetBit00) <> 0, 100, 0)
                LocLastByteArray(0) = LocByteArray(0)
            End If


            '--------16 bit data
            If (LocByteArray(3) <> LocLastByteArray(3)) Then
                LED_Data15.Brightness = If((LocByteArray(3) And ByteSetBit07) <> 0, 100, 0)
                LED_Data14.Brightness = If((LocByteArray(3) And ByteSetBit06) <> 0, 100, 0)
                LED_Data13.Brightness = If((LocByteArray(3) And ByteSetBit05) <> 0, 100, 0)
                LED_Data12.Brightness = If((LocByteArray(3) And ByteSetBit04) <> 0, 100, 0)

                LED_Data11.Brightness = If((LocByteArray(3) And ByteSetBit03) <> 0, 100, 0)
                LED_Data10.Brightness = If((LocByteArray(3) And ByteSetBit02) <> 0, 100, 0)
                LED_Data09.Brightness = If((LocByteArray(3) And ByteSetBit01) <> 0, 100, 0)
                LED_Data08.Brightness = If((LocByteArray(3) And ByteSetBit00) <> 0, 100, 0)
                LocLastByteArray(3) = LocByteArray(3)
            End If

            If (LocByteArray(2) <> LocLastByteArray(2)) Then
                LED_Data07.Brightness = If((LocByteArray(2) And ByteSetBit07) <> 0, 100, 0)
                LED_Data06.Brightness = If((LocByteArray(2) And ByteSetBit06) <> 0, 100, 0)
                LED_Data05.Brightness = If((LocByteArray(2) And ByteSetBit05) <> 0, 100, 0)
                LED_Data04.Brightness = If((LocByteArray(2) And ByteSetBit04) <> 0, 100, 0)

                LED_Data03.Brightness = If((LocByteArray(2) And ByteSetBit03) <> 0, 100, 0)
                LED_Data02.Brightness = If((LocByteArray(2) And ByteSetBit02) <> 0, 100, 0)
                LED_Data01.Brightness = If((LocByteArray(2) And ByteSetBit01) <> 0, 100, 0)
                LED_Data00.Brightness = If((LocByteArray(2) And ByteSetBit00) <> 0, 100, 0)
                LocLastByteArray(2) = LocByteArray(2)
            End If

            '--------16 bit switch register.
            If (LocByteArray(5) <> LocLastByteArray(5)) Then
                If (LocByteArray(5) And ByteSetBit07) <> 0 Then
                    LED_Switch15.Brightness = 100
                    locSwitches = locSwitches Or WordSetBit15
                Else
                    LED_Switch15.Brightness = 0
                    locSwitches = locSwitches And WordClearBit15
                End If
                If (LocByteArray(5) And ByteSetBit06) <> 0 Then
                    LED_Switch14.Brightness = 100
                    locSwitches = locSwitches Or WordSetBit14
                Else
                    LED_Switch14.Brightness = 0
                    locSwitches = locSwitches And WordClearBit14
                End If
                If (LocByteArray(5) And ByteSetBit05) <> 0 Then
                    LED_Switch13.Brightness = 100
                    locSwitches = locSwitches Or WordSetBit13
                Else
                    LED_Switch13.Brightness = 0
                    locSwitches = locSwitches And WordClearBit13
                End If
                If (LocByteArray(5) And ByteSetBit04) <> 0 Then
                    LED_Switch12.Brightness = 100
                    locSwitches = locSwitches Or WordSetBit12
                Else
                    LED_Switch12.Brightness = 0
                    locSwitches = locSwitches And WordClearBit12
                End If

                If (LocByteArray(5) And ByteSetBit03) <> 0 Then
                    LED_Switch11.Brightness = 100
                    locSwitches = locSwitches Or WordSetBit11
                Else
                    LED_Switch11.Brightness = 0
                    locSwitches = locSwitches And WordClearBit11
                End If
                If (LocByteArray(5) And ByteSetBit02) <> 0 Then
                    LED_Switch10.Brightness = 100
                    locSwitches = locSwitches Or WordSetBit10
                Else
                    LED_Switch10.Brightness = 0
                    locSwitches = locSwitches And WordClearBit10
                End If
                If (LocByteArray(5) And ByteSetBit01) <> 0 Then
                    LED_Switch09.Brightness = 100
                    locSwitches = locSwitches Or WordSetBit09
                Else
                    LED_Switch09.Brightness = 0
                    locSwitches = locSwitches And WordClearBit09
                End If
                If (LocByteArray(5) And ByteSetBit00) <> 0 Then
                    LED_Switch08.Brightness = 100
                    locSwitches = locSwitches Or WordSetBit08
                Else
                    LED_Switch08.Brightness = 0
                    locSwitches = locSwitches And WordClearBit08
                End If
                LocLastByteArray(5) = LocByteArray(5)
            End If

            If (LocByteArray(4) <> LocLastByteArray(4)) Then
                If (LocByteArray(4) And ByteSetBit07) <> 0 Then
                    LED_Switch07.Brightness = 100
                    locSwitches = locSwitches Or WordSetBit07
                Else
                    LED_Switch07.Brightness = 0
                    locSwitches = locSwitches And WordClearBit07
                End If
                If (LocByteArray(4) And ByteSetBit06) <> 0 Then
                    LED_Switch06.Brightness = 100
                    locSwitches = locSwitches Or WordSetBit06
                Else
                    LED_Switch06.Brightness = 0
                    locSwitches = locSwitches And WordClearBit07
                End If
                If (LocByteArray(4) And ByteSetBit05) <> 0 Then
                    LED_Switch05.Brightness = 100
                    locSwitches = locSwitches Or WordSetBit05
                Else
                    LED_Switch05.Brightness = 0
                    locSwitches = locSwitches And WordClearBit05
                End If
                If (LocByteArray(4) And ByteSetBit04) <> 0 Then
                    LED_Switch04.Brightness = 100
                    locSwitches = locSwitches Or WordSetBit04
                Else
                    LED_Switch04.Brightness = 0
                    locSwitches = locSwitches And WordClearBit04
                End If

                If (LocByteArray(4) And ByteSetBit03) <> 0 Then
                    LED_Switch03.Brightness = 100
                    locSwitches = locSwitches Or WordSetBit03
                Else
                    LED_Switch03.Brightness = 0
                    locSwitches = locSwitches And WordClearBit03
                End If
                If (LocByteArray(4) And ByteSetBit02) <> 0 Then
                    LED_Switch02.Brightness = 100
                    locSwitches = locSwitches Or WordSetBit02
                Else
                    LED_Switch02.Brightness = 0
                    locSwitches = locSwitches And WordClearBit02
                End If
                If (LocByteArray(4) And ByteSetBit01) <> 0 Then
                    LED_Switch01.Brightness = 100
                    locSwitches = locSwitches Or WordSetBit01
                Else
                    LED_Switch01.Brightness = 0
                    locSwitches = locSwitches And WordClearBit01
                End If
                If (LocByteArray(4) And ByteSetBit00) <> 0 Then
                    LED_Switch00.Brightness = 100
                    locSwitches = locSwitches Or WordSetBit00
                Else
                    LED_Switch00.Brightness = 0
                    locSwitches = locSwitches And WordClearBit00
                End If
                LocLastByteArray(4) = LocByteArray(4)
            End If

            '--------Misc
            If (LocByteArray(7) <> LocLastByteArray(7)) Then
                LED_CC_N.Brightness = If((LocByteArray(7) And ByteSetBit07) <> 0, 100, 0)
                LED_CC_Z.Brightness = If((LocByteArray(7) And ByteSetBit06) <> 0, 100, 0)
                LED_CC_O.Brightness = If((LocByteArray(7) And ByteSetBit05) <> 0, 100, 0)
                LED_CC_C.Brightness = If((LocByteArray(7) And ByteSetBit04) <> 0, 100, 0)

                LED_MemProt.Brightness = If((LocByteArray(7) And ByteSetBit03) <> 0, 100, 0)
                LED_Priv.Brightness = If((LocByteArray(7) And ByteSetBit02) <> 0, 100, 0)
                LED_PM.Brightness = If((LocByteArray(7) And ByteSetBit01) <> 0, 100, 0)
                LED_Virt.Brightness = If((LocByteArray(7) And ByteSetBit00) <> 0, 100, 0)
                LocLastByteArray(7) = LocByteArray(7)
            End If

            If (LocByteArray(6) <> LocLastByteArray(6)) Then
                LED_IoInt.Brightness = If((LocByteArray(6) And ByteSetBit07) <> 0, 100, 0)
                LED_TaskInt.Brightness = If((LocByteArray(6) And ByteSetBit06) <> 0, 100, 0)
                LED_MemErr.Brightness = If((LocByteArray(6) And ByteSetBit05) <> 0, 100, 0)
                '--LED_EMA04.Brightness = If((locByteArray(6) And ByteSetBit04) <> 0, 100, 0) '-- reserved for 7860

                LED_EMA03.Brightness = If((LocByteArray(6) And ByteSetBit03) <> 0, 100, 0)
                LED_EMA02.Brightness = If((LocByteArray(6) And ByteSetBit02) <> 0, 100, 0)
                LED_EMA01.Brightness = If((LocByteArray(6) And ByteSetBit01) <> 0, 100, 0)
                LED_EMA00.Brightness = If((LocByteArray(6) And ByteSetBit00) <> 0, 100, 0)
                LocLastByteArray(6) = LocByteArray(6)
            End If

            If (LocByteArray(9) <> LocLastByteArray(9)) Then
                LED_Power.Brightness = If((LocByteArray(9) And ByteSetBit07) <> 0, 100, 0)
                LED_Standby.Brightness = If((LocByteArray(9) And ByteSetBit06) <> 0, 100, 0)
                LED_BackupFailure.Brightness = If((LocByteArray(9) And ByteSetBit05) <> 0, 100, 0)
                '--------update LED and switch
                If (LocByteArray(9) And ByteSetBit04) <> 0 Then
                    LED_Run.Brightness = 100
                    RunHaltSwitch.IsOn = True
                    locRun = True
                Else
                    LED_Run.Brightness = 0
                    RunHaltSwitch.IsOn = False
                    locRun = False
                End If

                '--unused--LED_xx.Brightness = If((locByteArray(9) And ByteSetBit03) <> 0, 100, 0)
                '--unused--LED_xx.Brightness = If((locByteArray(9) And ByteSetBit02) <> 0, 100, 0)
                '--unused--LED_xx.Brightness = If((locByteArray(9) And ByteSetBit01) <> 0, 100, 0)
                '--------exit command....
                If (LocByteArray(9) And ByteSetBit00) <> 0 Then
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
                If (LocByteArray(5) And ByteSetBit07) <> 0 Then
                    LED_Switch15.Brightness = 100
                    locSwitches = locSwitches Or WordSetBit15
                Else
                    LED_Switch15.Brightness = 0
                    locSwitches = locSwitches And WordClearBit15
                End If
                If (LocByteArray(5) And ByteSetBit06) <> 0 Then
                    LED_Switch14.Brightness = 100
                    locSwitches = locSwitches Or WordSetBit14
                Else
                    LED_Switch14.Brightness = 0
                    locSwitches = locSwitches And WordClearBit14
                End If
                If (LocByteArray(5) And ByteSetBit05) <> 0 Then
                    LED_Switch13.Brightness = 100
                    locSwitches = locSwitches Or WordSetBit13
                Else
                    LED_Switch13.Brightness = 0
                    locSwitches = locSwitches And WordClearBit13
                End If
                If (LocByteArray(5) And ByteSetBit04) <> 0 Then
                    LED_Switch12.Brightness = 100
                    locSwitches = locSwitches Or WordSetBit12
                Else
                    LED_Switch12.Brightness = 0
                    locSwitches = locSwitches And WordClearBit12
                End If

                If (LocByteArray(5) And ByteSetBit03) <> 0 Then
                    LED_Switch11.Brightness = 100
                    locSwitches = locSwitches Or WordSetBit11
                Else
                    LED_Switch11.Brightness = 0
                    locSwitches = locSwitches And WordClearBit11
                End If
                If (LocByteArray(5) And ByteSetBit02) <> 0 Then
                    LED_Switch10.Brightness = 100
                    locSwitches = locSwitches Or WordSetBit10
                Else
                    LED_Switch10.Brightness = 0
                    locSwitches = locSwitches And WordClearBit10
                End If
                If (LocByteArray(5) And ByteSetBit01) <> 0 Then
                    LED_Switch09.Brightness = 100
                    locSwitches = locSwitches Or WordSetBit09
                Else
                    LED_Switch09.Brightness = 0
                    locSwitches = locSwitches And WordClearBit09
                End If
                If (LocByteArray(5) And ByteSetBit00) <> 0 Then
                    LED_Switch08.Brightness = 100
                    locSwitches = locSwitches Or WordSetBit08
                Else
                    LED_Switch08.Brightness = 0
                    locSwitches = locSwitches And WordClearBit08
                End If
                LocLastByteArray(5) = LocByteArray(5)
            End If

            If (LocByteArray(4) <> LocLastByteArray(4)) Then
                If (LocByteArray(4) And ByteSetBit07) <> 0 Then
                    LED_Switch07.Brightness = 100
                    locSwitches = locSwitches Or WordSetBit07
                Else
                    LED_Switch07.Brightness = 0
                    locSwitches = locSwitches And WordClearBit07
                End If
                If (LocByteArray(4) And ByteSetBit06) <> 0 Then
                    LED_Switch06.Brightness = 100
                    locSwitches = locSwitches Or WordSetBit06
                Else
                    LED_Switch06.Brightness = 0
                    locSwitches = locSwitches And WordClearBit06
                End If
                If (LocByteArray(4) And ByteSetBit05) <> 0 Then
                    LED_Switch05.Brightness = 100
                    locSwitches = locSwitches Or WordSetBit05
                Else
                    LED_Switch05.Brightness = 0
                    locSwitches = locSwitches And WordClearBit05
                End If
                If (LocByteArray(4) And ByteSetBit04) <> 0 Then
                    LED_Switch04.Brightness = 100
                    locSwitches = locSwitches Or WordSetBit04
                Else
                    LED_Switch04.Brightness = 0
                    locSwitches = locSwitches And WordClearBit04
                End If

                If (LocByteArray(4) And ByteSetBit03) <> 0 Then
                    LED_Switch03.Brightness = 100
                    locSwitches = locSwitches Or WordSetBit03
                Else
                    LED_Switch03.Brightness = 0
                    locSwitches = locSwitches And WordClearBit03
                End If
                If (LocByteArray(4) And ByteSetBit02) <> 0 Then
                    LED_Switch02.Brightness = 100
                    locSwitches = locSwitches Or WordSetBit02
                Else
                    LED_Switch02.Brightness = 0
                    locSwitches = locSwitches And WordClearBit02
                End If
                If (LocByteArray(4) And ByteSetBit01) <> 0 Then
                    LED_Switch01.Brightness = 100
                    locSwitches = locSwitches Or WordSetBit01
                Else
                    LED_Switch01.Brightness = 0
                    locSwitches = locSwitches And WordClearBit01
                End If
                If (LocByteArray(4) And ByteSetBit00) <> 0 Then
                    LED_Switch00.Brightness = 100
                    locSwitches = locSwitches Or WordSetBit00
                Else
                    LED_Switch00.Brightness = 0
                    locSwitches = locSwitches And WordClearBit00
                End If
                LocLastByteArray(4) = LocByteArray(4)
            End If

            '--------Misc 3
            LED_CC_N.Brightness = CInt(LocByteArray(BRIGHT_MISC3_START + 1)) '--If((locByteArray(7) And ByteSetBit07) <> 0, 100, 0)
            LED_CC_Z.Brightness = CInt(LocByteArray(BRIGHT_MISC3_START + 0)) '--If((locByteArray(7) And ByteSetBit06) <> 0, 100, 0)
            LED_CC_O.Brightness = CInt(LocByteArray(BRIGHT_MISC3_START + 3)) '--If((locByteArray(7) And ByteSetBit05) <> 0, 100, 0)
            LED_CC_C.Brightness = CInt(LocByteArray(BRIGHT_MISC3_START + 2)) '--If((locByteArray(7) And ByteSetBit04) <> 0, 100, 0)

            LED_MemProt.Brightness = CInt(LocByteArray(BRIGHT_MISC3_START + 5)) '--If((locByteArray(7) And ByteSetBit03) <> 0, 100, 0)
            LED_Priv.Brightness = CInt(LocByteArray(BRIGHT_MISC3_START + 4)) '--If((locByteArray(7) And ByteSetBit02) <> 0, 100, 0)
            LED_PM.Brightness = CInt(LocByteArray(BRIGHT_MISC3_START + 7)) '--If((locByteArray(7) And ByteSetBit01) <> 0, 100, 0)
            LED_Virt.Brightness = CInt(LocByteArray(BRIGHT_MISC3_START + 6)) '--If((locByteArray(7) And ByteSetBit00) <> 0, 100, 0)

            LED_IoInt.Brightness = CInt(LocByteArray(BRIGHT_MISC3_START + 9)) '--If((locByteArray(6) And ByteSetBit07) <> 0, 100, 0)
            LED_TaskInt.Brightness = CInt(LocByteArray(BRIGHT_MISC3_START + 8)) '--If((locByteArray(6) And ByteSetBit06) <> 0, 100, 0)
            LED_MemErr.Brightness = CInt(LocByteArray(BRIGHT_MISC3_START + 11)) '--If((locByteArray(6) And ByteSetBit05) <> 0, 100, 0)
            '--LED_EMA04.Brightness = CInt(locByteArray(BRIGHT_MISC3_START + 10)) '--If((locByteArray(6) And ByteSetBit04) <> 0, 100, 0) '-- reserved for 7860

            LED_EMA03.Brightness = CInt(LocByteArray(BRIGHT_MISC3_START + 13)) '--If((locByteArray(6) And ByteSetBit03) <> 0, 100, 0)
            LED_EMA02.Brightness = CInt(LocByteArray(BRIGHT_MISC3_START + 12)) '--If((locByteArray(6) And ByteSetBit02) <> 0, 100, 0)
            LED_EMA01.Brightness = CInt(LocByteArray(BRIGHT_MISC3_START + 15)) '--If((locByteArray(6) And ByteSetBit01) <> 0, 100, 0)
            LED_EMA00.Brightness = CInt(LocByteArray(BRIGHT_MISC3_START + 14)) '--If((locByteArray(6) And ByteSetBit00) <> 0, 100, 0)

            ' --------MISC4.   These values don't change quickly.   Just use on/off.
            If (LocByteArray(9) <> LocLastByteArray(9)) Then
                LED_Power.Brightness = If((LocByteArray(9) And ByteSetBit07) <> 0, 100, 0)
                LED_Standby.Brightness = If((LocByteArray(9) And ByteSetBit06) <> 0, 100, 0)
                LED_BackupFailure.Brightness = If((LocByteArray(9) And ByteSetBit05) <> 0, 100, 0)
                '--------update led and switch
                If (LocByteArray(9) And ByteSetBit04) <> 0 Then
                    LED_Run.Brightness = 100
                    RunHaltSwitch.IsOn = True
                    locRun = True
                Else
                    LED_Run.Brightness = 0
                    RunHaltSwitch.IsOn = False
                    locRun = False
                End If

                '--unused--LED_xx.Brightness = If((locByteArray(9) And ByteSetBit03) <> 0, 100, 0)
                '--unused--LED_xx.Brightness = If((locByteArray(9) And ByteSetBit02) <> 0, 100, 0)
                '--unused--LED_xx.Brightness = If((locByteArray(9) And ByteSetBit01) <> 0, 100, 0)
                '--unused--LED_xx.Brightness = If((locByteArray(9) And ByteSetBit00) <> 0, 100, 0)
                '--------exit command....
                If (LocByteArray(9) And ByteSetBit00) <> 0 Then
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
        FrmAboutBox.ShowDialog()
    End Sub

    Private Sub ExitToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles ExitToolStripMenuItem.Click
        frmSettings.ShowDialog()
    End Sub

    Private Sub RegSelSwitch01_MouseClick(sender As Object, e As MouseEventArgs) Handles RegSelSwitch01.MouseClick
        Debug.WriteLine("got mouseclick")
        RegSelSwitch01.Toggle()
        '--------update switch value
        If RegSelSwitch01.IsOn Then
            locRegDisplaySelect = locRegDisplaySelect Or WordSetBit00
        Else
            locRegDisplaySelect = locRegDisplaySelect And WordClearBit00
        End If
        '--------send new value to simulator..
        Call SendCmdRegDispSel()
    End Sub

    Private Sub RegSelSwitch02_MouseClick(sender As Object, e As MouseEventArgs) Handles RegSelSwitch02.MouseClick
        Debug.WriteLine("got mouseclick")
        RegSelSwitch02.Toggle()
        '--------update switch value
        If RegSelSwitch02.IsOn Then
            locRegDisplaySelect = locRegDisplaySelect Or WordSetBit01
        Else
            locRegDisplaySelect = locRegDisplaySelect And WordClearBit01
        End If
        '--------send new value to simulator..
        Call SendCmdRegDispSel()
    End Sub

    Private Sub RegSelSwitch03_MouseClick(sender As Object, e As MouseEventArgs) Handles RegSelSwitch03.MouseClick
        Debug.WriteLine("got mouseclick")
        RegSelSwitch03.Toggle()
        '--------update switch value
        If RegSelSwitch03.IsOn Then
            locRegDisplaySelect = locRegDisplaySelect Or WordSetBit02
        Else
            locRegDisplaySelect = locRegDisplaySelect And WordClearBit02
        End If
        '--------send new value to simulator..
        Call SendCmdRegDispSel()
    End Sub

    Private Sub RegSelSwitch04_MouseClick(sender As Object, e As MouseEventArgs) Handles RegSelSwitch04.MouseClick
        Debug.WriteLine("got mouseclick")
        RegSelSwitch04.Toggle()
        '--------update switch value
        If RegSelSwitch04.IsOn Then
            locRegDisplaySelect = locRegDisplaySelect Or WordSetBit03
        Else
            locRegDisplaySelect = locRegDisplaySelect And WordClearBit03
        End If
        '--------send new value to simulator..
        Call SendCmdRegDispSel()
    End Sub

    Private Sub RegSelSwitch05_MouseClick(sender As Object, e As MouseEventArgs) Handles RegSelSwitch05.MouseClick
        Debug.WriteLine("got mouseclick")
        RegSelSwitch05.Toggle()
        '--------update switch value
        If RegSelSwitch05.IsOn Then
            locRegDisplaySelect = locRegDisplaySelect Or WordSetBit04
        Else
            locRegDisplaySelect = locRegDisplaySelect And WordClearBit04
        End If
        '--------send new value to simulator..
        Call SendCmdRegDispSel()
    End Sub

    Private Sub RegSelSwitch06_MouseClick(sender As Object, e As MouseEventArgs) Handles RegSelSwitch06.MouseClick
        Debug.WriteLine("got mouseclick")
        RegSelSwitch06.Toggle()
        '--------update switch value
        If RegSelSwitch06.IsOn Then
            locRegDisplaySelect = locRegDisplaySelect Or WordSetBit05
        Else
            locRegDisplaySelect = locRegDisplaySelect And WordClearBit05
        End If
        '--------send new value to simulator..
        Call SendCmdRegDispSel()
    End Sub

    Private Sub RegSelSwitch07_MouseClick(sender As Object, e As MouseEventArgs) Handles RegSelSwitch07.MouseClick
        Debug.WriteLine("got mouseclick")
        RegSelSwitch07.Toggle()
        '--------update switch value
        If RegSelSwitch07.IsOn Then
            locRegDisplaySelect = locRegDisplaySelect Or WordSetBit06
        Else
            locRegDisplaySelect = locRegDisplaySelect And WordClearBit06
        End If
        '--------send new value to simulator..
        Call SendCmdRegDispSel()
    End Sub

    Private Sub RegSelSwitch08_MouseClick(sender As Object, e As MouseEventArgs) Handles RegSelSwitch08.MouseClick
        Debug.WriteLine("got mouseclick")
        RegSelSwitch08.Toggle()
        '--------update switch value
        If RegSelSwitch08.IsOn Then
            locRegDisplaySelect = locRegDisplaySelect Or WordSetBit07
        Else
            locRegDisplaySelect = locRegDisplaySelect And WordClearBit07
        End If
        '--------send new value to simulator..
        Call SendCmdRegDispSel()
    End Sub


    Private Sub RegSelSwitch09_MouseClick(sender As Object, e As MouseEventArgs) Handles RegSelSwitch09.MouseClick
        Debug.WriteLine("got mouseclick")
        RegSelSwitch09.Toggle()
        '--------update switch value
        If RegSelSwitch09.IsOn Then
            locRegDisplaySelect = locRegDisplaySelect Or WordSetBit08
        Else
            locRegDisplaySelect = locRegDisplaySelect And WordClearBit08
        End If
        '--------send new value to simulator..
        Call SendCmdRegDispSel()
    End Sub

    Private Sub RegSelSwitch10_MouseClick(sender As Object, e As MouseEventArgs) Handles RegSelSwitch10.MouseClick
        Debug.WriteLine("got mouseclick")
        RegSelSwitch10.Toggle()
        '--------update switch value
        If RegSelSwitch10.IsOn Then
            locRegDisplaySelect = locRegDisplaySelect Or WordSetBit09
        Else
            locRegDisplaySelect = locRegDisplaySelect And WordClearBit09
        End If
        '--------send new value to simulator..
        Call SendCmdRegDispSel()
    End Sub

    Private Sub RegSelSwitch11_MouseClick(sender As Object, e As MouseEventArgs) Handles RegSelSwitch11.MouseClick
        Debug.WriteLine("got mouseclick")
        RegSelSwitch11.Toggle()
        '--------update switch value
        If RegSelSwitch11.IsOn Then
            locRegDisplaySelect = locRegDisplaySelect Or WordSetBit10
        Else
            locRegDisplaySelect = locRegDisplaySelect And WordClearBit10
        End If
        '--------send new value to simulator..
        Call SendCmdRegDispSel()
    End Sub

    Private Sub RegSelSwitch12_MouseClick(sender As Object, e As MouseEventArgs) Handles RegSelSwitch12.MouseClick
        Debug.WriteLine("got mouseclick")
        RegSelSwitch12.Toggle()
        '--------update switch value
        If RegSelSwitch12.IsOn Then
            locRegDisplaySelect = locRegDisplaySelect Or WordSetBit11
        Else
            locRegDisplaySelect = locRegDisplaySelect And WordClearBit11
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
            locMemoryMode = locMemoryMode And WordClearBit00
        End If
        Call SendCmdSetMemMode()
    End Sub

    Private Sub VirtualActualSwitch_MouseClick(sender As Object, e As MouseEventArgs) Handles VirtualActualSwitch.MouseClick
        Debug.WriteLine("got mouseclick - virt/act")
        VirtualActualSwitch.Toggle()
        If VirtualActualSwitch.IsOn Then
            locMemoryMode = locMemoryMode Or 2US
        Else
            locMemoryMode = locMemoryMode And WordClearBit01
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

    ' -------- switch 15

    Private Sub FPSwitch15_LatchClick(sender As Object, e As EventArgs) Handles FPSwitch15.LatchClick
        If (FPSwitch15.Latch) Then
            FPSwitch15.SetOff()
            FPSwitch15.Value = False
            LED_Switch15.Brightness = 0
            locSwitches = locSwitches And WordClearBit15
            Call SendCmdSetSwitches()
        Else
            FPSwitch15.SetLatched()
            LED_Switch15.Brightness = 100
            locSwitches = locSwitches Or WordSetBit15
            Call SendCmdSetSwitches()
        End If
    End Sub


    Private Sub FPSwitch15_ToggleClick(sender As Object, e As EventArgs) Handles FPSwitch15.ToggleClick
        FPSwitch15.SetOn()
        FPSwitch15.Refresh()
        LED_Switch15.Brightness = 100
        locSwitches = locSwitches Or WordSetBit15
        Call SendCmdSetSwitches()
        Thread.Sleep(200)
        FPSwitch15.SetOff()
    End Sub


    '--------switch 14
    Private Sub FPSwitch14_LatchClick(sender As Object, e As EventArgs) Handles FPSwitch14.LatchClick
        If FPSwitch14.Latch Then
            FPSwitch14.SetOff()
            FPSwitch14.Value = False
            LED_Switch14.Brightness = 0
            locSwitches = locSwitches And WordClearBit14
            Call SendCmdSetSwitches()
        Else
            FPSwitch14.SetLatched()
            LED_Switch14.Brightness = 100
            locSwitches = locSwitches Or WordSetBit14
            Call SendCmdSetSwitches()
        End If
    End Sub

    Private Sub FPSwitch14_ToggleClick(sender As Object, e As EventArgs) Handles FPSwitch14.ToggleClick
        FPSwitch14.SetOn()
        FPSwitch14.Refresh()
        LED_Switch14.Brightness = 100
        locSwitches = locSwitches Or WordSetBit14
        Call SendCmdSetSwitches()
        Thread.Sleep(200)
        FPSwitch14.SetOff()
    End Sub



    '--------switch 13
    Private Sub FPSwitch13_LatchClick(sender As Object, e As EventArgs) Handles FPSwitch13.LatchClick
        If FPSwitch13.Latch Then
            FPSwitch13.SetOff()
            FPSwitch13.Value = False
            LED_Switch13.Brightness = 0
            locSwitches = locSwitches And WordClearBit13
            Call SendCmdSetSwitches()
        Else
            FPSwitch13.SetLatched()
            LED_Switch13.Brightness = 100
            locSwitches = locSwitches Or WordSetBit13
            Call SendCmdSetSwitches()
        End If
    End Sub

    Private Sub FPSwitch13_ToggleClick(sender As Object, e As EventArgs) Handles FPSwitch13.ToggleClick
        FPSwitch13.SetOn()
        FPSwitch13.Refresh()
        LED_Switch13.Brightness = 100
        locSwitches = locSwitches Or WordSetBit13
        Call SendCmdSetSwitches()
        Thread.Sleep(200)
        FPSwitch13.SetOff()
    End Sub




    '--------switch 12
    Private Sub FPSwitch12_LatchClick(sender As Object, e As EventArgs) Handles FPSwitch12.LatchClick
        If FPSwitch12.Latch Then
            FPSwitch12.SetOff()
            FPSwitch12.Value = False
            LED_Switch12.Brightness = 0
            locSwitches = locSwitches And WordClearBit12
            Call SendCmdSetSwitches()
        Else
            FPSwitch12.SetLatched()
            LED_Switch12.Brightness = 100
            locSwitches = locSwitches Or WordSetBit12
            Call SendCmdSetSwitches()
        End If
    End Sub

    Private Sub FPSwitch12_ToggleClick(sender As Object, e As EventArgs) Handles FPSwitch12.ToggleClick
        FPSwitch12.SetOn()
        FPSwitch12.Refresh()
        LED_Switch12.Brightness = 100
        locSwitches = locSwitches Or WordSetBit12
        Call SendCmdSetSwitches()
        Thread.Sleep(200)
        FPSwitch12.SetOff()
    End Sub



    '--------switch 11
    Private Sub FPSwitch11_LatchClick(sender As Object, e As EventArgs) Handles FPSwitch11.LatchClick
        If FPSwitch11.Latch Then
            FPSwitch11.SetOff()
            FPSwitch11.Value = False
            LED_Switch11.Brightness = 0
            locSwitches = locSwitches And WordClearBit11
            Call SendCmdSetSwitches()
        Else
            FPSwitch11.SetLatched()
            LED_Switch11.Brightness = 100
            locSwitches = locSwitches Or WordSetBit11
            Call SendCmdSetSwitches()
        End If
    End Sub

    Private Sub FPSwitch11_ToggleClick(sender As Object, e As EventArgs) Handles FPSwitch11.ToggleClick
        FPSwitch11.SetOn()
        FPSwitch11.Refresh()
        LED_Switch11.Brightness = 100
        locSwitches = locSwitches Or WordSetBit11
        Call SendCmdSetSwitches()
        Thread.Sleep(200)
        FPSwitch11.SetOff()
    End Sub


    '--------switch 10
    Private Sub FPSwitch10_LatchClick(sender As Object, e As EventArgs) Handles FPSwitch10.LatchClick
        If FPSwitch10.Latch Then
            FPSwitch10.SetOff()
            FPSwitch10.Value = False
            LED_Switch10.Brightness = 0
            locSwitches = locSwitches And WordClearBit10
            Call SendCmdSetSwitches()
        Else
            FPSwitch10.SetLatched()
            LED_Switch10.Brightness = 100
            locSwitches = locSwitches Or WordSetBit10
            Call SendCmdSetSwitches()
        End If
    End Sub

    Private Sub FPSwitch10_ToggleClick(sender As Object, e As EventArgs) Handles FPSwitch10.ToggleClick
        FPSwitch10.SetOn()
        FPSwitch10.Refresh()
        LED_Switch10.Brightness = 100
        locSwitches = locSwitches Or WordSetBit10
        Call SendCmdSetSwitches()
        Thread.Sleep(200)
        FPSwitch10.SetOff()
    End Sub


    '--------switch 09
    Private Sub FPSwitch09_LatchClick(sender As Object, e As EventArgs) Handles FPSwitch09.LatchClick
        If FPSwitch09.Latch Then
            FPSwitch09.SetOff()
            FPSwitch09.Value = False
            LED_Switch09.Brightness = 0
            locSwitches = locSwitches And WordClearBit09
            Call SendCmdSetSwitches()
        Else
            FPSwitch09.SetLatched()
            LED_Switch09.Brightness = 100
            locSwitches = locSwitches Or WordSetBit09
            Call SendCmdSetSwitches()
        End If
    End Sub

    Private Sub FPSwitch09_ToggleClick(sender As Object, e As EventArgs) Handles FPSwitch09.ToggleClick
        FPSwitch09.SetOn()
        FPSwitch09.Refresh()
        LED_Switch09.Brightness = 100
        locSwitches = locSwitches Or WordSetBit09
        Call SendCmdSetSwitches()
        Thread.Sleep(200)
        FPSwitch09.SetOff()
    End Sub


    '--------switch 08
    Private Sub FPSwitch08_LatchClick(sender As Object, e As EventArgs) Handles FPSwitch08.LatchClick
        If FPSwitch08.Latch Then
            FPSwitch08.SetOff()
            FPSwitch08.Value = False
            LED_Switch08.Brightness = 0
            locSwitches = locSwitches And WordClearBit08
            Call SendCmdSetSwitches()
        Else
            FPSwitch08.SetLatched()
            LED_Switch08.Brightness = 100
            locSwitches = locSwitches Or WordSetBit08
            Call SendCmdSetSwitches()
        End If
    End Sub

    Private Sub FPSwitch08_ToggleClick(sender As Object, e As EventArgs) Handles FPSwitch08.ToggleClick
        FPSwitch08.SetOn()
        FPSwitch08.Refresh()
        LED_Switch08.Brightness = 100
        locSwitches = locSwitches Or WordSetBit08
        Call SendCmdSetSwitches()
        Thread.Sleep(200)
        FPSwitch08.SetOff()
    End Sub


    '--------switch 07
    Private Sub FPSwitch07_LatchClick(sender As Object, e As EventArgs) Handles FPSwitch07.LatchClick
        If FPSwitch07.Latch Then
            FPSwitch07.SetOff()
            FPSwitch07.Value = False
            LED_Switch07.Brightness = 0
            locSwitches = locSwitches And WordClearBit07
            Call SendCmdSetSwitches()
        Else
            FPSwitch07.SetLatched()
            LED_Switch07.Brightness = 100
            locSwitches = locSwitches Or WordSetBit07
            Call SendCmdSetSwitches()
        End If
    End Sub

    Private Sub FPSwitch07_ToggleClick(sender As Object, e As EventArgs) Handles FPSwitch07.ToggleClick
        FPSwitch07.SetOn()
        FPSwitch07.Refresh()
        LED_Switch07.Brightness = 100
        locSwitches = locSwitches Or WordSetBit07
        Call SendCmdSetSwitches()
        Thread.Sleep(200)
        FPSwitch07.SetOff()
    End Sub


    '--------switch 06
    Private Sub FPSwitch06_LatchClick(sender As Object, e As EventArgs) Handles FPSwitch06.LatchClick
        If FPSwitch06.Latch Then
            FPSwitch06.SetOff()
            FPSwitch06.Value = False
            LED_Switch06.Brightness = 0
            locSwitches = locSwitches And WordClearBit06
            Call SendCmdSetSwitches()
        Else
            FPSwitch06.SetLatched()
            LED_Switch06.Brightness = 100
            locSwitches = locSwitches Or WordSetBit06
            Call SendCmdSetSwitches()
        End If
    End Sub

    Private Sub FPSwitch06_ToggleClick(sender As Object, e As EventArgs) Handles FPSwitch06.ToggleClick
        FPSwitch06.SetOn()
        FPSwitch06.Refresh()
        LED_Switch06.Brightness = 100
        locSwitches = locSwitches Or WordSetBit06
        Call SendCmdSetSwitches()
        Thread.Sleep(200)
        FPSwitch06.SetOff()
    End Sub


    '--------switch 05
    Private Sub FPSwitch05_LatchClick(sender As Object, e As EventArgs) Handles FPSwitch05.LatchClick
        If FPSwitch05.Latch Then
            FPSwitch05.SetOff()
            FPSwitch05.Value = False
            LED_Switch05.Brightness = 0
            locSwitches = locSwitches And WordClearBit05
            Call SendCmdSetSwitches()
        Else
            FPSwitch05.SetLatched()
            LED_Switch05.Brightness = 100
            locSwitches = locSwitches Or WordSetBit05
            Call SendCmdSetSwitches()
        End If
    End Sub

    Private Sub FPSwitch05_ToggleClick(sender As Object, e As EventArgs) Handles FPSwitch05.ToggleClick
        FPSwitch05.SetOn()
        FPSwitch05.Refresh()
        LED_Switch05.Brightness = 100
        locSwitches = locSwitches Or WordSetBit05
        Call SendCmdSetSwitches()
        Thread.Sleep(200)
        FPSwitch05.SetOff()
    End Sub


    '--------switch 04
    Private Sub FPSwitch04_LatchClick(sender As Object, e As EventArgs) Handles FPSwitch04.LatchClick
        If FPSwitch04.Latch Then
            FPSwitch04.SetOff()
            FPSwitch04.Value = False
            LED_Switch04.Brightness = 0
            locSwitches = locSwitches And WordClearBit04
            Call SendCmdSetSwitches()
        Else
            FPSwitch04.SetLatched()
            LED_Switch04.Brightness = 100
            locSwitches = locSwitches Or WordSetBit04
            Call SendCmdSetSwitches()
        End If
    End Sub

    Private Sub FPSwitch04_ToggleClick(sender As Object, e As EventArgs) Handles FPSwitch04.ToggleClick
        FPSwitch04.SetOn()
        FPSwitch04.Refresh()
        LED_Switch04.Brightness = 100
        locSwitches = locSwitches Or WordSetBit04
        Call SendCmdSetSwitches()
        Thread.Sleep(200)
        FPSwitch04.SetOff()
    End Sub


    '--------switch 03
    Private Sub FPSwitch03_LatchClick(sender As Object, e As EventArgs) Handles FPSwitch03.LatchClick
        If FPSwitch03.Latch Then
            FPSwitch03.SetOff()
            FPSwitch03.Value = False
            LED_Switch03.Brightness = 0
            locSwitches = locSwitches And WordClearBit03
            Call SendCmdSetSwitches()
        Else
            FPSwitch03.SetLatched()
            LED_Switch03.Brightness = 100
            locSwitches = locSwitches Or WordSetBit03
            Call SendCmdSetSwitches()
        End If
    End Sub

    Private Sub FPSwitch03_ToggleClick(sender As Object, e As EventArgs) Handles FPSwitch03.ToggleClick
        FPSwitch03.SetOn()
        FPSwitch03.Refresh()
        LED_Switch03.Brightness = 100
        locSwitches = locSwitches Or WordSetBit03
        Call SendCmdSetSwitches()
        Thread.Sleep(200)
        FPSwitch03.SetOff()
    End Sub


    '--------switch 02
    Private Sub FPSwitch02_LatchClick(sender As Object, e As EventArgs) Handles FPSwitch02.LatchClick
        If FPSwitch02.Latch Then
            FPSwitch02.SetOff()
            FPSwitch02.Value = False
            LED_Switch02.Brightness = 0
            locSwitches = locSwitches And WordClearBit02
            Call SendCmdSetSwitches()
        Else
            FPSwitch02.SetLatched()
            LED_Switch02.Brightness = 100
            locSwitches = locSwitches Or WordSetBit02
            Call SendCmdSetSwitches()
        End If
    End Sub

    Private Sub FPSwitch02_ToggleClick(sender As Object, e As EventArgs) Handles FPSwitch02.ToggleClick
        FPSwitch02.SetOn()
        FPSwitch02.Refresh()
        LED_Switch02.Brightness = 100
        locSwitches = locSwitches Or WordSetBit02
        Call SendCmdSetSwitches()
        Thread.Sleep(200)
        FPSwitch02.SetOff()
    End Sub


    '--------switch 01
    Private Sub FPSwitch01_LatchClick(sender As Object, e As EventArgs) Handles FPSwitch01.LatchClick
        If FPSwitch01.Latch Then
            FPSwitch01.SetOff()
            FPSwitch01.Value = False
            LED_Switch01.Brightness = 0
            locSwitches = locSwitches And WordClearBit01
            Call SendCmdSetSwitches()
        Else
            FPSwitch01.SetLatched()
            LED_Switch01.Brightness = 100
            locSwitches = locSwitches Or WordSetBit01
            Call SendCmdSetSwitches()
        End If
    End Sub

    Private Sub FPSwitch01_ToggleClick(sender As Object, e As EventArgs) Handles FPSwitch01.ToggleClick
        FPSwitch01.SetOn()
        FPSwitch01.Refresh()
        LED_Switch01.Brightness = 100
        locSwitches = locSwitches Or WordSetBit01
        Call SendCmdSetSwitches()
        Thread.Sleep(200)
        FPSwitch01.SetOff()
    End Sub


    '--------switch 00
    Private Sub FPSwitch00_LatchClick(sender As Object, e As EventArgs) Handles FPSwitch00.LatchClick
        If FPSwitch00.Latch Then
            FPSwitch00.SetOff()
            FPSwitch00.Value = False
            LED_Switch00.Brightness = 0
            locSwitches = locSwitches And WordClearBit00
            Call SendCmdSetSwitches()
        Else
            FPSwitch00.SetLatched()
            LED_Switch00.Brightness = 100
            locSwitches = locSwitches Or WordSetBit00
            Call SendCmdSetSwitches()
        End If
    End Sub

    Private Sub FPSwitch00_ToggleClick(sender As Object, e As EventArgs) Handles FPSwitch00.ToggleClick
        FPSwitch00.SetOn()
        FPSwitch00.Refresh()
        LED_Switch00.Brightness = 100
        locSwitches = locSwitches Or WordSetBit00
        Call SendCmdSetSwitches()
        Thread.Sleep(200)
        FPSwitch00.SetOff()
    End Sub

    Private Sub FPClear_ToggleClick(sender As Object, e As EventArgs) Handles FPClear.ToggleClick
        FPClear.SetOn()
        FPClear.Refresh()
        If Not FPSwitch15.Latch Then
            FPSwitch15.SetOff()
            FPSwitch15.Value = False
            LED_Switch15.Brightness = 0
            locSwitches = locSwitches And WordClearBit15
        End If
        If Not FPSwitch14.Latch Then
            FPSwitch14.SetOff()
            FPSwitch14.Value = False
            LED_Switch14.Brightness = 0
            locSwitches = locSwitches And WordClearBit14
        End If
        If Not FPSwitch13.Latch Then
            FPSwitch13.SetOff()
            FPSwitch13.Value = False
            LED_Switch13.Brightness = 0
            locSwitches = locSwitches And WordClearBit13
        End If
        If Not FPSwitch12.Latch Then
            FPSwitch12.SetOff()
            FPSwitch12.Value = False
            LED_Switch12.Brightness = 0
            locSwitches = locSwitches And WordClearBit12
        End If
        If Not FPSwitch11.Latch Then
            FPSwitch11.SetOff()
            FPSwitch11.Value = False
            LED_Switch11.Brightness = 0
            locSwitches = locSwitches And WordClearBit11
        End If
        If Not FPSwitch10.Latch Then
            FPSwitch10.SetOff()
            FPSwitch10.Value = False
            LED_Switch10.Brightness = 0
            locSwitches = locSwitches And WordClearBit10
        End If
        If Not FPSwitch09.Latch Then
            FPSwitch09.SetOff()
            FPSwitch09.Value = False
            LED_Switch09.Brightness = 0
            locSwitches = locSwitches And WordClearBit09
        End If
        If Not FPSwitch08.Latch Then
            FPSwitch08.SetOff()
            FPSwitch08.Value = False
            LED_Switch08.Brightness = 0
            locSwitches = locSwitches And WordClearBit08
        End If
        If Not FPSwitch07.Latch Then
            FPSwitch07.SetOff()
            FPSwitch07.Value = False
            LED_Switch07.Brightness = 0
            locSwitches = locSwitches And WordClearBit07
        End If
        If Not FPSwitch06.Latch Then
            FPSwitch06.SetOff()
            FPSwitch06.Value = False
            LED_Switch06.Brightness = 0
            locSwitches = locSwitches And WordClearBit06
        End If
        If Not FPSwitch05.Latch Then
            FPSwitch05.SetOff()
            FPSwitch05.Value = False
            LED_Switch05.Brightness = 0
            locSwitches = locSwitches And WordClearBit05
        End If
        If Not FPSwitch04.Latch Then
            FPSwitch04.SetOff()
            FPSwitch04.Value = False
            LED_Switch04.Brightness = 0
            locSwitches = locSwitches And WordClearBit04
        End If
        If Not FPSwitch03.Latch Then
            FPSwitch03.SetOff()
            FPSwitch03.Value = False
            LED_Switch03.Brightness = 0
            locSwitches = locSwitches And WordClearBit03
        End If
        If Not FPSwitch02.Latch Then
            FPSwitch02.SetOff()
            FPSwitch02.Value = False
            LED_Switch02.Brightness = 0
            locSwitches = locSwitches And WordClearBit02
        End If
        If Not FPSwitch01.Latch Then
            FPSwitch01.SetOff()
            FPSwitch01.Value = False
            LED_Switch01.Brightness = 0
            locSwitches = locSwitches And WordClearBit01
        End If
        If Not FPSwitch00.Latch Then
            FPSwitch00.SetOff()
            FPSwitch00.Value = False
            LED_Switch00.Brightness = 0
            locSwitches = locSwitches And WordClearBit00
        End If
        Call SendCmdSetSwitches()
        Thread.Sleep(200)
        FPClear.SetOff()
    End Sub
End Class


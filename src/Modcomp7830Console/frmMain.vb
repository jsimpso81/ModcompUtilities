Option Strict On

Imports System.ComponentModel
Imports System
Imports System.Net
Imports System.Net.Sockets
Imports System.Text
Imports System.Numerics



Public Class frmMain

    Private DemoIndex As Integer = 0

    Private WithEvents udpSendClient As UdpClient
    Private WithEvents remoteSendEndPoint As IPEndPoint
    Private SendPort As Integer = 57831
    '--------implement a blocking concurrent queue...... for the data...

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

    Delegate Sub typProcSimData(ByRef loc_recv_bytes As Byte())
    Public MyDelProc As New typProcSimData(AddressOf ProcSimData)


    Private Sub PictureBox1_Click(sender As Object, e As EventArgs) Handles PictureBox1.Click
        If (LED_Power.Brightness > 50) Then
            LED_Power.Brightness = 0
        Else
            LED_Power.Brightness = 100
        End If
    End Sub

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
        Call UDP_Close(UdpRecvClient)
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
            If parm_recv_bytes.Length <> locByteArray.Length Then
                ReDim Preserve locByteArray(parm_recv_bytes.Length - 1)
            End If
            If parm_recv_bytes.Length <> locLastByteArray.Length Then
                ReDim Preserve locLastByteArray(parm_recv_bytes.Length - 1)
            End If
            Buffer.BlockCopy(parm_recv_bytes, 0, locByteArray, 0, parm_recv_bytes.Length)
        End If

        '--------old version, just on or off...
        If parm_recv_bytes.Length >= 9 And parm_recv_bytes.Length <= 11 Then

            '--------BYTES are backwards....

            '--------16 bit address
            If (locByteArray(1) <> locLastByteArray(1)) Then
                LED_Addr15.Brightness = If((locByteArray(1) And &H80) <> 0, 100, 0)
                LED_Addr14.Brightness = If((locByteArray(1) And &H40) <> 0, 100, 0)
                LED_Addr13.Brightness = If((locByteArray(1) And &H20) <> 0, 100, 0)
                LED_Addr12.Brightness = If((locByteArray(1) And &H10) <> 0, 100, 0)

                LED_Addr11.Brightness = If((locByteArray(1) And &H8) <> 0, 100, 0)
                LED_Addr10.Brightness = If((locByteArray(1) And &H4) <> 0, 100, 0)
                LED_Addr09.Brightness = If((locByteArray(1) And &H2) <> 0, 100, 0)
                LED_Addr08.Brightness = If((locByteArray(1) And &H1) <> 0, 100, 0)
                locLastByteArray(1) = locByteArray(1)
            End If


            If (locByteArray(0) <> locLastByteArray(0)) Then
                LED_Addr07.Brightness = If((locByteArray(0) And &H80) <> 0, 100, 0)
                LED_Addr06.Brightness = If((locByteArray(0) And &H40) <> 0, 100, 0)
                LED_Addr05.Brightness = If((locByteArray(0) And &H20) <> 0, 100, 0)
                LED_Addr04.Brightness = If((locByteArray(0) And &H10) <> 0, 100, 0)

                LED_Addr03.Brightness = If((locByteArray(0) And &H8) <> 0, 100, 0)
                LED_Addr02.Brightness = If((locByteArray(0) And &H4) <> 0, 100, 0)
                LED_Addr01.Brightness = If((locByteArray(0) And &H2) <> 0, 100, 0)
                LED_Addr00.Brightness = If((locByteArray(0) And &H1) <> 0, 100, 0)
                locLastByteArray(0) = locByteArray(0)
            End If


            '--------16 bit data
            If (locByteArray(3) <> locLastByteArray(3)) Then
                LED_Data15.Brightness = If((locByteArray(3) And &H80) <> 0, 100, 0)
                LED_Data14.Brightness = If((locByteArray(3) And &H40) <> 0, 100, 0)
                LED_Data13.Brightness = If((locByteArray(3) And &H20) <> 0, 100, 0)
                LED_Data12.Brightness = If((locByteArray(3) And &H10) <> 0, 100, 0)

                LED_Data11.Brightness = If((locByteArray(3) And &H8) <> 0, 100, 0)
                LED_Data10.Brightness = If((locByteArray(3) And &H4) <> 0, 100, 0)
                LED_Data09.Brightness = If((locByteArray(3) And &H2) <> 0, 100, 0)
                LED_Data08.Brightness = If((locByteArray(3) And &H1) <> 0, 100, 0)
                locLastByteArray(3) = locByteArray(3)
            End If

            If (locByteArray(2) <> locLastByteArray(2)) Then
                LED_Data07.Brightness = If((locByteArray(2) And &H80) <> 0, 100, 0)
                LED_Data06.Brightness = If((locByteArray(2) And &H40) <> 0, 100, 0)
                LED_Data05.Brightness = If((locByteArray(2) And &H20) <> 0, 100, 0)
                LED_Data04.Brightness = If((locByteArray(2) And &H10) <> 0, 100, 0)

                LED_Data03.Brightness = If((locByteArray(2) And &H8) <> 0, 100, 0)
                LED_Data02.Brightness = If((locByteArray(2) And &H4) <> 0, 100, 0)
                LED_Data01.Brightness = If((locByteArray(2) And &H2) <> 0, 100, 0)
                LED_Data00.Brightness = If((locByteArray(2) And &H1) <> 0, 100, 0)
                locLastByteArray(2) = locByteArray(2)
            End If

            '--------16 bit switch register.
            If (LocByteArray(5) <> LocLastByteArray(5)) Then
                LED_Switch15.Brightness = If((LocByteArray(5) And &H80) <> 0, 100, 0)
                LED_Switch14.Brightness = If((LocByteArray(5) And &H40) <> 0, 100, 0)
                LED_Switch13.Brightness = If((LocByteArray(5) And &H20) <> 0, 100, 0)
                LED_Switch12.Brightness = If((LocByteArray(5) And &H10) <> 0, 100, 0)

                LED_Switch11.Brightness = If((LocByteArray(5) And &H8) <> 0, 100, 0)
                LED_Switch10.Brightness = If((LocByteArray(5) And &H4) <> 0, 100, 0)
                LED_Switch09.Brightness = If((LocByteArray(5) And &H2) <> 0, 100, 0)
                LED_Switch08.Brightness = If((LocByteArray(5) And &H1) <> 0, 100, 0)
                LocLastByteArray(5) = LocByteArray(5)
            End If

            If (locByteArray(4) <> locLastByteArray(4)) Then
                LED_Switch07.Brightness = If((locByteArray(4) And &H80) <> 0, 100, 0)
                LED_Switch06.Brightness = If((locByteArray(4) And &H40) <> 0, 100, 0)
                LED_Switch05.Brightness = If((locByteArray(4) And &H20) <> 0, 100, 0)
                LED_Switch04.Brightness = If((locByteArray(4) And &H10) <> 0, 100, 0)

                LED_Switch03.Brightness = If((locByteArray(4) And &H8) <> 0, 100, 0)
                LED_Switch02.Brightness = If((locByteArray(4) And &H4) <> 0, 100, 0)
                LED_Switch01.Brightness = If((locByteArray(4) And &H2) <> 0, 100, 0)
                LED_Switch00.Brightness = If((locByteArray(4) And &H1) <> 0, 100, 0)
                locLastByteArray(4) = locByteArray(4)
            End If

            '--------Misc
            If (locByteArray(7) <> locLastByteArray(7)) Then
                LED_CC_N.Brightness = If((locByteArray(7) And &H80) <> 0, 100, 0)
                LED_CC_Z.Brightness = If((locByteArray(7) And &H40) <> 0, 100, 0)
                LED_CC_O.Brightness = If((locByteArray(7) And &H20) <> 0, 100, 0)
                LED_CC_C.Brightness = If((locByteArray(7) And &H10) <> 0, 100, 0)

                LED_MemProt.Brightness = If((locByteArray(7) And &H8) <> 0, 100, 0)
                LED_Priv.Brightness = If((locByteArray(7) And &H4) <> 0, 100, 0)
                LED_PM.Brightness = If((locByteArray(7) And &H2) <> 0, 100, 0)
                LED_Virt.Brightness = If((locByteArray(7) And &H1) <> 0, 100, 0)
                locLastByteArray(7) = locByteArray(7)
            End If

            If (locByteArray(6) <> locLastByteArray(6)) Then
                LED_IoInt.Brightness = If((locByteArray(6) And &H80) <> 0, 100, 0)
                LED_TaskInt.Brightness = If((locByteArray(6) And &H40) <> 0, 100, 0)
                LED_MemErr.Brightness = If((locByteArray(6) And &H20) <> 0, 100, 0)
                '--LED_EMA04.Brightness = If((locByteArray(6) And &H10) <> 0, 100, 0) '-- reserved for 7860

                LED_EMA03.Brightness = If((locByteArray(6) And &H8) <> 0, 100, 0)
                LED_EMA02.Brightness = If((locByteArray(6) And &H4) <> 0, 100, 0)
                LED_EMA01.Brightness = If((locByteArray(6) And &H2) <> 0, 100, 0)
                LED_EMA00.Brightness = If((locByteArray(6) And &H1) <> 0, 100, 0)
                locLastByteArray(6) = locByteArray(6)
            End If

            If (locByteArray(9) <> locLastByteArray(9)) Then
                LED_Power.Brightness = If((locByteArray(9) And &H80) <> 0, 100, 0)
                LED_Standby.Brightness = If((locByteArray(9) And &H40) <> 0, 100, 0)
                LED_BackupFailure.Brightness = If((locByteArray(9) And &H20) <> 0, 100, 0)
                LED_Run.Brightness = If((locByteArray(9) And &H10) <> 0, 100, 0) '-- reserved for 7860

                '--unused--LED_xx.Brightness = If((locByteArray(9) And &H8) <> 0, 100, 0)
                '--unused--LED_xx.Brightness = If((locByteArray(9) And &H4) <> 0, 100, 0)
                '--unused--LED_xx.Brightness = If((locByteArray(9) And &H2) <> 0, 100, 0)
                '--unused--LED_xx.Brightness = If((locByteArray(9) And &H1) <> 0, 100, 0)
                locLastByteArray(9) = locByteArray(9)
            End If

            '--------nothing defined in byte 8 yet.



            '--------new version, has brightness...
        ElseIf parm_recv_bytes.Length >= 90 Then

            '--------BYTES are backwards....

            '--------16 bit address
            LED_Addr15.Brightness = CInt(locByteArray(BRIGHT_ADDR_START + 1))
            LED_Addr14.Brightness = CInt(locByteArray(BRIGHT_ADDR_START + 0))
            LED_Addr13.Brightness = CInt(locByteArray(BRIGHT_ADDR_START + 3))
            LED_Addr12.Brightness = CInt(locByteArray(BRIGHT_ADDR_START + 2))

            LED_Addr11.Brightness = CInt(locByteArray(BRIGHT_ADDR_START + 5))
            LED_Addr10.Brightness = CInt(locByteArray(BRIGHT_ADDR_START + 4))
            LED_Addr09.Brightness = CInt(locByteArray(BRIGHT_ADDR_START + 7))
            LED_Addr08.Brightness = CInt(locByteArray(BRIGHT_ADDR_START + 6))


            LED_Addr07.Brightness = CInt(locByteArray(BRIGHT_ADDR_START + 9))
            LED_Addr06.Brightness = CInt(locByteArray(BRIGHT_ADDR_START + 8))
            LED_Addr05.Brightness = CInt(locByteArray(BRIGHT_ADDR_START + 11))
            LED_Addr04.Brightness = CInt(locByteArray(BRIGHT_ADDR_START + 10))

            LED_Addr03.Brightness = CInt(locByteArray(BRIGHT_ADDR_START + 13))
            LED_Addr02.Brightness = CInt(locByteArray(BRIGHT_ADDR_START + 12))
            LED_Addr01.Brightness = CInt(locByteArray(BRIGHT_ADDR_START + 15))
            LED_Addr00.Brightness = CInt(locByteArray(BRIGHT_ADDR_START + 14))

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
            LED_Data15.Brightness = CInt(locByteArray(BRIGHT_DATA_START + 1))
            LED_Data14.Brightness = CInt(locByteArray(BRIGHT_DATA_START + 0))
            LED_Data13.Brightness = CInt(locByteArray(BRIGHT_DATA_START + 3))
            LED_Data12.Brightness = CInt(locByteArray(BRIGHT_DATA_START + 2))

            LED_Data11.Brightness = CInt(locByteArray(BRIGHT_DATA_START + 5))
            LED_Data10.Brightness = CInt(locByteArray(BRIGHT_DATA_START + 4))
            LED_Data09.Brightness = CInt(locByteArray(BRIGHT_DATA_START + 7))
            LED_Data08.Brightness = CInt(locByteArray(BRIGHT_DATA_START + 6))


            LED_Data07.Brightness = CInt(locByteArray(BRIGHT_DATA_START + 9))
            LED_Data06.Brightness = CInt(locByteArray(BRIGHT_DATA_START + 8))
            LED_Data05.Brightness = CInt(locByteArray(BRIGHT_DATA_START + 11))
            LED_Data04.Brightness = CInt(locByteArray(BRIGHT_DATA_START + 10))

            LED_Data03.Brightness = CInt(locByteArray(BRIGHT_DATA_START + 13))
            LED_Data02.Brightness = CInt(locByteArray(BRIGHT_DATA_START + 12))
            LED_Data01.Brightness = CInt(locByteArray(BRIGHT_DATA_START + 15))
            LED_Data00.Brightness = CInt(locByteArray(BRIGHT_DATA_START + 14))

            '--------16 bit switch register.   It doesn't change much so just use off / on
            If (LocByteArray(5) <> LocLastByteArray(5)) Then
                LED_Switch15.Brightness = If((LocByteArray(5) And &H80) <> 0, 100, 0)
                LED_Switch14.Brightness = If((LocByteArray(5) And &H40) <> 0, 100, 0)
                LED_Switch13.Brightness = If((LocByteArray(5) And &H20) <> 0, 100, 0)
                LED_Switch12.Brightness = If((LocByteArray(5) And &H10) <> 0, 100, 0)

                LED_Switch11.Brightness = If((LocByteArray(5) And &H8) <> 0, 100, 0)
                LED_Switch10.Brightness = If((LocByteArray(5) And &H4) <> 0, 100, 0)
                LED_Switch09.Brightness = If((LocByteArray(5) And &H2) <> 0, 100, 0)
                LED_Switch08.Brightness = If((LocByteArray(5) And &H1) <> 0, 100, 0)

                LocLastByteArray(5) = LocByteArray(5)
            End If

            If (locByteArray(4) <> locLastByteArray(4)) Then
                LED_Switch07.Brightness = If((locByteArray(4) And &H80) <> 0, 100, 0)
                LED_Switch06.Brightness = If((locByteArray(4) And &H40) <> 0, 100, 0)
                LED_Switch05.Brightness = If((locByteArray(4) And &H20) <> 0, 100, 0)
                LED_Switch04.Brightness = If((locByteArray(4) And &H10) <> 0, 100, 0)

                LED_Switch03.Brightness = If((locByteArray(4) And &H8) <> 0, 100, 0)
                LED_Switch02.Brightness = If((locByteArray(4) And &H4) <> 0, 100, 0)
                LED_Switch01.Brightness = If((locByteArray(4) And &H2) <> 0, 100, 0)
                LED_Switch00.Brightness = If((locByteArray(4) And &H1) <> 0, 100, 0)

                locLastByteArray(4) = locByteArray(4)
            End If

            '--------Misc 3
            LED_CC_N.Brightness = CInt(LocByteArray(BRIGHT_MISC3_START + 1)) '--If((locByteArray(7) And &H80) <> 0, 100, 0)
            LED_CC_Z.Brightness = CInt(locByteArray(BRIGHT_MISC3_START + 0)) '--If((locByteArray(7) And &H40) <> 0, 100, 0)
            LED_CC_O.Brightness = CInt(locByteArray(BRIGHT_MISC3_START + 3)) '--If((locByteArray(7) And &H20) <> 0, 100, 0)
            LED_CC_C.Brightness = CInt(locByteArray(BRIGHT_MISC3_START + 2)) '--If((locByteArray(7) And &H10) <> 0, 100, 0)

            LED_MemProt.Brightness = CInt(locByteArray(BRIGHT_MISC3_START + 5)) '--If((locByteArray(7) And &H8) <> 0, 100, 0)
            LED_Priv.Brightness = CInt(locByteArray(BRIGHT_MISC3_START + 4)) '--If((locByteArray(7) And &H4) <> 0, 100, 0)
            LED_PM.Brightness = CInt(locByteArray(BRIGHT_MISC3_START + 7)) '--If((locByteArray(7) And &H2) <> 0, 100, 0)
            LED_Virt.Brightness = CInt(locByteArray(BRIGHT_MISC3_START + 6)) '--If((locByteArray(7) And &H1) <> 0, 100, 0)

            LED_IoInt.Brightness = CInt(LocByteArray(BRIGHT_MISC3_START + 9)) '--If((locByteArray(6) And &H80) <> 0, 100, 0)
            LED_TaskInt.Brightness = CInt(locByteArray(BRIGHT_MISC3_START + 8)) '--If((locByteArray(6) And &H40) <> 0, 100, 0)
            LED_MemErr.Brightness = CInt(locByteArray(BRIGHT_MISC3_START + 11)) '--If((locByteArray(6) And &H20) <> 0, 100, 0)
            '--LED_EMA04.Brightness = CInt(locByteArray(BRIGHT_MISC3_START + 10)) '--If((locByteArray(6) And &H10) <> 0, 100, 0) '-- reserved for 7860

            LED_EMA03.Brightness = CInt(locByteArray(BRIGHT_MISC3_START + 13)) '--If((locByteArray(6) And &H8) <> 0, 100, 0)
            LED_EMA02.Brightness = CInt(locByteArray(BRIGHT_MISC3_START + 12)) '--If((locByteArray(6) And &H4) <> 0, 100, 0)
            LED_EMA01.Brightness = CInt(locByteArray(BRIGHT_MISC3_START + 15)) '--If((locByteArray(6) And &H2) <> 0, 100, 0)
            LED_EMA00.Brightness = CInt(locByteArray(BRIGHT_MISC3_START + 14)) '--If((locByteArray(6) And &H1) <> 0, 100, 0)

            ' --------MISC4.   These values don't change quickly.   Just use on/off.
            If (locByteArray(9) <> locLastByteArray(9)) Then
                LED_Power.Brightness = If((locByteArray(9) And &H80) <> 0, 100, 0)
                LED_Standby.Brightness = If((locByteArray(9) And &H40) <> 0, 100, 0)
                LED_BackupFailure.Brightness = If((locByteArray(9) And &H20) <> 0, 100, 0)
                LED_Run.Brightness = If((locByteArray(9) And &H10) <> 0, 100, 0) '-- reserved for 7860

                '--unused--LED_xx.Brightness = If((locByteArray(9) And &H8) <> 0, 100, 0)
                '--unused--LED_xx.Brightness = If((locByteArray(9) And &H4) <> 0, 100, 0)
                '--unused--LED_xx.Brightness = If((locByteArray(9) And &H2) <> 0, 100, 0)
                '--unused--LED_xx.Brightness = If((locByteArray(9) And &H1) <> 0, 100, 0)
                locLastByteArray(9) = locByteArray(9)
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


        Return

    End Sub

    Private Sub MenuStrip1_ItemClicked(sender As Object, e As ToolStripItemClickedEventArgs) Handles MenuStrip1.ItemClicked

    End Sub


    Private Sub UDP_Recv_Thread()

        '--Debug.Print(" UDP_Recv_Thread started.")

        Do While Not CloseNetwork

            Try
                Dim udpRecvBytes As Byte()
                '---Debug.Print("starting udp read")
                udpRecvBytes = udpRecvClient.Receive(remoteRecvEndPoint)
                '---Dim receivedData As String = Encoding.ASCII.GetString(receivedBytes)
                '---------Process the received data (e.g., display in a TextBox)
                '---------Example:  Me.Invoke(Sub() TextBox1.AppendText($"Received from {remoteIpEndPoint.Address}: {receivedData}{Environment.NewLine}"))
                '--Debug.Print("recieved " & udpRecvBytes.Length.ToString & " bytes")
                '--Debug.Print("calling delegate to process")
                Me.Invoke(Sub() myDelProc(udpRecvBytes))

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
        udpRecvClient = New UdpClient(listenPort)
        remoteRecvEndPoint = New IPEndPoint(IPAddress.Any, 0)

        '--------Start a new thread for receiving to prevent UI blocking
        Dim receiveThread As New Threading.Thread(AddressOf UDP_Recv_Thread)
        receiveThread.IsBackground = True ' Allow the application to exit even if the thread is running
        receiveThread.Start()

    End Sub

    Private Sub UDP_Send_Init()
        '--------Open UDP send client.  Send to local address...
        udpSendClient = New UdpClient(SendPort)

        '--------Get the host name of the current machine
        Dim hostName As String = Dns.GetHostName()
        Debug.WriteLine(" host name " & hostName)

        ' Retrieve all IP addresses associated with the host
        Dim ipAddresses = Dns.GetHostEntry(hostName).AddressList

        Dim useIp As New IPAddress(0)
        Dim done As Boolean = False
        ' Loop through and find the IPv4 address
        For Each ip As IPAddress In ipAddresses
            If ip.AddressFamily = Sockets.AddressFamily.InterNetwork Then
                Debug.WriteLine("IPv4 Address: " & ip.ToString())
                If Not done Then
                    useIp = ip
                    done = True
                End If
            End If
        Next

        remoteSendEndPoint = New IPEndPoint(useIp, SendPort)

        '--------Start a new thread for receiving to prevent UI blocking
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
        '--------send new value to simulator..
    End Sub

    Private Sub RegSelSwitch02_MouseClick(sender As Object, e As MouseEventArgs) Handles RegSelSwitch02.MouseClick
        Debug.WriteLine("got mouseclick")
        RegSelSwitch02.Toggle()
        '--------update switch value
        '--------send new value to simulator..
    End Sub

    Private Sub RegSelSwitch03_MouseClick(sender As Object, e As MouseEventArgs) Handles RegSelSwitch03.MouseClick
        Debug.WriteLine("got mouseclick")
        RegSelSwitch03.Toggle()
        '--------update switch value
        '--------send new value to simulator..
    End Sub

    Private Sub RegSelSwitch04_MouseClick(sender As Object, e As MouseEventArgs) Handles RegSelSwitch04.MouseClick
        Debug.WriteLine("got mouseclick")
        RegSelSwitch04.Toggle()
        '--------update switch value
        '--------send new value to simulator..
    End Sub

    Private Sub RegSelSwitch05_MouseClick(sender As Object, e As MouseEventArgs) Handles RegSelSwitch05.MouseClick
        Debug.WriteLine("got mouseclick")
        RegSelSwitch05.Toggle()
        '--------update switch value
        '--------send new value to simulator..
    End Sub

    Private Sub RegSelSwitch06_MouseClick(sender As Object, e As MouseEventArgs) Handles RegSelSwitch06.MouseClick
        Debug.WriteLine("got mouseclick")
        RegSelSwitch06.Toggle()
        '--------update switch value
        '--------send new value to simulator..
    End Sub

    Private Sub RegSelSwitch07_MouseClick(sender As Object, e As MouseEventArgs) Handles RegSelSwitch07.MouseClick
        Debug.WriteLine("got mouseclick")
        RegSelSwitch07.Toggle()
        '--------update switch value
        '--------send new value to simulator..
    End Sub

    Private Sub RegSelSwitch08_MouseClick(sender As Object, e As MouseEventArgs) Handles RegSelSwitch08.MouseClick
        Debug.WriteLine("got mouseclick")
        RegSelSwitch08.Toggle()
        '--------update switch value
        '--------send new value to simulator..
    End Sub


    Private Sub RegSelSwitch09_MouseClick(sender As Object, e As MouseEventArgs) Handles RegSelSwitch09.MouseClick
        Debug.WriteLine("got mouseclick")
        RegSelSwitch09.Toggle()
        '--------update switch value
        '--------send new value to simulator..
    End Sub

    Private Sub RegSelSwitch10_MouseClick(sender As Object, e As MouseEventArgs) Handles RegSelSwitch10.MouseClick
        Debug.WriteLine("got mouseclick")
        RegSelSwitch10.Toggle()
        '--------update switch value
        '--------send new value to simulator..
    End Sub

    Private Sub RegSelSwitch11_MouseClick(sender As Object, e As MouseEventArgs) Handles RegSelSwitch11.MouseClick
        Debug.WriteLine("got mouseclick")
        RegSelSwitch11.Toggle()
        '--------update switch value
        '--------send new value to simulator..
    End Sub

    Private Sub RegSelSwitch12_MouseClick(sender As Object, e As MouseEventArgs) Handles RegSelSwitch12.MouseClick
        Debug.WriteLine("got mouseclick")
        RegSelSwitch12.Toggle()
        '--------update switch value
        '--------send new value to simulator..
    End Sub

End Class


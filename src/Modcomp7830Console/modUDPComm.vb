Option Strict On



Imports System.Net
Imports System.Net.Sockets
Imports System.Text




Module modUDPComm


    '--------global variables...

    Sub UDP_Open(ByRef udpRecvClient As UdpClient, ByRef udpSendClient As UdpClient,
                 ByRef remoteRecvEndPoint As IPEndPoint, ByRef remoteSendEndPoint As IPEndPoint)

        '--------create object 
        udpSendClient = New UdpClient()
        '--------create remote end point
        remoteSendEndPoint = New IPEndPoint(IPAddress.Parse("127.0.0.1"), 8080) ' Replace with your target IP and port

        '--------
        udpRecvClient = New UdpClient(8080) ' Listen on port 8080
        remoteRecvEndPoint = New IPEndPoint(IPAddress.Any, 0)

    End Sub

    Sub UDP_Close(ByRef objUdpClient As UdpClient)

        objUdpClient.Close()

    End Sub
    Sub UDP_Close_Send(ByRef udpRecvClient As UdpClient, ByRef udpSendCLient As UdpClient)

        udpSendCLient.Close()
        udpRecvClient.Close()

    End Sub

    Sub UDP_Send(ByRef udpSendCLient As UdpClient, ByRef remoteSendEndpoint As IPEndPoint, ByRef message As String)
        Dim data As Byte() = Encoding.UTF8.GetBytes(message)

        udpSendCLient.Send(data, data.Length, remoteSendEndpoint)
        '--Console.WriteLine("Message sent!")

    End Sub


    Sub UDP_Recv(ByRef udpRecvClient As UdpClient, ByRef remoteRecvEndPoint As IPEndPoint, ByRef recievedData As Byte())

        '---Console.WriteLine("Waiting for a message...")
        Dim receivedData As Byte() = udpRecvClient.Receive(remoteRecvEndPoint)

        '---Console.WriteLine($"Received: {receivedMessage} from {remoteRecvEndPoint}")
    End Sub


End Module





Imports System.Net
Imports System.Net.Sockets
Imports System.Text




Module modUDPComm


    '--------global variables...
    Dim udpSendClient As UdpClient
    Dim remoteSendEndPoint As IPEndPoint
    Dim udpRecvClient As UdpClient
    Dim remoteRecvEndPoint As IPEndPoint

    Sub UDP_Open()

        '--------create object 
        udpSendClient = New UdpClient()
        '--------create remote end point
        remoteSendEndPoint = New IPEndPoint(IPAddress.Parse("127.0.0.1"), 8080) ' Replace with your target IP and port

        udpRecvClient = New UdpClient(8080) ' Listen on port 8080
        remoteRecvEndPoint = New IPEndPoint(IPAddress.Any, 0)

    End Sub

    Sub UDP_Close()

        udpSendClient.Close()
        udpRecvClient.Close()

    End Sub

    Sub UDP_Send()
        Dim message As String = "Hello, UDP!"
        Dim data As Byte() = Encoding.UTF8.GetBytes(message)

        udpSendClient.Send(data, data.Length, remoteSendEndPoint)
        Console.WriteLine("Message sent!")

    End Sub


    Sub UDP_Recv()

        Console.WriteLine("Waiting for a message...")
        Dim receivedData As Byte() = udpRecvClient.Receive(remoteRecvEndPoint)
        Dim receivedMessage As String = Encoding.UTF8.GetString(receivedData)

        Console.WriteLine($"Received: {receivedMessage} from {remoteRecvEndPoint}")
    End Sub


End Module




Imports System.ComponentModel
Imports System.Net
Imports System.Net.Sockets
Imports System.Text



Public Class frmMain
    Private Sub PictureBox1_Click(sender As Object, e As EventArgs) Handles PictureBox1.Click

    End Sub

    Private Sub frmMain_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        Call UDP_Open()

    End Sub

    Private Sub frmMain_Closed(sender As Object, e As EventArgs) Handles Me.Closed
        Debug.WriteLine("closed")

    End Sub

    Private Sub frmMain_Closing(sender As Object, e As CancelEventArgs) Handles Me.Closing
        Debug.WriteLine("closing")

    End Sub

    Private Sub frmMain_Disposed(sender As Object, e As EventArgs) Handles Me.Disposed
        Call UDP_Close()
        Debug.WriteLine("disposed")
    End Sub

    Private Sub frmMain_FormClosed(sender As Object, e As FormClosedEventArgs) Handles Me.FormClosed
        Debug.WriteLine("form closed")

    End Sub

    Private Sub frmMain_FormClosing(sender As Object, e As FormClosingEventArgs) Handles Me.FormClosing
        Debug.WriteLine("form closing")

    End Sub

    Private Sub frmMain_HelpButtonClicked(sender As Object, e As CancelEventArgs) Handles Me.HelpButtonClicked

    End Sub
End Class

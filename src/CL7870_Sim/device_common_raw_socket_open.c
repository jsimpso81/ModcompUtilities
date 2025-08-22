// ================================================================================================
//
//		Copyright 2023, 2024 James A. Simpson, all rights reserved.
//
// ================================================================================================
//
//	Module:			XXXX.c
//
//	Description:	Routines to xxxxxxx.
//
//	Externally accessible routines:
//					XXXXXXX
// 
// Internal only routines:
//					XXXXXXX
//
// Notes:
//		XXXXXX
// 
// ================================================================================================
//	Revision history:
//		6/28/2024	JAS		Added new header
// ================================================================================================

#include "simj_base.h"
// #include <winsock2.h>
// #pragma comment(lib, "ws2_32.lib") // Link with ws2_32.lib

// -------- open a raw tcp socket, listen for connections, and accept a connection.
int device_common_raw_socket_open(SIMJ_U16 port, SOCKET* tcp_socket, DWORD* last_error) {


    WSADATA wsaData;

    SOCKET listenSocket = INVALID_SOCKET;
    SOCKET clientSocket = INVALID_SOCKET;

    struct sockaddr_in serverAddr;

    struct sockaddr_in clientAddr;

    int clientAddrLen = sizeof(clientAddr);

    u_short loc_port = 0;

    //  #define BUFFER_SIZE 1024
    //  char recvBuffer[BUFFER_SIZE];
    //  int bytesReceived;


    loc_port = port;

    // -------- 1. Initialize Winsock library
    if (WSAStartup(MAKEWORD(2, 2), &wsaData) != 0) {
        fprintf(stderr, " *** ERROR *** device_common_raw_socket_open - WSAStartup failed: %d\n", WSAGetLastError());
        *last_error = WSAGetLastError();
        return 1;
    }

    // -------- 2. Create a socket
    // -------- AF_INET - IP4, SOCK_STREAM = TCP, IPPROTO_TCP = TCP
    listenSocket = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
    if (listenSocket == INVALID_SOCKET) {
        fprintf(stderr, " *** ERROR *** device_common_raw_socket_open - socket failed: %d\n", WSAGetLastError());
        *last_error = WSAGetLastError();
        WSACleanup();
        return 1;
    }

    // -------- 3. Bind the socket
    serverAddr.sin_family = AF_INET;  // -------- IP4
    serverAddr.sin_addr.s_addr = INADDR_ANY;  // -------- Listen on all available interfaces
    serverAddr.sin_port = htons(loc_port); // -------- Convert port to network byte order

    if (bind(listenSocket, (struct sockaddr*)&serverAddr, sizeof(serverAddr)) == SOCKET_ERROR) {
        fprintf(stderr, " *** ERROR *** device_common_raw_socket_open - bind failed: %d\n", WSAGetLastError());
        *last_error = WSAGetLastError();
        closesocket(listenSocket);
        WSACleanup();
        return 1;
    }

    // -------- 4. Listen for incoming connections
    // -- if (listen(listenSocket, SOMAXCONN) == SOCKET_ERROR) {
#define MY_MAX_CONN 1
    if (listen(listenSocket, MY_MAX_CONN) == SOCKET_ERROR) {
        fprintf(stderr, " *** ERROR *** device_common_raw_socket_open - listen failed: %d\n", WSAGetLastError());
        *last_error = WSAGetLastError();
        closesocket(listenSocket);
        WSACleanup();
        return 1;
    }

    printf(" *** INFO *** device_common_raw_socket_open - Listening on port %d...\n", loc_port);
    
    // TODO: fix blocking stuff.

    // 5. Accept a client connection
    clientSocket = accept(listenSocket, (struct sockaddr*)&clientAddr, &clientAddrLen);
    if (clientSocket == INVALID_SOCKET) {
        fprintf(stderr, " *** ERROR *** device_common_raw_socket_open - accept failed: %d\n", WSAGetLastError());
        *last_error = WSAGetLastError();
        closesocket(listenSocket);
        WSACleanup();
        return 1;
    }

    // TODO: Add more info on connection to message..
    printf(" *** INFO *** device_common_raw_socket_open - Connected on port %d...\n", loc_port);

    // --------close the listen socket -- should no longer need.
    closesocket(listenSocket);

    // --------return the socket info and set no error
    *tcp_socket = clientSocket;
    *last_error = 0;
    return(0);

}

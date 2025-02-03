# chat_server

An OTP application

## Build

    $ rebar3 compile

## Description

Simple OTP-based Erlang application for a chat server. It allows clients to connect via TCP, register their names, and start chatting. The server handles multiple clients, broadcasting messages to all connected clients. When a client disconnects, the server notifies other clients.

## Run the application

To start the server, run:

    $ rebar3 shell

Once the shell is loaded, the server will start and listen on the port defined in the configuration file

## Deployment application on aws

To run the terraform script:

    $ terraform init
    $ terraform apply

To destroy the resources created:
$ terraform destroy

## Test

To see the app into action:

    $ telnet ip_server 8085

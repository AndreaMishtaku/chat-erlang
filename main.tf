provider "aws" {
  region = "eu-central-1"
}

resource "aws_vpc" "chat_server_vpc" {
  cidr_block = "10.0.0.0/16"
  tags = {
    Name = "ChatServerVPC"
  }
}

resource "aws_subnet" "chat_server_subnet" {
  vpc_id                  = aws_vpc.chat_server_vpc.id
  cidr_block              = "10.0.1.0/24"
  availability_zone       = "eu-central-1a"
  map_public_ip_on_launch = true
  tags = {
    Name = "ChatServerSubnet"
  }
}

resource "aws_internet_gateway" "chat_server_igw" {
  vpc_id = aws_vpc.chat_server_vpc.id
  tags = {
    Name = "ChatServerIGW"
  }
}

resource "aws_route_table" "chat_server_rt" {
  vpc_id = aws_vpc.chat_server_vpc.id

  route {
    cidr_block = "0.0.0.0/0"
    gateway_id = aws_internet_gateway.chat_server_igw.id
  }

  tags = {
    Name = "ChatServerRouteTable"
  }
}

resource "aws_route_table_association" "chat_server_rta" {
  subnet_id      = aws_subnet.chat_server_subnet.id
  route_table_id = aws_route_table.chat_server_rt.id
}


resource "aws_security_group" "chat_server_sg" {
  name        = "chat_server_sg"
  description = "Allow chat server port access"
  vpc_id      = aws_vpc.chat_server_vpc.id

  ingress {
    from_port   = 22
    to_port     = 22
    protocol    = "tcp"
    cidr_blocks = ["0.0.0.0/0"]
  }

  ingress {
    from_port   = 8085
    to_port     = 8085
    protocol    = "tcp"
    cidr_blocks = ["0.0.0.0/0"]
  }

  egress {
    from_port   = 0
    to_port     = 0
    protocol    = "-1"
    cidr_blocks = ["0.0.0.0/0"]
  }

  tags = {
    Name = "ChatServerSecurityGroup"
  }
}

resource "aws_instance" "chat_server_instance" {
  ami           = "ami-07eef52105e8a2059"
  instance_type = "t2.micro"
  subnet_id     = aws_subnet.chat_server_subnet.id
  iam_instance_profile = aws_iam_instance_profile.ssm_profile.name
  vpc_security_group_ids = [aws_security_group.chat_server_sg.id]

  user_data = <<-EOF
              #!/bin/bash
              set -e

              # Update system packages
              sudo apt-get update -y

              # Install required dependencies
              sudo apt-get install -y git ec2-instance-connect

              #Install erlang and rebar3
              sudo apt-get install -y erlang
              sudo apt-get install -y rebar3

              # Download the project from S3
              mkdir -p /home/project
              sudo chown -R ubuntu:ubuntu /home/project
              cd /home/project
              git clone https://github.com/AndreaMishtaku/chat-erlang.git
              echo "Cloned successfully"
              cd chat-erlang
              echo "Started building the project"

              sudo rebar3 compile
              sudo rebar3 release
              cd _build/default/rel/chat_server
              sudo ./bin/chat_server daemon
              EOF

  tags = {
    Name = "ChatServer"
  }
  monitoring = true
}

resource "aws_iam_role" "ssm_role" {
  name = "SSMRole"

  assume_role_policy = jsonencode({
    Version = "2012-10-17"
    Statement = [
      {
        Action    = "sts:AssumeRole"
        Effect    = "Allow"
        Principal = { Service = "ec2.amazonaws.com" }
      }
    ]
  })
}

resource "aws_iam_role_policy_attachment" "ssm_policy" {
  role       = aws_iam_role.ssm_role.name
  policy_arn = "arn:aws:iam::aws:policy/AmazonSSMManagedInstanceCore"
}

resource "aws_iam_instance_profile" "ssm_profile" {
  name = "SSMProfile"
  role = aws_iam_role.ssm_role.name
}

resource "aws_iam_role_policy_attachment" "ec2_instance_connect_policy" {
  role       = aws_iam_role.ssm_role.name
  policy_arn = "arn:aws:iam::aws:policy/EC2InstanceConnect"
}

output "public_ip" {
  value       = aws_instance.chat_server_instance.public_ip
  description = "Public IP of the chat server"
}
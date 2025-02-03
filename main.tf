provider "aws" {
  region = "eu-central-1"
}

resource "aws_vpc" "chat_server_vpc" {
  cidr_block = "10.0.0.0/16"
  tags = {
    Name = "ChatServerVPC"
  }
}

resource "aws_subnet" "chat_server_subnet_1" {
  vpc_id                  = aws_vpc.chat_server_vpc.id
  cidr_block              = "10.0.1.0/24"
  availability_zone       = "eu-central-1a"
  map_public_ip_on_launch = true
  tags = {
    Name = "ChatServerSubnet1"
  }
}

resource "aws_subnet" "chat_server_subnet_2" {
  vpc_id                  = aws_vpc.chat_server_vpc.id
  cidr_block              = "10.0.2.0/24"
  availability_zone       = "eu-central-1b"
  map_public_ip_on_launch = true
  tags = {
    Name = "ChatServerSubnet2"
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

resource "aws_route_table_association" "chat_server_rta_1" {
  subnet_id      = aws_subnet.chat_server_subnet_1.id
  route_table_id = aws_route_table.chat_server_rt.id
}

resource "aws_route_table_association" "chat_server_rta_2" {
  subnet_id      = aws_subnet.chat_server_subnet_2.id
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

resource "aws_launch_template" "chat_server_launch_template" {
  name_prefix   = "chat-server-lt"
  image_id      = "ami-07eef52105e8a2059"
  instance_type = "t2.micro"

  network_interfaces {
    associate_public_ip_address = true
    subnet_id                   = aws_subnet.chat_server_subnet_1.id
    security_groups             = [aws_security_group.chat_server_sg.id]
  }

  iam_instance_profile {
    name = aws_iam_instance_profile.ssm_profile.name
  }

  user_data = base64encode(<<-EOF
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
  )

  tag_specifications {
    resource_type = "instance"

    tags = {
      Name = "ChatServer"
    }
  }
}

resource "aws_lb" "chat_server_lb" {
  name               = "chat-server-lb"
  internal           = false
  load_balancer_type = "application"
  security_groups    = [aws_security_group.chat_server_sg.id]
  subnets            = [aws_subnet.chat_server_subnet_1.id, aws_subnet.chat_server_subnet_2.id]

  enable_deletion_protection = false

  tags = {
    Name = "ChatServerLoadBalancer"
  }
}

resource "aws_lb_target_group" "chat_server_tg" {
  name     = "chat-server-tg"
  port     = 8085
  protocol = "HTTP"
  vpc_id   = aws_vpc.chat_server_vpc.id

  health_check {
    path                = "/"
    port                = 8085
    protocol            = "HTTP"
    healthy_threshold   = 2
    unhealthy_threshold = 2
    timeout             = 10
    interval            = 30
  }

  tags = {
    Name = "ChatServerTargetGroup"
  }
}

resource "aws_lb_listener" "chat_server_listener" {
  load_balancer_arn = aws_lb.chat_server_lb.arn
  port              = 8085
  protocol          = "HTTP"

  default_action {
    type             = "forward"
    target_group_arn = aws_lb_target_group.chat_server_tg.arn
  }
}

resource "aws_autoscaling_group" "chat_server_asg" {
  name_prefix          = "chat-server-asg"
  vpc_zone_identifier  = [aws_subnet.chat_server_subnet_1.id, aws_subnet.chat_server_subnet_2.id]
  target_group_arns    = [aws_lb_target_group.chat_server_tg.arn]
  min_size             = 1
  max_size             = 3
  desired_capacity     = 1
  health_check_type    = "ELB"

  launch_template {
    id      = aws_launch_template.chat_server_launch_template.id
    version = "$Latest"
  }

  tag {
    key                 = "Name"
    value               = "ChatServer"
    propagate_at_launch = true
  }
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

output "load_balancer_dns" {
  value       = aws_lb.chat_server_lb.dns_name
  description = "DNS name of the load balancer"
}
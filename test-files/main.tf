provider "aws" {
  region = "us-west-1"
}

locals {
  one = 1
}

resource "aws_instance" "myec2" {
  ami           = "ami-12345qwert"
  instance_type = local.one
}

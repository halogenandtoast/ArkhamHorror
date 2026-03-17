variable "aws_region" {
  type    = string
  default = "ap-northeast-1"
}

variable "project_name" {
  type    = string
  default = "arkham-horror"
}

variable "environment" {
  type    = string
  default = "prod"
}

variable "domain_name" {
  type    = string
  default = "arkhamhorror.app"
}

variable "cluster_name" {
  type    = string
  default = "arkham-horror-prod"
}

variable "db_name" {
  type    = string
  default = "arkham_horror_backend"
}

variable "db_username" {
  type    = string
  default = "arkham_pg_user"
}

variable "db_password" {
  type      = string
  sensitive = true
}

variable "node_instance_types" {
  type    = list(string)
  default = ["t4g.medium"]
}

variable "desired_size" {
  type    = number
  default = 2
}

variable "min_size" {
  type    = number
  default = 1
}

variable "max_size" {
  type    = number
  default = 3
}


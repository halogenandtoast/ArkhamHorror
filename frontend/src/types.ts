export interface User {
  username: string;
  email: string;
  beta: boolean;
}

export interface Authentication {
  token: string;
}

export interface Credentials {
  email: string;
  password: string;
}

export interface Registration {
  username: string;
  email: string;
  password: string;
}

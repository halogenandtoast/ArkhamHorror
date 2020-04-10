export interface User {
  username: string;
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

export interface LoginState {
  currentUser?: User;
}

export interface GameState {
  cycles: string[];
  scenarios: Record<string, string[]>;
  game: string;
}

export interface RootState {
  version: string;
}

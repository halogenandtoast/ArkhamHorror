export interface User {
  username: string;
  email: string;
  beta: boolean;
  phaseTransitionNotifications?: boolean;
  admin: boolean;
  /* Features an admin has let this account into, from the users row. Not the
   * same as `beta`, which the account sets for itself. */
  features?: string[];
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

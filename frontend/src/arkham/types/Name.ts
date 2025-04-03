import * as JsonDecoder from 'ts.data.json';

export type Name = {
  title: string;
  subtitle: string | null;
}

export const nameDecoder = JsonDecoder.object<Name>(
  {
    title: JsonDecoder.string(),
    subtitle: JsonDecoder.nullable(JsonDecoder.string()),
  },
  'Name'
);

export function simpleName(name: Name) {
    return name.title
}

export function fullName(name: Name) {
  return name.subtitle ? `${name.title}: ${name.subtitle}` : name.title
}

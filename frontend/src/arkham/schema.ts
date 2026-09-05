// The constructor schema the ability editor builds against, served by
// /api/v1/arkham/schema and reified from the Haskell types themselves.
import { reactive, ref } from 'vue'
import api from '@/api'

export type FieldSchema = { name: string | null; type: string }
export type ConSchema = { name: string; fields: FieldSchema[] }
export type TypeSchema = {
  name: string
  constructors: ConSchema[]
  record: boolean
  enum: boolean
  alias: string | null
}

const types = reactive(new Map<string, TypeSchema>())
export const schemaLoaded = ref(false)
let loading: Promise<void> | null = null

export async function loadSchema() {
  if (schemaLoaded.value) return
  loading ??= (async () => {
    const { data } = await api.get('arkham/schema')
    for (const t of data as TypeSchema[]) types.set(t.name, t)
    schemaLoaded.value = true
  })()
  try {
    await loading
  } catch (e) {
    loading = null
    console.error(e)
  }
}

export function typeSchema(name: string): TypeSchema | undefined {
  return types.get(name)
}

/* A rendered type is one of: a list, an optional, a known sum type, or a leaf
 * the editor renders with a plain input. */
export type Shape =
  | { kind: 'list'; inner: string }
  | { kind: 'maybe'; inner: string }
  | { kind: 'sum'; schema: TypeSchema }
  | { kind: 'text' }
  | { kind: 'number' }
  | { kind: 'bool' }
  | { kind: 'raw'; type: string }

export function shapeOf(type: string, depth = 0): Shape {
  const t = type.trim()

  if (t.startsWith('[') && t.endsWith(']')) return { kind: 'list', inner: t.slice(1, -1) }
  if (t.startsWith('Maybe ')) return { kind: 'maybe', inner: t.slice(6).trim() }

  const known = types.get(t)
  // A synonym stands for another type: Who is an InvestigatorMatcher. Follow it,
  // with a depth guard in case the schema ever describes a cycle.
  if (known?.alias && depth < 8) return shapeOf(known.alias, depth + 1)
  if (known) return { kind: 'sum', schema: known }

  if (t === 'Text' || t === 'String') return { kind: 'text' }
  if (t === 'Int' || t === 'Integer' || t === 'Double') return { kind: 'number' }
  if (t === 'Bool') return { kind: 'bool' }

  return { kind: 'raw', type: t }
}

/* Aeson's TaggedObject encoding, which is what these types serialize with:
 * a record constructor puts its fields alongside the tag, a positional one puts
 * them in `contents` (bare when there is exactly one).
 *
 * The exception is a type whose constructors are *all* nullary: aeson encodes
 * those as a bare string (allNullaryToStringTag), and sending the tagged form
 * instead fails to decode — silently, since a modifier or ability that will not
 * parse simply does nothing. */
export function encodeConstructor(schema: TypeSchema, con: ConSchema, values: Record<string, any>): any {
  if (schema.enum) return con.name
  if (con.fields.length === 0) return { tag: con.name, contents: [] }

  if (con.fields.every((f) => f.name)) {
    const out: Record<string, any> = { tag: con.name }
    for (const field of con.fields) out[field.name!] = values[field.name!]
    return out
  }

  const positional = con.fields.map((_, i) => values[String(i)])
  return { tag: con.name, contents: positional.length === 1 ? positional[0] : positional }
}

export function decodeConstructor(schema: TypeSchema, value: any): { con: ConSchema; values: Record<string, any> } | null {
  if (schema.enum) {
    const con = schema.constructors.find((c) => c.name === value)
    return con ? { con, values: {} } : null
  }

  if (!value || typeof value !== 'object' || !value.tag) return null
  const con = schema.constructors.find((c) => c.name === value.tag)
  if (!con) return null

  const values: Record<string, any> = {}
  if (con.fields.every((f) => f.name)) {
    for (const field of con.fields) values[field.name!] = value[field.name!]
  } else if (con.fields.length === 1) {
    /* A one-field constructor holds its value in `contents` unwrapped -- and
     * that value may itself be a list (EnemyMatchAll [EnemyMatcher]), so taking
     * the first element here would quietly drop everything after it. */
    values['0'] = value.contents
  } else {
    const contents = Array.isArray(value.contents) ? value.contents : []
    con.fields.forEach((_, i) => (values[String(i)] = contents[i]))
  }
  return { con, values }
}

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

/* Every message the engine can send, by the name a handler listens for.
 *
 * Most messages sit inside a grouping constructor — `Defeated` is really
 * `DefeatMessage (Defeated_ …)` — and the constructor inside carries a trailing
 * underscore. Handlers name them the way the engine's own pattern synonyms do,
 * so the schema is flattened to match. */
export function messageConstructors(): Map<string, FieldSchema[]> {
  const found = new Map<string, FieldSchema[]>()
  for (const [name, type] of types) {
    if (name !== 'Message' && !name.endsWith('Message')) continue
    for (const con of type.constructors) {
      // A grouping constructor is just the wrapper; its own contents are listed
      // separately under the type it wraps.
      if (con.fields.length === 1 && con.fields[0].type === con.name) continue
      found.set(con.name.replace(/_$/, ''), con.fields)
    }
  }
  return found
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

  const exact = types.get(t)
  // A synonym stands for another type: Who is an InvestigatorMatcher. Follow it,
  // with a depth guard in case the schema ever describes a cycle.
  if (exact?.alias && depth < 8) return shapeOf(exact.alias, depth + 1)
  if (exact) return { kind: 'sum', schema: exact }

  const [head, ...args] = t.split(/\s+/)
  const applied = args.length ? types.get(head) : undefined
  if (applied) return { kind: 'sum', schema: substituteTypeArgs(applied, args) }

  if (t === 'Text' || t === 'String') return { kind: 'text' }
  if (t === 'Int' || t === 'Integer' || t === 'Double') return { kind: 'number' }
  if (t === 'Bool') return { kind: 'bool' }

  return { kind: 'raw', type: t }
}

/* A field's type can be applied — `EffectMetadata Message` — while the schema
 * registers only the head, `EffectMetadata`, with its own variable left standing
 * (`EffectMessages [a]`). Rendering the head alone is most of the answer, since
 * the argument does not change which constructors the type has; the variable
 * then has to be filled in, or a field typed `[a]` is a raw JSON box inside an
 * otherwise editable type.
 *
 * Without any of this the lookup missed outright, which is how the second
 * argument of `CreateWindowModifierEffect` — the documented way to scope a
 * modifier — ended up unbuildable.
 *
 * The schema does not record a type's variables in order, so this substitutes a
 * lone argument for every variable it finds: a type of one argument is the only
 * shape the reified types actually have. */
function substituteTypeArgs(schema: TypeSchema, args: string[]): TypeSchema {
  if (args.length !== 1) return schema
  const arg = args[0]
  // A variable is a bare lowercase identifier; a concrete type is capitalised.
  // The test pattern is deliberately separate from the replace pattern: a /g
  // regex carries lastIndex between calls, so sharing one makes `some` skip
  // every other field it is asked about.
  const hasVariable = (type: string) => /\b[a-z]\w*\b/.test(type)
  if (!schema.constructors.some((c) => c.fields.some((f) => hasVariable(f.type)))) return schema
  return {
    ...schema,
    constructors: schema.constructors.map((c) => ({
      ...c,
      fields: c.fields.map((f) => ({ ...f, type: f.type.replace(/\b[a-z]\w*\b/g, arg) })),
    })),
  }
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

  const named = con.fields.length > 0 && con.fields.every((f) => f.name)
  const positional = con.fields.map((_, i) => values[String(i)])

  /* A type with one constructor carries no tag at all: aeson's
   * `tagSingleConstructors` is False by default and nothing here overrides it,
   * so `Modifier`, `DamageAssignment` and the other 48 encode as a bare record
   * or a bare value. Tagging them anyway produced JSON that silently failed to
   * decode, and left the editor unable to read its own output back. */
  if (isUntagged(schema)) {
    if (named) return Object.fromEntries(con.fields.map((f) => [f.name!, values[f.name!]]))
    return positional.length === 1 ? positional[0] : positional
  }

  if (con.fields.length === 0) return { tag: con.name, contents: [] }
  if (named) {
    const out: Record<string, any> = { tag: con.name }
    for (const field of con.fields) out[field.name!] = values[field.name!]
    return out
  }
  return { tag: con.name, contents: positional.length === 1 ? positional[0] : positional }
}

// | Whether the type's constructor is written down in its JSON.
export function isUntagged(schema: TypeSchema): boolean {
  return !schema.enum && schema.constructors.length === 1
}

export function decodeConstructor(schema: TypeSchema, value: any): { con: ConSchema; values: Record<string, any> } | null {
  if (schema.enum) {
    const con = schema.constructors.find((c) => c.name === value)
    return con ? { con, values: {} } : null
  }

  // The mirror of encodeConstructor's untagged case: there is only one
  // constructor it could be, so the value stands for itself.
  if (isUntagged(schema)) {
    const con = schema.constructors[0]
    const named = con.fields.length > 0 && con.fields.every((f) => f.name)
    if (named) {
      if (!value || typeof value !== 'object' || Array.isArray(value)) return null
      return { con, values: Object.fromEntries(con.fields.map((f) => [f.name!, value[f.name!]])) }
    }
    if (con.fields.length === 1) return { con, values: { '0': value } }
    const contents = Array.isArray(value) ? value : []
    return { con, values: Object.fromEntries(con.fields.map((_, i) => [String(i), contents[i]])) }
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

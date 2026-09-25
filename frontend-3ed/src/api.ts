import { getToken } from '@/authToken'
import type { Catalog, TableSummary, TableView, User, GameMode } from '@/types'

export class ApiError extends Error {
  constructor(
    message: string,
    public status: number,
  ) {
    super(message)
  }
}

async function request<T>(method: string, path: string, body?: unknown): Promise<T> {
  const headers: Record<string, string> = {}
  const token = getToken()
  if (token) headers.Authorization = `Token ${token}`
  if (body !== undefined) headers['Content-Type'] = 'application/json'
  let r: Response
  try {
    r = await fetch(`/api/v1${path}`, {
      method,
      headers,
      body: body === undefined ? undefined : JSON.stringify(body),
    })
  } catch {
    throw new ApiError('Could not reach the server', 0)
  }
  const text = await r.text()
  let j: unknown = null
  try {
    j = text ? JSON.parse(text) : null
  } catch {
    j = null
  }
  if (!r.ok) {
    const msg = (j as { error?: string } | null)?.error ?? `${r.status} ${r.statusText}`
    throw new ApiError(msg, r.status)
  }
  return j as T
}

export const errorText = (e: unknown) => (e instanceof Error ? e.message : String(e))

export const whoami = () => request<User>('GET', '/whoami')
export const getCatalog = () => request<Catalog>('GET', '/3ed/catalog')
export const listTables = () => request<{ open: TableSummary[]; mine: TableSummary[] }>('GET', '/3ed/tables')

export interface NewTable {
  name?: string
  seats: number
  expansions: string[]
  mode: GameMode
  debug: boolean
}
export const createTable = (body: NewTable) => request<TableView>('POST', '/3ed/tables', body)
export const getTable = (id: string) => request<TableView>('GET', `/3ed/tables/${id}`)
export const closeTable = (id: string) => request<null>('DELETE', `/3ed/tables/${id}`)
export const joinTable = (id: string, seat?: number) =>
  request<TableView>('POST', `/3ed/tables/${id}/join`, seat === undefined ? {} : { seat })
export const leaveTable = (id: string, seat?: number) =>
  request<TableView>('POST', `/3ed/tables/${id}/leave`, seat === undefined ? {} : { seat })
export const startTable = (id: string) => request<TableView>('POST', `/3ed/tables/${id}/start`, {})
// version: the table being answered; the server refuses an answer to a question that has since changed
export const answer = (id: string, player: number, choice: number, version: number) =>
  request<TableView>('POST', `/3ed/tables/${id}/answer`, { player, choice, version })
export const debug = (id: string, action: { tag: string; contents?: unknown }) =>
  request<TableView>('POST', `/3ed/tables/${id}/debug`, action)
export const undo = (id: string) => request<TableView>('POST', `/3ed/tables/${id}/undo`, {})

// same origin as the page, http(s) -> ws(s), and `?token=` auth (the GET upgrades)
export const websocketUrl = (path: string, token: string | null): string => {
  const { protocol, hostname, port } = window.location
  const ws = protocol === 'https:' ? 'wss:' : 'ws:'
  return `${ws}//${hostname}${port ? `:${port}` : ''}/api/v1${path}?token=${encodeURIComponent(token ?? '')}`
}
export const tableSocketUrl = (id: string) => websocketUrl(`/3ed/tables/${id}`, getToken())

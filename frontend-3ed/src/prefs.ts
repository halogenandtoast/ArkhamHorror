// UI preferences kept in localStorage; storage can be unavailable, so every access is guarded
export function readPref(key: string): string | null {
  try {
    return localStorage.getItem(key)
  } catch {
    return null
  }
}

export function writePref(key: string, value: string) {
  try {
    localStorage.setItem(key, value)
  } catch {
    // private mode: the preference just doesn't persist
  }
}

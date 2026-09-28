import { reactive, toRefs, FunctionalComponent, HTMLAttributes, VNodeProps } from 'vue'
import type { ShortcutAction } from '@/arkham/keybindings'

export type MenuEntry = {
  id: string;
  icon?: FunctionalComponent<HTMLAttributes & VNodeProps, {}, any, {}>;
  content: string;
  /* A named shortcut, resolved to a key at render time so the entry follows the
   * active keybinding profile. `shortcut` is the raw-key escape hatch for keys no
   * profile remaps (Escape). */
  binding?: ShortcutAction;
  shortcut?: string;
  nested?: string;
  action: () => void;
}

const state = reactive({ menuItems: [] as MenuEntry[] })

export function useMenu() {
  const removeEntry = (id: string) => {
    state.menuItems = state.menuItems.filter((entry) => entry.id !== id)
  }
  const addEntry = (entry: MenuEntry) => {
    removeEntry(entry.id)
    state.menuItems.push(entry)
  }
  return {
    ...toRefs(state),
    addEntry,
    removeEntry
  }
}

import { reactive, toRefs, FunctionalComponent, HTMLAttributes, VNodeProps } from 'vue'
import type { ShortcutAction } from '@/arkham/keybindings'

/* Which block of the View menu an entry belongs to.
 *
 * The menu is built from whatever the board has registered, so the grouping has
 * to travel with the entry rather than live in a list in Game.vue -- a panel
 * registered by a card, a scenario or an investigator should land in the right
 * block without the bar knowing it exists.
 *
 * 'panels' is the default: something you open and read.
 */
export type MenuSection = 'panels' | 'layout' | 'preferences'

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
  section?: MenuSection;
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

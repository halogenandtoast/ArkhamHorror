import { reactive, toRefs, FunctionalComponent, HTMLAttributes, VNodeProps } from 'vue'

export type MenuEntry = {
  id: string;
  icon?: FunctionalComponent<HTMLAttributes & VNodeProps, {}, any, {}>;
  content: string;
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

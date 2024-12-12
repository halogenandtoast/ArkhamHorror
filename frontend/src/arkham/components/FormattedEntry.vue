<script lang="ts">
import { defineComponent, h } from 'vue';
import { type FlavorTextEntry, type FlavorTextModifier } from '@/arkham/types/Question';
import { formatContent } from '@/arkham/helpers';
import { useI18n } from 'vue-i18n';

function entryStyles(entry: FlavorTextEntry): { [key: string]: boolean } {
  switch (entry.tag) {
    case 'BasicEntry': return {}
    case 'I18nEntry': return {}
    case 'ModifyEntry': return entry.modifiers.map((m) => { return { [modifierToStyle(m)]: true }})
    case 'CompositeEntry': return {}
    default: return {}
  }
}

function modifierToStyle(modifier: FlavorTextModifier): string {
  switch (modifier) {
    case 'BlueEntry': return 'blue'
    case 'RightAligned': return 'right'
    case 'PlainText': return 'basic'
    case 'InvalidEntry': return 'invalid'
    case 'ValidEntry': return 'valid'
    default: throw new Error("Unknown modifier")
  }
}

function formatEntry(t, entry: FlavorTextEntry): any {
  console.log(entry)
  switch (entry.tag) {
    case 'BasicEntry': return h('div', { innerHTML: formatContent(entry.text.startsWith('$') ? t(entry.text.slice(1)) : entry.text) })
     case 'I18nEntry': return h('div', { innerHTML: formatContent(t(entry.key, entry.variables)) })
     case 'ModifyEntry': return h('div', { class: entryStyles(entry) }, [formatEntry(t, entry.entry)])
     case 'CompositeEntry': return h('div', { class: "composite" }, entry.entries.map((e) => formatEntry(t, e)))
    default: return h('div', "Unknown entry type")
  }
}

export default defineComponent({
  props: { entry: { type: Object as () => FlavorTextEntry, required: true }},
  render() {
    const { entry } = this
    const {t } = useI18n()
    return formatEntry(t, entry)
  }
})
</script>

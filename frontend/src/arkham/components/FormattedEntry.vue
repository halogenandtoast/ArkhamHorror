<script lang="ts">
import { defineComponent, h } from 'vue';
import { type FlavorTextEntry, type FlavorTextModifier } from '@/arkham/types/FlavorText';
import { baseUrl, formatContent } from '@/arkham/helpers';
import { useI18n } from 'vue-i18n';

function entryStyles(entry: FlavorTextEntry): { [key: string]: boolean } {
  switch (entry.tag) {
    case 'BasicEntry': return {}
    case 'I18nEntry': return {}
    case 'ModifyEntry': return entry.modifiers.map((m) => { return { [modifierToStyle(m)]: true }})
    case 'CompositeEntry': return {}
    case 'ListEntry': return {}
    case 'EntrySplit': return {}
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

function formatListEntry(t, entry: ListEntry): any {
  const inner = formatEntry(t, entry.entry)
  return h('li',  entry.nested.length == 0 ? inner : [inner, h('ul', entry.nested.map((e) => formatListEntry(t, e)))])
}

function formatEntry(t, entry: FlavorTextEntry): any {
  switch (entry.tag) {
    case 'BasicEntry': return h('p', { innerHTML: formatContent(entry.text.startsWith('$') ? t(entry.text.slice(1)) : entry.text) })
     case 'I18nEntry': return h('div', { innerHTML: formatContent(t(entry.key, {...entry.variables, setImgPath: `${baseUrl}/img/arkham/encounter-sets` })) })
     case 'ModifyEntry': return h('div', { class: entryStyles(entry) }, [formatEntry(t, entry.entry)])
     case 'CompositeEntry': return h('div', { class: "composite" }, entry.entries.map((e) => formatEntry(t, e)))
     case 'ListEntry': return h('ul', entry.list.map((e) => formatListEntry(t, e)))
     case 'EntrySplit': return h('hr')
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

<style scoped lang="scss">
.composite { display: contents; }

.blue, :deep(.blue), p.blue, :deep(p.blue) {
  border: 3px solid #3a4a69;
  border-radius: 55px;
  background-color: color-mix(in srgb, #3a4a69, transparent 90%);
  padding: 20px;
  position: relative;
  z-index: 0;

  &.nested {
    border: 0;
    &::after {
      filter: grayscale(1);
    }
  }

  &::after {
    border-radius: 55px;
    box-shadow: inset 0 0 15px color-mix(in srgb, #3a4a69, transparent 10%), 1px 1px 3px color-mix(in srgb, #3a4a69, transparent 30%);
    content: "";
    position: absolute;
    inset: 0;
    z-index: 1;
  }

  > p:first-child {
    margin-left: 35px;
  }
}

.right, :deep(.right) {
  text-align: right !important;
  align-self: end !important;
}

.basic, :deep(.basic), p.basic, :deep(p.basic) {
  font-style: normal !important;
  font-family: auto !important;
}

:deep(b), :deep(i), :deep(strong) {
  display: contents;
}

:deep(b) { font-family: none; }
:deep(i) { font-style: italic; }

p, :deep(p) {
  font-family: "ArkhamFlavor";
  margin: 10px;
}

:deep(strong), :deep(b), b, strong {
  font-weight: bolder !important;
  font-style: normal !important;
  font-family: auto !important;
}

:deep(.checkpoint), :deep(.interlude) {
  ul {
    margin-inline: 20px;
  }
}

.invalid, :deep(.invalid) {
  &:not(li) {
    display: inline-flex;
  }
  align-items: center;
  color: #666;
  &::before {
    content: '';
    display: inline-block;
    width: 20px;
    min-width: 20px;
    height: 20px;
    margin-right: 8px; /* Adjust spacing between the circle and the element */
    border-radius: 50%;
    background-size: 12px 12px; /* Adjust size of the X and checkmark */
    background-position: center;
    background-repeat: no-repeat;
    background-color: var(--survivor-dark);
    background-image: url('data:image/svg+xml,%3Csvg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" fill="white"%3E%3Cpath d="M12 10.586l4.95-4.95 1.414 1.414-4.95 4.95 4.95 4.95-1.414 1.414-4.95-4.95-4.95 4.95-1.414-1.414 4.95-4.95-4.95-4.95L7.05 5.636l4.95 4.95z"/%3E%3C/svg%3E');
  }
}

.valid, :deep(.valid) {
  &:not(li) {
    display: inline-flex;
  }
  align-items: center;
  &::before {
    content: '';
    display: inline-block;
    width: 20px;
    min-width: 20px;
    height: 20px;
    margin-right: 8px; /* Adjust spacing between the circle and the element */
    border-radius: 50%;
    background-size: 12px 12px; /* Adjust size of the X and checkmark */
    background-position: center;
    background-repeat: no-repeat;
    background-color: green;
    background-image: url('data:image/svg+xml,%3Csvg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" fill="white"%3E%3Cpath d="M9 19l-6-6 1.414-1.414L9 16.172l10.586-10.586L21 7.586z"/%3E%3C/svg%3E');
  }
}

ul, :deep(ul) {
  list-style-type: "\0059";
  margin-inline: 10px;
  li {
    padding-left: 10px;
    margin-left: 10px;

    ul {
      margin-block: 10px;
      list-style-type: "\e91a";
      li::marker {
        font-family: "ArkhamIcons";
      }
    }
  }

  li::marker {
    font-family: "ArkhamSlim";
    color: var(--spooky-green-dark);
  }
}

.resolution, :deep(.resolution) {
  li::marker {
    color: var(--bullet-red);
  }
}

.checkpoint, :deep(.checkpoint), .interlude, :deep(.interlude) {
  li::marker {
    color: #19214F;
  }

  h1 {
    color: #2F3863;
    text-align: center;
    font-size: 1.2em;
    border-bottom: 1px solid #2F3863;
    &::after {
      border-bottom: 1px solid #2F3863;
    }
  }
}

:deep(h1) {
  font-family: "Teutonic";
  font-weight: 500;
  color: #38615F;
  margin: 0 0 10px 0;
  padding-bottom: 2px;
  border-bottom: 1px solid #38615f;
  &::after {
    display: block;
    content: " ";
    margin-top: 2px;
    border-bottom: 1px solid #38615f;
  }
}

:deep(header) {
  font-family: "Teutonic";
  font-weight: 500;
  font-size: 1.5em;
  color: #222;
  margin: 0 0 10px 0;
  padding-bottom: 2px;
}


:deep(.encounter-sets) {
  display: flex;
  gap: 5px;
  margin-block: 10px;
  img {
    width: 40px;
  }
}

:deep(hr) {
  border:0;
  border-bottom: 2px solid #60759F;
  margin-inline: -20px;
  margin-block: 10px;
}
</style>

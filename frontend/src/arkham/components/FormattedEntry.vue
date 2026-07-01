<script lang="ts">
import { defineComponent, h } from 'vue'
import { type FlavorTextEntry, type FlavorTextModifier, type ImageModifier, type ListItemEntry } from '@/arkham/types/FlavorText'
import { baseUrl, formatContent, imgsrc } from '@/arkham/helpers'
import { cardImage } from '@/arkham/cardImages'
import { type ComposerTranslation, useI18n } from 'vue-i18n'
import { tarotArcanaImage } from '@/arkham/types/TarotCard'
import { chaosTokenImage } from '@/arkham/types/ChaosToken'
import CodexEntry from '@/arkham/components/CodexEntry.vue'
import ChaosTokenMorph from '@/arkham/components/ChaosTokenMorph.vue'

function entryStyles(entry: FlavorTextEntry): { [key: string]: boolean } {
  switch (entry.tag) {
    case 'BasicEntry': return {}
    case 'I18nEntry': return {}
    case 'ModifyEntry': return entry.modifiers.reduce((acc, m) => { return { [modifierToStyle(m)]: true, ...acc }}, {})
    case 'CompositeEntry': return {}
    case 'ColumnEntry': return {}
    case 'ListEntry': return {}
    case 'EntrySplit': return {}
    case 'HeaderEntry': return {}
    case 'TarotEntry': return {"card": true, "no-overlay": true}
    case 'ChaosTokenEntry': return {"chaos-token": true}
    case 'CardEntry': {
      const mods = entry.imageModifiers.reduce((acc, m) => { return { [imageModifierToStyle(m)]: true, ...acc }}, {})
      return {"card": true, "no-overlay": true, ...mods}
    }

    default: return {}
  }
}

function imageModifierToStyle(modifier: ImageModifier): string {
  switch (modifier) {
    case 'RemoveImage': return 'remove'
    case 'SelectImage': return 'select'
    default: throw new Error("Unknown modifier")
  }
}

function modifierToStyle(modifier: FlavorTextModifier): string {
  switch (modifier) {
    case 'BlueEntry': return 'blue'
    case 'GreenEntry': return 'green'
    case 'CodexEntry': return 'codex'
    case 'RedEntry': return 'red'
    case 'BorderedEntry': return 'bordered'
    case 'NestedEntry': return 'nested'
    case 'ResolutionEntry': return 'resolution'
    case 'CheckpointEntry': return 'checkpoint'
    case 'InterludeEntry': return 'interlude'
    case 'HauntedEntry': return 'haunted'
    case 'RightAligned': return 'right'
    case 'CenteredEntry': return 'center'
    case 'NoUnderline': return 'no-underline'
    case 'PlainText': return 'basic'
    case 'InvalidEntry': return 'invalid'
    case 'ValidEntry': return 'valid'
    default: throw new Error("Unknown modifier")
  }
}

function formatListEntry(t: ComposerTranslation, entry: ListItemEntry): ReturnType<typeof h> {
  const inner = formatEntry(t, entry.entry)
  return h('li',  entry.nested.length == 0 ? inner : [inner, h('ul', entry.nested.map((e: ListItemEntry) => formatListEntry(t, e)))])
}

function formatEntry(t: ComposerTranslation, entry: FlavorTextEntry, classes: { [key: string]: boolean } = {}): ReturnType<typeof h> {
  switch (entry.tag) {
    case 'BasicEntry': return h('p', { innerHTML: formatContent(entry.text.startsWith('$') ? t(entry.text.slice(1)) : entry.text) })
    case 'HeaderEntry': if (entry.level == 1) {
        return h('header', [h('h1', { class: classes, innerHTML: formatContent(t(entry.key)) })])
      } else {
        return h('h3', { class: classes, innerHTML: formatContent(t(entry.key)) })
      }
    case 'I18nEntry': return h('div', { innerHTML: formatContent(t(entry.key, {...entry.variables, setImgPath: `${baseUrl}/img/arkham/encounter-sets` })) })
    case 'ModifyEntry': {
      const styles = entryStyles(entry)
      if (styles.codex) {
        return h(CodexEntry, null, { default: () => [formatEntry(t, entry.entry)] })
      }
      // HeaderEntry is handled specially to avoid wrapping it in a div
      if (entry.entry.tag === 'HeaderEntry') return formatEntry(t, entry.entry, styles)
      return h('div', { class: styles }, [formatEntry(t, entry.entry)])
    }
    case 'CompositeEntry': return h('div', { class: "composite" }, entry.entries.map((e) => formatEntry(t, e)))
    case 'ColumnEntry': return h('div', { class: "columns" }, entry.entries.map((e) => formatEntry(t, e)))
    case 'ListEntry': return h('ul', entry.list.map((e) => formatListEntry(t, e)))
    case 'CardEntry': return h('div', [h('img', { class: entryStyles(entry), src: cardImage(entry.cardCode)})])
    case 'TarotEntry': return h('div', [h('img', { class: entryStyles(entry), src: imgsrc(`tarot/${tarotArcanaImage(entry.tarot)}`)})])
    case 'ChaosTokenEntry': return h('div', [h('img', { class: entryStyles(entry), src: chaosTokenImage(entry.chaosTokenFace)})])
    case 'ChaosTokenMorphEntry': return h(ChaosTokenMorph, { from: entry.morphFrom, to: entry.morphTo })
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
  },
  data() {
    return { green_fleur: `url(${imgsrc('green_fleur.png')})` }
  }
})
</script>

<style scoped>
.composite { display: contents; }

.chaos-token, :deep(.chaos-token) {
  display: block;
  width: 140px;
  height: 140px;
  margin: 0 auto;
  object-fit: contain;
}
.columns, :deep(.columns) {
  display: flex;
  flex-direction: row;
  justify-content: space-between;
  gap: 10px;
  > * {
    display: flex;
    flex-direction: column;
    flex: 1 1 auto;
    padding: 10px 20px;
    z-index: var(--z-index-5);
  }

  .composite {
    position: relative;
  }

  .composite:not(:first-child)::after {
    position: absolute;
    inset: 0;
    top: 10px;
    bottom: 10px;
    height: calc(100% - 20px);
    content: '';
    border-left: 1px solid black;
  }

  /* When a column contains a chaos token, drop the divider line and
     center the token + paragraph as a stacked block. */
  .composite:has(.chaos-token) {
    justify-content: center;
    align-items: center;
    gap: 16px;
    padding: 20px 24px;

    &::after {
      content: none;
    }

    > div {
      width: 100%;
      display: flex;
      justify-content: center;
    }

    p, :deep(p) {
      margin: 0;
      text-align: center;
    }
  }

  &.simple {
    justify-content: flex-start;
    gap: 20px;
    p, section {
      margin: 0;
      padding: 0;
    }
  }

  > div:has(img) {
    max-width: 300px;
    min-width: 25%;
    padding: 0;
    .card {
      border-radius: 10px;
    }
  }

  .invalid {
    flex-direction: row;
    height: fit-content;
  }
}

.blue, :deep(.blue), p.blue, :deep(p.blue) {
  --embed-color: #00264D;
  border: 3px solid #3a4a69;
  border-radius: 55px;
  background-color: color-mix(in srgb, #3a4a69, transparent 90%);
  padding: 20px;
  position: relative;
  z-index: var(--z-index-0);

  &.nested {
    border: 0;
    &::after {
      filter: grayscale(1);
    }
  }

  &::before {
    border-radius: 52px;
    box-shadow: inset 0 0 15px color-mix(in srgb, #3a4a69, transparent 10%), 1px 1px 3px color-mix(in srgb, #3a4a69, transparent 30%);
    content: "";
    position: absolute;
    inset: 0;
    z-index: var(--z-index-neg-1);
  }

  > p:first-child {
    margin-left: 35px;
  }

  h1 {
    color: #3a4a69;
    text-align: center;
    border-bottom: 0;
    &::after {
      border-bottom: 0;
    }
  }
}

.green, :deep(.green), p.green, :deep(p.green) {
  --color: #213C35;
  --border-color: var(--color);
  min-height: 6em;
  place-content: center;
  border: 3px solid var(--color);
  border-radius: 55px;
  background-color: color-mix(in srgb, var(--color), transparent 90%);
  padding: 20px;
  position: relative;
  z-index: var(--z-index-0);

  &:has(.composite > :nth-child(2)) {
    display: flex;
    flex-direction: column;
    gap: 10px;
  }

  &.nested {
    border: 0;
    &::after {
      filter: grayscale(1);
    }
  }

  &::after {
    border-radius: 52px;
    box-shadow: inset 0 0 15px color-mix(in srgb, var(--color), transparent 10%), 1px 1px 3px color-mix(in srgb, var(--color), transparent 30%);
    content: "";
    position: absolute;
    inset: 0;
    z-index: var(--z-index-1);
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

.censor, :deep(.censor) {
  background-color: black;
  color: black;
  padding: 0 4px;
  border-radius: 2px;
}

:deep(b), :deep(i), :deep(strong) {
  display: contents;
}

:deep(p.cursive) {
  font-family: "ArkhamCursive";
  display: block;
  font-size: 1.3em;
  line-height: 1.3em;
  padding-bottom: 1.3em;
}

:deep(span.wolgast) {
  font-family: "Wolgast";
}

:deep(div.wolgast) {
  font-family: "Wolgast";
  text-align: center;
  margin-block: 30px;
  p {
    margin-block: -20px;
    font-family: "Wolgast";
  }
}

:deep(b) { font-family: none; }
:deep(i) { font-style: italic; }

:deep([data-image-id]) {
  color: var(--embed-color, black);
  display: inline-flex;
  gap: 2px;
  &:after {
    font-family: "Materials";
    content: "\e3f4";
    align-self: center;
  }
}

p, :deep(p) {
  font-family: "ArkhamFlavor";
  margin: 10px;
}

p.anke, :deep(p.anke) {
  font-family: "Anke";
  font-weight: 500;
  margin: 10px;
}

p.typewriter, :deep(p.typewriter) {
  font-family: "Typewriter";
  font-weight: 600;
  margin: 10px;
  text-indent: 50px;
  &.center {
    text-indent: unset;
  }
}

p.accountant, :deep(p.accountant) {
  font-family: "Accountant";
  font-weight: 800;
  margin: 10px;
  font-size: 1.2em;
  text-indent: 50px;
}

p.indented, :deep(p.indented) {
  margin-inline: 50px;
}

div:has(p.unindented), :deep(div:has(p.unindented)) {
  padding-inline: 0px;
}

p.billenia, :deep(p.billenia) {
  font-family: "Billenia";
  font-weight: 500;
  font-size: 1.4em;
  font-style: normal;
  margin: 10px;
}

:deep(strong), :deep(b), b, strong {
  font-weight: bolder !important;
  font-style: normal !important;
  font-family: auto !important;
}

.term, :deep(.term) {
  font-weight: bold;
  font-family: "Arno";
  text-transform: uppercase;
  font-size: 0.8em;
  display: inline-block;
  color: black;
  &::first-letter {
    font-size: 1.2em;
  }
}

.intro-text {
  div, :deep(div) {
    &:has(.note-green) {
      min-height: fit-content;
      margin-block: 20px;
      margin-inline: 50px;
      box-shadow: unset;
      overflow: hidden;
      padding: 50px;
      position: relative;
      &::after {
        position: absolute;
        inset: 0px;
        box-sizing: border-box;
        content: "";
        filter: blur(0.25em);
        margin: 20px;
        background-color: #E1E4DF;
        mix-blend-mode: multiply;
      }
      &::before {
        z-index: var(--z-index-2);
        pointer-events: none;
        position: absolute;
        inset: 10px;
        border-image-source: v-bind(green_fleur);
        border-image-slice: 49.9%;
        border-image-repeat: no-repeat;
        border-image-width: 50px;
        content: "";
      }
    }
  }
}

:deep(.checkpoint), :deep(.interlude) {
  ul {
    margin-inline: 20px;
  }
}

.invalid, :deep(.invalid) {
  align-items: center;
  color: #666;
  width: 100%;
  > div {
    width: 100%;
  }
  &:not(li) {
    display: inline-flex;
    &:has(.right) {
      justify-content: flex-end;
    }
  }
  &:not(.right)::before {
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

  &:has(> .composite),
  &:has(> ul) {
    display: grid;
    grid-template-columns: auto minmax(0, 1fr);
    align-items: start;
  }

  &:has(> .composite)::before,
  &:has(> ul)::before {
    margin-top: 10px;
  }

  > .composite {
    display: block;
  }

  &.right::after {
    content: '';
    display: inline-block;
    width: 20px;
    min-width: 20px;
    height: 20px;
    margin-left: 8px; /* Adjust spacing between the circle and the element */
    border-radius: 50%;
    background-size: 12px 12px; /* Adjust size of the X and checkmark */
    background-position: center;
    background-repeat: no-repeat;
    background-color: var(--survivor-dark);
    background-image: url('data:image/svg+xml,%3Csvg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" fill="white"%3E%3Cpath d="M12 10.586l4.95-4.95 1.414 1.414-4.95 4.95 4.95 4.95-1.414 1.414-4.95-4.95-4.95 4.95-1.414-1.414 4.95-4.95-4.95-4.95L7.05 5.636l4.95 4.95z"/%3E%3C/svg%3E');
  }
}

li:has(> .invalid) > ul,
:deep(li:has(> .invalid) > ul) {
  color: #666;
}

li:has(> .invalid) > ul .valid,
:deep(li:has(> .invalid) > ul .valid) {
  color: var(--neutral-extra-dark);
}

h3, :deep(h3) {
  font-size: 1.3em;
  font-weight: bold;
  text-decoration: underline;
  justify-self: center;
}

.valid, :deep(.valid) {
  width: 100%;
  > div {
    width: 100%;
  }
  align-items: center;
  &:not(li) {
    display: inline-flex;
    &:has(.right) {
      justify-content: flex-end;
    }
  }
  &:not(.right)::before {
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

  &:has(> .composite),
  &:has(> ul) {
    display: grid;
    grid-template-columns: auto minmax(0, 1fr);
    align-items: start;
  }

  &:has(> .composite)::before,
  &:has(> ul)::before {
    margin-top: 10px;
  }

  > .composite {
    display: block;
  }

  &.right::after {
    content: '';
    display: inline-block;
    width: 20px;
    min-width: 20px;
    height: 20px;
    margin-left: 8px; /* Adjust spacing between the circle and the element */
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
    margin-bottom: 5px;

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

:deep(.columns), .columns {
  &:is(:first-child) {
    padding: 0 !important;
  }
  &:has(.scarlet-keys-vote) {
    background: var(--title);
    padding: 0;
    text-align: center;
    gap: 0;
    border-bottom: 2px solid var(--neutral-dark);

    > .composite {
      padding: 0;
      &::after {
        border: 0 !important;
      }
      &:has(.nay) {
        background-color: darkseagreen;
      }

      &:has(.yea) {
        background-color: lightcoral;
        border-left: 2px solid var(--neutral-dark);
      }
    }
    h3 {
      text-align: center;
      width: 100%;
      margin: 0;
      padding: 0;
      padding-top: 10px;
    }
  }
}

.red, :deep(.red) {
  --color: #F4DFD1;
  &:not(.bordered) {
    position: relative;
    margin: 20px;
    isolation: isolate;
    mix-blend-mode: multiply;
    &::before {
      z-index: var(--z-index-neg-1);
      content: '';
      position: absolute;
      inset: -10px;
      background: var(--color);
      filter: blur(5px);
    }
  }
  &.bordered {
    position: relative;
    mix-blend-mode: multiply;
    background: var(--color);
    padding: 10px;
    border: 2px solid black;
    border-radius: 55px;
    margin-inline: 10px;
    > p:first-of-type {
      text-indent: 1.5em;
    }

    .composite > *:nth-child(1) {
      text-indent: 1.5em;
    }

    hr {
      border-bottom: 2px solid black !important;
      margin-inline: -10px;
    }

    &.bordered::before {
      border-radius: 55px;
      content: "";
      position: absolute;
      inset: 0;
      z-index: var(--z-index-3);
      border: 3px solid black;
      filter: blur(5px);
    }
  }
}

.resolution, :deep(.resolution) {
  overflow-y: auto;
  padding: 40px;
  li::marker {
    color: var(--bullet-red);
  }
  ul {
    margin-top: 10px;
  }
}

.checkpoint, :deep(.checkpoint), .interlude, :deep(.interlude) {
  li::marker {
    color: #19214F;
  }

  h1 {
    color: #2F3863;
    text-align: center;
    font-size: 2em;
    border-bottom: 1px solid #2F3863;
    &::after {
      border-bottom: 1px solid #2F3863;
    }
  }
}

.haunted, :deep(.haunted) {
  color: #fafbe8;

  p, :deep(p) {
    color: #fcfdef;
    text-shadow:
      0 1px 2px rgba(0, 0, 0, 1),
      0 0 8px rgba(0, 0, 0, 0.9),
      0 0 18px rgba(0, 0, 0, 0.65);
    font-style: italic;
    font-weight: 500;
  }

  .chaos-token, :deep(.chaos-token) {
    border-radius: 50%;
    filter:
      drop-shadow(0 0 1px rgba(0, 0, 0, 1))
      drop-shadow(0 0 2px rgba(0, 0, 0, 0.95))
      drop-shadow(0 2px 4px rgba(0, 0, 0, 0.85))
      drop-shadow(0 0 10px rgba(208, 215, 100, 0.95))
      drop-shadow(0 0 22px rgba(180, 188, 75, 0.85))
      drop-shadow(0 0 50px rgba(131, 137, 56, 0.6))
      drop-shadow(0 0 90px rgba(131, 137, 56, 0.35));
    animation: haunted-token-pulse 3.2s ease-in-out infinite;
  }

  .card, :deep(.card), img.card, :deep(img.card) {
    filter:
      brightness(0.7) contrast(1.15) saturate(0.6)
      drop-shadow(0 0 18px rgba(0, 0, 0, 0.95))
      drop-shadow(0 0 30px rgba(66, 69, 28, 0.5));
    transition: filter 220ms ease;
  }

  .columns, :deep(.columns) {
    justify-content: space-evenly;
    gap: 0;

    > * {
      flex: 0 1 auto;
      padding: 10px 8px;
    }

    .composite:has(.chaos-token), :deep(.composite:has(.chaos-token)) {
      gap: 56px;
    }

    .composite::after {
      border-left-color: rgba(131, 137, 56, 0.3) !important;
    }
  }
}

@keyframes haunted-token-pulse {
  0%, 100% {
    filter:
      drop-shadow(0 0 1px rgba(0, 0, 0, 1))
      drop-shadow(0 0 2px rgba(0, 0, 0, 0.95))
      drop-shadow(0 2px 4px rgba(0, 0, 0, 0.85))
      drop-shadow(0 0 10px rgba(208, 215, 100, 0.95))
      drop-shadow(0 0 22px rgba(180, 188, 75, 0.85))
      drop-shadow(0 0 50px rgba(131, 137, 56, 0.6))
      drop-shadow(0 0 90px rgba(131, 137, 56, 0.35));
    transform: scale(1);
  }
  50% {
    filter:
      drop-shadow(0 0 1px rgba(0, 0, 0, 1))
      drop-shadow(0 0 2px rgba(0, 0, 0, 0.95))
      drop-shadow(0 3px 6px rgba(0, 0, 0, 0.9))
      drop-shadow(0 0 16px rgba(230, 235, 130, 1))
      drop-shadow(0 0 34px rgba(208, 215, 100, 1))
      drop-shadow(0 0 70px rgba(180, 188, 75, 0.85))
      drop-shadow(0 0 120px rgba(131, 137, 56, 0.55));
    transform: scale(1.05);
  }
}

.note-green, :deep(.note-green) {
  h1 {
    text-align: center;
    width: fit-content;
    justify-self: center;
    font-size: 1.8em;
    border-bottom: 1px solid var(--green-title);
    &::after {
      border-bottom: 1px solid var(--green-title);
    }
  }
}

:deep(h3) {
  font-family: "Teutonic";
  font-weight: 800;
  text-decoration: none;
  text-align: left;
  align-self: start;
  justify-self: start;
  justify-content: start;
  margin: 0;
}

:deep(h1) {
  font-family: "Teutonic";
  font-weight: 500;
  color: var(--green-title);
  padding-bottom: 2px;
  &:not(.no-underline) {
    border-bottom: 1px solid var(--green-title);
    &::after {
      border-bottom: 1px solid var(--green-title);
    }
  }
  font-size: 2em;
  &::after {
    display: block;
    content: " ";
    margin-top: 2px;
  }
}

:deep(header) {
  font-family: "Teutonic";
  font-weight: 500;
  font-size: 1.5em;
  color: var(--neutral-extra-dark);
}

:deep(.set-icon-font) {
  display: inline-block;
  font-family: "ArkhamEncounters";
  font-style: inherit;
  font-size: inherit;
  line-height: inherit;
  color: inherit;
  background-color: transparent;
}

:deep(.encounter-sets) {
  display: flex;
  gap: 10%;
  margin-block: 10px;
  justify-content: center;
  flex-wrap: wrap;
  img {
    align-self: center;
    width: 40px;
  }
  span {
    text-align: left;
  }

  &.grid {
    display: grid;
    grid-template-columns: repeat(3, 1fr);
    gap: 10px 20px;
    justify-content: stretch;
    > div {
      display: flex;
      flex-direction: row;
      align-items: center;
      gap: 10px;
      img {
        width: 36px;
        flex-shrink: 0;
      }
      span {
        font-weight: bold;
      }
    }
  }
}

/* example card images for the additional rules sections, mimicking the
   rulebook layout: the two portrait cards (enemy/treachery) sit side by side
   on top, with the landscape agenda centered below and slightly larger.
   Natural aspect ratios are preserved so the landscape agenda is not cropped.
   Faction-colored arrows (set via the --faction custom property) point at each
   card, emulating the call-outs printed in the rulebook. */
:deep(.card-examples) {
  display: flex;
  flex-direction: column;
  align-items: center;
  gap: 10px;
  margin: 12px 0;
  .top {
    display: flex;
    flex-wrap: wrap;
    gap: 10px;
    justify-content: center;
  }
  .ex {
    position: relative;
    display: inline-block;
    line-height: 0;
  }
  img {
    width: 120px;
    height: auto;
    border-radius: 8px;
    box-shadow: 0 6px 14px rgba(0, 0, 0, 0.3);
  }
  img.agenda {
    width: 170px;
  }
  /* arrows overlay the card edge, tip pointing onto the card; --at sets the
     vertical position to roughly match the rulebook call-out for each card.
     The SVG (faction-colored, with its own white border + drop shadow) points
     left; the base .arrow flips it to point right, .arrow.left keeps it left. */
  .arrow {
    position: absolute;
    top: var(--at, 30%);
    left: var(--left, -25px);
    transform: translateY(-50%) scaleX(-1);
    width: 36px;
    height: auto;
    border-radius: 0;
    box-shadow: none;
    z-index: var(--z-index-5);
  }
  .arrow.left {
    left: auto;
    right: var(--right, -30px);
    transform: translateY(-50%);
  }
  .arrow.top {
    left: var(--left, 50%);
    top: -15px;
    right: var(--right, -30px);
    transform: rotate(var(--angle, -90deg));
  }
}

/* "Place around this location" example: a location card with clue tokens
   physically bordering it (around the edges, not placed on the card).
   Clue size = 1/6 of the card height, so exactly 6 clues run along the card's
   right edge; the two corner clues extend one clue beyond the card, making the
   right column 8 total. The left/right columns own the corners, so the
   top/bottom rows sit inset between them to avoid corner overlap. */
:deep(.place-around) {
  --card-w: 180px;
  --clue: calc(var(--card-w) * 1.4 / 6);
  position: relative;
  width: var(--card-w);
  margin: calc(var(--clue) + 8px) auto;
  > img {
    width: 100%;
    display: block;
    border-radius: 8px;
    box-shadow: 0 6px 14px rgba(0, 0, 0, 0.3);
  }
  .clues {
    position: absolute;
    display: flex;
  }
  .clues img {
    width: var(--clue);
    height: var(--clue);
    border-radius: 0;
    box-shadow: none;
    transform: rotate(90deg);
    filter: drop-shadow(1px 1px 2px rgba(0, 0, 0, 0.6));
  }
  .clues.left,
  .clues.right {
    flex-direction: column;
  }
  /* right column extends into both corners (level with the top/bottom rows),
     so its top and bottom clues sit in the corners with 6 along the side */
  .clues.right {
    right: calc(-1 * var(--clue));
    top: calc(-1 * var(--clue));
    height: calc(100% + 2 * var(--clue));
    justify-content: space-between;
  }
  /* left column: 2 clues at the bottom, the lowest sitting in the corner
     (level with the bottom row) */
  .clues.left {
    left: calc(-1 * var(--clue));
    top: 0;
    height: calc(100% + var(--clue));
    justify-content: flex-end;
  }
  /* rows: full width, inset from the corners owned by the columns */
  .clues.top,
  .clues.bottom {
    left: 0;
    width: 100%;
    flex-direction: row;
    justify-content: space-evenly;
  }
  .clues.top {
    top: calc(-1 * var(--clue));
    justify-content: flex-end;
  }
  .clues.bottom {
    bottom: calc(-1 * var(--clue));
  }
}

/* "Warring" worked example — three panels mirroring the rulebook diagram:
   setup, hunter move, and resolve attacks. */
:deep(.warring-example) {
  --we-card: 84px;
  --we-h: calc(var(--we-card) * 1.4);
  margin: 12px 0 4px;
}
:deep(.warring-example) .we-cap {
  margin: 16px 0 10px;
}
:deep(.warring-example) .we-title {
  margin: 8px 0;
}
:deep(.warring-example) img {
  border-radius: 5px;
  box-shadow: 0 3px 8px rgba(0, 0, 0, 0.35);
}
:deep(.warring-example) .faded {
  opacity: 0.4;
}
:deep(.warring-example) .we-panel {
  position: relative;
}
:deep(.warring-example) .we-arrow {
  position: absolute;
  overflow: visible;
  pointer-events: none;
  z-index: 5;
}

/* Panel 1 — setup: three location + enemy groups, centered with a gap */
:deep(.warring-example) .we-setup {
  display: flex;
  justify-content: center;
  gap: 72px;
  padding-bottom: 52px;
}
:deep(.warring-example) .we-spot {
  position: relative;
  flex: none;
  width: var(--we-card);
}
:deep(.warring-example) .we-spot > .loc {
  display: block;
  width: 100%;
}
:deep(.warring-example) .we-spot > .enemy {
  position: absolute;
  width: 100%;
  left: -40%;
  top: 30%;
  z-index: 2;
}

/* Panel 2 — hunter move: same centered layout as the setup panel. The side
   locations keep faded "origin" copies of the enemies that moved; the middle
   location gathers all three. */
:deep(.warring-example) .we-move {
  display: flex;
  justify-content: center;
  gap: 72px;
  padding-bottom: 52px;
  width: fit-content;
  margin-inline: auto;
  margin-bottom: 28px;
}
/* the left move arrow: from the bottom of the left enemy to the lower middle
   of the same (red) enemy gathered on the middle location */
:deep(.warring-example) .we-mv-left {
  left: 10px;
  bottom: -19px;
  width: 150px;
  height: 40px;
  box-shadow: none;
  border-radius: 0;
}
/* mirror of the left move arrow: from the right enemy to the middle (green) */
:deep(.warring-example) .we-mv-right {
  right: 10px;
  bottom: -19px;
  width: 150px;
  height: 40px;
  box-shadow: none;
  border-radius: 0;
}
:deep(.warring-example) .we-move .enemy.red-faction {
  left: -40%;
  right: auto;
  top: 30%;
}
:deep(.warring-example) .we-move .enemy.green-faction {
  left: auto;
  right: -40%;
  top: 30%;
}
:deep(.warring-example) .we-move .enemy.blue-faction {
  left: 0;
  right: auto;
  top: 25%;
  z-index: 1;
}
/* the three enemies gathered on the middle location sit lower and further out
   than the side "origin" copies */
:deep(.warring-example) .we-move .gathered .enemy.red-faction {
  left: -60%;
  top: 50%;
}
:deep(.warring-example) .we-move .gathered .enemy.green-faction {
  right: -60%;
  top: 50%;
}

/* Panel 3 — resolve attacks: three enemies in a row */
:deep(.warring-example) .we-attack {
  display: flex;
  justify-content: center;
  gap: 16px;
  padding: 8px 0 12px;
  width: fit-content;
  margin-inline: auto;
}
/* attack arrows: top = Herald -> Disciple, bottom = Zealot -> Disciple */
:deep(.warring-example) .we-atk-top {
  left: 44px;
  top: -10px;
  width: 72px;
  height: auto;
  box-shadow: none;
  border-radius: 0;
}
:deep(.warring-example) .we-atk-bot {
  left: 72px;
  bottom: -24px;
  width: 150px;
  height: auto;
  box-shadow: none;
  border-radius: 0;
}
:deep(.warring-example) .we-attack .enemy {
  position: relative;
  flex: none;
  width: var(--we-card);
}
/* the outer two enemies (attackers' target and the green attacker) sit lower
   than the middle one */
:deep(.warring-example) .we-attack .enemy:nth-child(1),
:deep(.warring-example) .we-attack .enemy:nth-child(3) {
  transform: translateY(25%);
}
:deep(.warring-example) .we-attack .enemy > img {
  display: block;
  width: 100%;
}
:deep(.warring-example) .we-attack .dmg {
  position: absolute;
  /* width: 15px !important;*/
  width: calc(var(--we-card) * 0.2) !important;
  height: auto;
  border-radius: 0;
  box-shadow: none;
  filter: drop-shadow(0 1px 2px rgba(0, 0, 0, 0.85));
  z-index: 3;
}
:deep(.warring-example) .we-attack .dmg.d1 { top: -8%; left: 40%; }
:deep(.warring-example) .we-attack .dmg.d2 { top: 17%; left: 43%; }
:deep(.warring-example) .we-attack .dmg.d3 { top: 27%; left: 79%; }
:deep(.warring-example) .we-attack .dmg.d4 { top: 66%; left: 60%; }

:deep(hr) {
  border:0;
  border-bottom: 2px solid var(--border-color, #60759F);
  margin-inline: -20px;
  margin-block: 10px;
}

img.remove {
  filter: brightness(81%) saturate(113%);
}

div:has(> img.remove) {
  position: relative;
  &::before {
    z-index: var(--z-index-1);
    content: "";
    display: block;
    inset: 0;
    position: absolute;
    pointer-events: none;
    background: rgba(253, 62, 65, 0.31);
    border-radius: 10px;
  }
}

.center, :deep(.center) {
  justify-content: center;
  align-content: center;
  text-align: center;
}

.task, :deep(.task) {
  display: flex;
  flex-direction: row;
  justify-content: space-between;
  gap: 2em;
  margin: 0;
  align-items: center;
  .tally {
    padding: 5px 10px;
    border: 2px solid black;
    border-top-left-radius: 255px 15px;
    border-top-right-radius: 15px 225px;
    border-bottom-right-radius: 225px 15px;
    border-bottom-left-radius:15px 255px;
  }
}

/* each image behaves like a card */
:deep(.fan) {
  counter-reset: cards;
  --n: 2;
  /* tweak these to taste */
  --card-w: 120px;        /* card width  */
  --spread: 14deg;        /* total rotation step between neighbors */
  --shift: 56px;         /* horizontal overlap (negative = overlap) */
  --lift: 0px;            /* base vertical lift */
  --hover-lift: -18px;    /* how high a hovered card rises */

  position: relative;
  height: calc(var(--card-w) * 1.4);   /* room for the arc */
  display: block;
  > img{
    counter-increment: cards;
    position: absolute;
    bottom: 0; left: 50%;
    width: var(--card-w);
    aspect-ratio: 63 / 88;               /* poker-ish ratio; remove if you prefer */
    object-fit: cover;
    border-radius: 10px;
    box-shadow: 0 10px 22px rgba(0,0,0,.25);
    transform-origin: 50% 90%;           /* pivot near bottom-center */
    transition: transform .18s ease, box-shadow .18s ease;
    will-change: transform;
    /* place, rotate, and overlap based on index (set below) */
    transform:
      translateX(calc(-50% + (var(--i) - (var(--n) - 1)/2) * var(--shift)))
      rotate(calc((var(--i) - (var(--n) - 1)/2) * var(--spread)))
      translateY(var(--lift));
    z-index: calc(var(--i) + var(--z-index-1));
  }

  /* lift the hovered card and bring it to the top */
  > img:hover{
    transform:
      translateX(calc(-50% + (var(--i) - (var(--n) - 1)/2) * var(--shift)))
      rotate(calc((var(--i) - (var(--n) - 1)/2) * var(--spread)))
      translateY(var(--hover-lift));
    box-shadow: 0 16px 30px rgba(0,0,0,.35);
    z-index: var(--z-index-999);
  }

  /* ---- small utility: assign index (--i) with :nth-child rules ---- */
  /* also writes --n = number of cards seen so far; update the last rule if you show more */
  /* add more nth-child lines if you might have more cards */
  > :nth-child(1){ --i: 0 }
  > :nth-child(2){ --i: 1 }
  > :nth-child(3){ --i: 2 }
  > :nth-child(4){ --i: 3 }
  > :nth-child(5){ --i: 4 }
}

/* optional: tighten the spread on narrow screens */
@media (max-width: 600px){
  :deep(.fan) {
    --card-w: 140px;
    --spread: 11deg;
    --shift: -44px;
  }
}

/* each image behaves like a card */
:deep(.fan-thin) {
  counter-reset: cards;
  --n: 2;
  /* tweak these to taste */
  --card-w: 120px;        /* card width  */
  --spread: 24deg;        /* total rotation step between neighbors */
  --shift: 16px;         /* horizontal overlap (negative = overlap) */
  --lift: 0px;            /* base vertical lift */
  --hover-lift: -18px;    /* how high a hovered card rises */

  position: relative;
  height: calc(var(--card-w) * 1.4);   /* room for the arc */
  display: block;
  > img{
    counter-increment: cards;
    position: absolute;
    bottom: 0; left: 50%;
    width: var(--card-w);
    aspect-ratio: 63 / 88;               /* poker-ish ratio; remove if you prefer */
    object-fit: cover;
    border-radius: 10px;
    box-shadow: 0 10px 22px rgba(0,0,0,.25);
    transform-origin: 50% 90%;           /* pivot near bottom-center */
    transition: transform .18s ease, box-shadow .18s ease;
    will-change: transform;
    /* place, rotate, and overlap based on index (set below) */
    transform:
      translateX(calc(-50% + (var(--i) - (var(--n) - 1)/2) * var(--shift)))
      rotate(calc((var(--i) - (var(--n) - 1)/2) * var(--spread)))
      translateY(var(--lift));
    z-index: calc(var(--z-index-100) - var(--i));
  }

  /* lift the hovered card and bring it to the top */
  > img:hover{
    transform:
      translateX(calc(-50% + (var(--i) - (var(--n) - 1)/2) * var(--shift)))
      rotate(calc((var(--i) - (var(--n) - 1)/2) * var(--spread)))
      translateY(var(--hover-lift));
    box-shadow: 0 16px 30px rgba(0,0,0,.35);
    z-index: var(--z-index-999);
  }

  /* ---- small utility: assign index (--i) with :nth-child rules ---- */
  /* also writes --n = number of cards seen so far; update the last rule if you show more */
  /* add more nth-child lines if you might have more cards */
  > :nth-child(1){ --i: 0 }
  > :nth-child(2){ --i: 1 }
  > :nth-child(3){ --i: 2 }
  > :nth-child(4){ --i: 3 }
  > :nth-child(5){ --i: 4 }
}

/* optional: tighten the spread on narrow screens */
@media (max-width: 600px){
  :deep(.fan-thin) {
    --card-w: 140px;
    --spread: 11deg;
    --shift: -44px;
  }
}

div:has(.unspendable) {
  border: 2px dashed teal;
  margin-inline: 10px;
  display: block;
  width: 100%;
  padding: 10px;
  text-transform: uppercase;
}
</style>

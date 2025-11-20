<script lang="ts">
import { defineComponent, h } from 'vue'
import { type FlavorTextEntry, type FlavorTextModifier, type ImageModifier, type ListItemEntry } from '@/arkham/types/FlavorText'
import { baseUrl, formatContent } from '@/arkham/helpers'
import { I18n, useI18n } from 'vue-i18n'
import { imgsrc } from '@/arkham/helpers'
import { tarotArcanaImage } from '@/arkham/types/TarotCard'

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
    case 'NestedEntry': return 'nested'
    case 'ResolutionEntry': return 'resolution'
    case 'CheckpointEntry': return 'checkpoint'
    case 'InterludeEntry': return 'interlude'
    case 'RightAligned': return 'right'
    case 'CenteredEntry': return 'center'
    case 'NoUnderline': return 'no-underline'
    case 'PlainText': return 'basic'
    case 'InvalidEntry': return 'invalid'
    case 'ValidEntry': return 'valid'
    default: throw new Error("Unknown modifier")
  }
}

function formatListEntry(t: I18n, entry: { tag: 'ListEntry', list: ListItemEntry[] }): any {
  const inner = formatEntry(t, entry.entry)
  return h('li',  entry.nested.length == 0 ? inner : [inner, h('ul', entry.nested.map((e) => formatListEntry(t, e)))])
}

function formatEntry(t: I18n, entry: FlavorTextEntry, classes: { [key: string]: boolean } = {}): any {
  switch (entry.tag) {
    case 'BasicEntry': return h('p', { innerHTML: formatContent(entry.text.startsWith('$') ? t(entry.text.slice(1)) : entry.text) })
    case 'HeaderEntry': return h('header', [h('h1', { class: classes, innerHTML: formatContent(t(entry.key)) })])
    case 'I18nEntry': return h('div', { innerHTML: formatContent(t(entry.key, {...entry.variables, setImgPath: `${baseUrl}/img/arkham/encounter-sets` })) })
    case 'ModifyEntry': {
      // HeaderEntry is handled specially to avoid wrapping it in a div
      if (entry.entry.tag === 'HeaderEntry') return formatEntry(t, entry.entry, entryStyles(entry))
      return h('div', { class: entryStyles(entry) }, [formatEntry(t, entry.entry)])
    }
    case 'CompositeEntry': return h('div', { class: "composite" }, entry.entries.map((e) => formatEntry(t, e)))
    case 'ColumnEntry': return h('div', { class: "columns" }, entry.entries.map((e) => formatEntry(t, e)))
    case 'ListEntry': return h('ul', entry.list.map((e) => formatListEntry(t, e)))
    case 'CardEntry': return h('div', [h('img', { class: entryStyles(entry), src: imgsrc(`cards/${entry.cardCode.replace(/^c/, "")}.avif`)})])
    case 'TarotEntry': return h('div', [h('img', { class: entryStyles(entry), src: imgsrc(`tarot/${tarotArcanaImage(entry.tarot)}`)})])
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
    z-index: 5;
  }

  .composite {
    position: relative;
  }

  .composite:not(:first-child)::after {
    position: absolute;
    bottom: -1rem;
    left: 0;
    height: calc(100% - 20px);
    margin-bottom: 20px;
    content: '';
    border-left: 1px solid black;
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
  z-index: 0;

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
    z-index: -1;
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
  min-height: 6em;
  place-content: center;
  border: 3px solid var(--color);
  border-radius: 55px;
  background-color: color-mix(in srgb, var(--color), transparent 90%);
  padding: 20px;
  position: relative;
  z-index: 0;

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

:deep(p.cursive) {
  font-family: "ArkhamCursive";
  display: block;
  font-size: 1.3em;
  line-height: 1.3em;
  padding-bottom: 1.3em;
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

p.billenia, :deep(p.billenia) {
  font-family: "Billenia";
  font-weight: 500;
  font-size: 1.4em;
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
        z-index: 2;
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

h3, :deep(h3) {
  margin-bottom: 10px;
  font-size: 1.1em;
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

.red, :deep(.red) {
  position: relative;
  margin: 20px;
  isolation: isolate;
  mix-blend-mode: multiply;
  &::before {
    z-index: -1;
    content: '';
    position: absolute;
    inset: -10px;
    background: #F4DFD1;
    filter: blur(5px);
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
  margin: 10px;
}

:deep(h1) {
  font-family: "Teutonic";
  font-weight: 500;
  color: #38615F;
  padding-bottom: 2px;
  &:not(.no-underline) {
    border-bottom: 1px solid #38615f;
    &::after {
      border-bottom: 1px solid #38615f;
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
  color: #222;
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
}

:deep(hr) {
  border:0;
  border-bottom: 2px solid #60759F;
  margin-inline: -20px;
  margin-block: 10px;
}

img.remove {
  filter: brightness(81%) saturate(113%);
}

div:has(> img.remove) {
  position: relative;
  &::before {
    z-index: 1;
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
    z-index: calc(var(--i) + 1);
  }

  /* lift the hovered card and bring it to the top */
  > img:hover{
    transform:
      translateX(calc(-50% + (var(--i) - (var(--n) - 1)/2) * var(--shift)))
      rotate(calc((var(--i) - (var(--n) - 1)/2) * var(--spread)))
      translateY(var(--hover-lift));
    box-shadow: 0 16px 30px rgba(0,0,0,.35);
    z-index: 999;
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

div:has(.unspendable) {
  border: 2px dashed teal;
  margin-inline: 10px;
  display: block;
  width: 100%;
  padding: 10px;
  text-transform: uppercase;
}
</style>

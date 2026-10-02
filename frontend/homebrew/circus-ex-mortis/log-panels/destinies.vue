<script lang="ts" setup>
/* The Fata Diana's destinies, one per investigator.
 *
 * The campaign log stores each as a plain string, "<investigator>:<word>" (see
 * `destinyEntry` in the campaign's Haskell helpers), which the generic recorded
 * set list would print verbatim as "02005:heart".
 */
import { computed, ref } from 'vue'
import { investigatorPortrait } from '@/arkham/cardImages'
import type { Game } from '@/arkham/types/Game'
import { useDbCardStore } from '@/stores/dbCards'
import { useI18n } from 'vue-i18n'

const props = defineProps<{ entries: any[]; game: Game }>()

const { t } = useI18n()
const store = useDbCardStore()

/* Card codes come in two forms and this panel needs both. The string inside the
 * recorded set carries the RAW code (`02005`) because the backend writes
 * `unCardCode`, while every id in the game JSON is `c`-prefixed (`ToJSON
 * CardCode` prepends it). So: `c` + code to find the seat, raw code for
 * `dbCards` (indexed by ArkhamDB code, which has no prefix). `investigatorPortrait`
 * strips the prefix itself, so it takes the prefixed id.
 */
const seats = computed<Record<string, any>>(() => ({
  ...props.game.investigators,
  ...props.game.otherInvestigators,
  ...props.game.killedInvestigators,
}))

const investigatorName = (code: string): string => {
  const seat = seats.value[`c${code}`]
  if (seat) return seat.name.subtitle ? `${seat.name.title}: ${seat.name.subtitle}` : seat.name.title
  // Nobody at the table holds it: a stale entry, or a destiny whose investigator
  // left before a replacement claimed it. The card record still knows the name.
  const dbCard = store.getDbCard(code)
  if (dbCard) return dbCard.subname ? `${dbCard.name}: ${dbCard.subname}` : dbCard.name
  return code
}

/* The destiny as the interlude printed it. Those labels are choice labels, so
 * they carry the "Record ..." instruction after a <br>; only the sentence
 * belongs in the log. An unrecognised word shows itself rather than a missing
 * i18n path. */
const destinyLabel = (word: string): string => {
  const key = `circusExMortis.writtenInStone.label.${word}`
  const label = t(key)
  if (!label || label === key) return word
  return label.split('<br>')[0]
}

const destinies = computed(() =>
  props.entries
    .map((entry) => {
      const raw = String(entry?.contents ?? entry?.recordVal?.contents ?? '')
      const idx = raw.indexOf(':')
      const code = idx === -1 ? '' : raw.slice(0, idx)
      const word = idx === -1 ? raw : raw.slice(idx + 1)
      return {
        key: raw,
        code,
        word,
        name: code ? investigatorName(code) : '',
        label: destinyLabel(word),
        crossedOut: entry?.tag === 'CrossedOut',
      }
    })
    .filter((d) => d.key)
)

// A departed or made-up investigator may have no portrait on the CDN; drop the
// frame rather than leave a broken image in the row.
const noPortrait = ref<string[]>([])
</script>

<template>
  <ul class="destinies">
    <li v-for="d in destinies" :key="d.key" :class="{ 'crossed-out': d.crossedOut }">
      <div
        v-if="d.code && !noPortrait.includes(d.code)"
        class="portrait-wrap"
      >
        <img
          :src="investigatorPortrait(game, `c${d.code}`)"
          class="portrait"
          alt=""
          @error="noPortrait.push(d.code)"
        />
      </div>
      <div class="destiny">
        <span v-if="d.name" class="name">{{ d.name }}</span>
        <span class="label">{{ d.label }}</span>
      </div>
    </li>
  </ul>
</template>

<style scoped>
.destinies {
  display: flex;
  flex-direction: column;
  gap: 4px;
  margin: 0;
  padding: 0;
  list-style: none;
}

.destinies li {
  display: flex;
  align-items: center;
  gap: 10px;
  margin: 0;
  padding: 7px 10px;
  border-radius: 5px;
  background: rgba(255, 255, 255, 0.04);
  color: var(--title);
  font-size: 0.92rem;
  line-height: 1.4;
}

.portrait-wrap {
  width: 40px;
  height: 40px;
  border-radius: 6px;
  overflow: hidden;
  flex-shrink: 0;
  border: 1px solid rgba(255, 255, 255, 0.15);
  box-shadow: 0 2px 6px rgba(0, 0, 0, 0.5);
}

.portrait {
  width: 115px;
  display: block;
}

.destiny {
  display: flex;
  flex-direction: column;
  min-width: 0;
}

.name {
  font-family: teutonic, sans-serif;
  font-size: 1.1em;
  letter-spacing: 0.04em;
  overflow-wrap: break-word;
}

.label {
  color: rgba(255, 255, 255, 0.7);
  font-style: italic;
  overflow-wrap: break-word;
}

.crossed-out {
  text-decoration: line-through;
}
</style>

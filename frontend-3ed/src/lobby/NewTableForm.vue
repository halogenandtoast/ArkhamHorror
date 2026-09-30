<script setup lang="ts">
import { computed, reactive, ref } from 'vue'
import { useRouter } from 'vue-router'
import { createTable, errorText } from '@/api'
import { img, isBroken, markBroken } from '@/assets'
import ExamineIcon from '@/game/ExamineIcon.vue'
import { zoomFlip } from '@/game/overlays'
import { GAME_MODES, expArt, expName } from '@/game/util'
import type { Catalog, GameMode, ScenarioInfo } from '@/types'

const props = defineProps<{ catalog: Catalog }>()
const emit = defineEmits<{ cancel: [] }>()
const router = useRouter()

const choice = reactive({
  name: '',
  seats: 1,
  scenario: '',
  extras: [] as string[],
  mode: 'StandardMode' as GameMode,
  debug: false,
})
const error = ref('')
const busy = ref(false)

// the scenarios box by box, in the order the catalog lists them; a box the engine
// cannot play yet is folded away rather than filling the page
const groups = computed(() =>
  props.catalog.expansions
    .map((e) => {
      const scenarios = props.catalog.scenarios.filter((sc) => sc.expansion === e)
      return { exp: e, scenarios, playable: scenarios.some((sc) => sc.playable) }
    })
    .filter((g) => g.scenarios.length),
)
const picked = computed<ScenarioInfo | null>(
  () => props.catalog.scenarios.find((sc) => sc.code === choice.scenario) ?? null,
)
/* The base game is always in play, so it is never offered. Any other box the
engine has cards for can lend its content to a scenario from elsewhere -- Dead
of Night's monsters and encounters in a Core Set scenario, say. */
const contentBoxes = computed(
  () =>
    props.catalog.contentExpansions ??
    props.catalog.expansions.filter((e) => props.catalog.scenarios.some((sc) => sc.expansion === e && sc.playable)),
)
const extrasOffered = computed(() => {
  const sc = picked.value
  if (!sc) return []
  return contentBoxes.value.filter((e) => e !== 'CoreSet' && e !== sc.expansion)
})
const expansions = computed(() => {
  const sc = picked.value
  const extras = choice.extras.filter((e) => extrasOffered.value.includes(e))
  return ['CoreSet', ...(sc ? [sc.expansion] : []), ...extras]
})
const modeNote = computed(() => GAME_MODES.find(([v]) => v === choice.mode)?.[2] ?? '')

const story = (code: string) => img(`scenarios/${code}.webp`)
const setup = (code: string) => img(`scenarios/${code}b.webp`)
const cover = (e: string) => img(`expansions/${expArt(e)}.webp`)
const toggleExtra = (e: string, on: boolean) => {
  choice.extras = on ? [...choice.extras.filter((x) => x !== e), e] : choice.extras.filter((x) => x !== e)
}
const pick = (sc: ScenarioInfo) => {
  if (sc.playable) choice.scenario = sc.code
}

async function submit() {
  if (busy.value || !picked.value) return
  busy.value = true
  error.value = ''
  try {
    const t = await createTable({
      name: choice.name.trim() || undefined,
      seats: choice.seats,
      expansions: expansions.value,
      mode: choice.mode,
      debug: choice.debug,
      scenario: choice.scenario,
    })
    void router.push(`/tables/${t.id}`)
  } catch (e) {
    error.value = errorText(e)
  } finally {
    busy.value = false
  }
}
</script>

<template>
  <div id="setup" class="ng-page">
    <header class="ng-header">
      <h2>New game</h2>
      <p class="ng-lede">Pick the scenario you want to play, then say who is at the table.</p>
    </header>
    <form class="ng" @submit.prevent="submit">
      <div class="ng-scenarios">
        <section v-for="g in groups" :key="g.exp" class="ng-card" :class="{ 'ng-later': !g.playable }">
          <!-- a box the engine cannot play yet opens only if you ask to see it -->
          <component :is="g.playable ? 'div' : 'details'" class="ng-group">
            <component :is="g.playable ? 'div' : 'summary'" class="ng-title">
              {{ expName(g.exp) }}<template v-if="!g.playable"> · not in the engine yet</template>
            </component>
            <div class="sc-grid">
            <!-- a scenario is picked by its sheet; the corner icon opens it flippable between story and setup sides -->
            <div v-for="sc in g.scenarios" :key="sc.code" class="sc-tile" :class="{ waiting: !sc.playable }">
              <button
                type="button"
                class="sc-pick"
                :class="{ 'no-art': isBroken(story(sc.code)), on: choice.scenario === sc.code }"
                :disabled="!sc.playable"
                :title="sc.playable ? `Play ${sc.name}` : `${sc.name} is not in the engine yet`"
                @click="pick(sc)"
              >
                <img v-if="!isBroken(story(sc.code))" :src="story(sc.code)" alt="" @error="markBroken(story(sc.code))" />
                <span class="sc-name">{{ sc.name }}</span>
                <span class="sc-exp">{{ sc.playable ? expName(sc.expansion) : 'Not playable yet' }}</span>
              </button>
              <span
                class="label-zoom"
                role="button"
                tabindex="0"
                title="Enlarge, then click to flip"
                @click="zoomFlip(story(sc.code), setup(sc.code))"
                @keydown.enter.prevent="zoomFlip(story(sc.code), setup(sc.code))"
                ><ExamineIcon
              /></span>
              </div>
            </div>
          </component>
        </section>
      </div>

      <div class="ng-config">
        <div v-if="extrasOffered.length" class="ng-card">
          <div class="ng-title">Add content</div>
          <div class="ng-tiles">
            <template v-for="e in extrasOffered" :key="e">
              <input
                :id="`ngE${e}`"
                type="checkbox"
                name="exp"
                :value="e"
                :checked="choice.extras.includes(e)"
                @change="toggleExtra(e, ($event.target as HTMLInputElement).checked)"
              /><label :for="`ngE${e}`" :class="{ 'no-art': isBroken(cover(e)) }"
                ><img v-if="!isBroken(cover(e))" :src="cover(e)" alt="" @error="markBroken(cover(e))" />
                <span class="ng-tile-text"
                  ><b>{{ expName(e) }}</b
                  ><small>Monsters, encounters and cards from this box</small></span
                ></label
              >
            </template>
          </div>
          <p class="ng-note">The base game is always included.</p>
        </div>

        <div class="ng-card">
          <div class="ng-title">Game name</div>
          <div class="ng-seed">
            <input v-model="choice.name" class="ng-text" type="text" placeholder="Optional" maxlength="80" />
          </div>
        </div>

        <div class="ng-card">
          <div class="ng-title">Number of seats</div>
          <div class="ng-seg" style="--n: 6">
            <template v-for="n in [1, 2, 3, 4, 5, 6]" :key="n">
              <input :id="`ngP${n}`" v-model="choice.seats" type="radio" name="seats" :value="n" /><label :for="`ngP${n}`">{{
                n
              }}</label>
            </template>
          </div>
          <p class="ng-note">You take seat 1. A player may hold several seats.</p>
        </div>

        <div class="ng-card">
          <div class="ng-title">Game mode</div>
          <div class="ng-seg" style="--n: 3">
            <template v-for="[v, l] in GAME_MODES" :key="v">
              <input :id="`ngM${v}`" v-model="choice.mode" type="radio" name="mode" :value="v" /><label :for="`ngM${v}`">{{
                l
              }}</label>
            </template>
          </div>
          <p id="ngModeNote" class="ng-note">{{ modeNote }}</p>
        </div>

        <div class="ng-card">
          <div class="ng-title">Debug controls</div>
          <div class="ng-seg" style="--n: 2">
            <input id="ngDbgOff" v-model="choice.debug" type="radio" name="debug" :value="false" /><label for="ngDbgOff"
              >Off</label
            >
            <input id="ngDbgOn" v-model="choice.debug" type="radio" name="debug" :value="true" /><label for="ngDbgOn">On</label>
          </div>
          <p class="ng-note">Lets every seated player edit tokens, deal cards and look through decks.</p>
        </div>

        <div class="ng-actions table-actions">
          <button id="ngStart" class="primary" type="submit" :disabled="busy || !picked">
            {{ picked ? `Play ${picked.name}` : 'Pick a scenario' }}
          </button>
          <button type="button" :disabled="busy" @click="emit('cancel')">Cancel</button>
        </div>
        <div id="setupError" class="err">{{ error }}</div>
      </div>
    </form>
  </div>
</template>

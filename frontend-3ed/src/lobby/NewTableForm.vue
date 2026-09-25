<script setup lang="ts">
import { computed, reactive, ref } from 'vue'
import { useRouter } from 'vue-router'
import { createTable, errorText } from '@/api'
import { img, isBroken, markBroken } from '@/assets'
import { GAME_MODES, expArt, expName } from '@/game/util'
import type { Catalog, GameMode } from '@/types'

const props = defineProps<{ catalog: Catalog }>()
const emit = defineEmits<{ cancel: [] }>()
const router = useRouter()

const choice = reactive({ name: '', seats: 1, expansions: ['CoreSet'] as string[], mode: 'StandardMode' as GameMode, debug: false })
const error = ref('')
const busy = ref(false)

const counts = computed(() => {
  const out: Record<string, number> = {}
  props.catalog.scenarios.filter((sc) => sc.playable).forEach((sc) => (out[sc.expansion] = (out[sc.expansion] || 0) + 1))
  return out
})
const modeNote = computed(() => GAME_MODES.find(([v]) => v === choice.mode)?.[2] ?? '')
const cover = (e: string) => img(`expansions/${expArt(e)}.webp`)
// the core set is the base game; expansions only add to it
const toggleExp = (e: string, on: boolean) => {
  const rest = choice.expansions.filter((x) => x !== e && x !== 'CoreSet')
  choice.expansions = ['CoreSet', ...(on ? [...rest, e] : rest)]
}

async function submit() {
  if (busy.value) return
  busy.value = true
  error.value = ''
  try {
    const t = await createTable({
      name: choice.name.trim() || undefined,
      seats: choice.seats,
      expansions: choice.expansions,
      mode: choice.mode,
      debug: choice.debug,
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
  <div id="setup">
    <header class="ng-header"><h2>New game</h2></header>
    <form class="ng" @submit.prevent="submit">
      <div class="ng-config">
        <div class="ng-card">
          <div class="ng-title">Game name</div>
          <div class="ng-seed"><input v-model="choice.name" class="ng-text" type="text" placeholder="Optional" maxlength="80" /></div>
        </div>
        <div class="ng-card">
          <div class="ng-title">Number of seats</div>
          <div class="ng-seg" style="--n: 6">
            <template v-for="n in [1, 2, 3, 4, 5, 6]" :key="n">
              <input :id="`ngP${n}`" v-model="choice.seats" type="radio" name="seats" :value="n" /><label :for="`ngP${n}`">{{ n }}</label>
            </template>
          </div>
          <p class="ng-note">You take seat 1. A player may hold several seats.</p>
        </div>
        <div class="ng-card">
          <div class="ng-title">Game content</div>
          <div class="ng-tiles">
            <template v-for="e in catalog.expansions" :key="e">
              <input
                :id="`ngE${e}`"
                type="checkbox"
                name="exp"
                :value="e"
                :checked="choice.expansions.includes(e)"
                :disabled="e === 'CoreSet'"
                @change="toggleExp(e, ($event.target as HTMLInputElement).checked)"
              /><label :for="`ngE${e}`" :class="{ 'no-art': isBroken(cover(e)) }"
                ><img v-if="!isBroken(cover(e))" :src="cover(e)" alt="" @error="markBroken(cover(e))" />
                <span class="ng-tile-text"
                  ><b>{{ expName(e) }}</b
                  ><small
                    >{{ e === 'CoreSet' ? 'Always included · ' : ''
                    }}{{
                      counts[e] ? `${counts[e]} playable scenario${counts[e] === 1 ? '' : 's'}` : 'none playable yet'
                    }}</small
                  ></span
                ></label
              >
            </template>
          </div>
        </div>
        <div class="ng-card">
          <div class="ng-title">Game mode</div>
          <div class="ng-seg" style="--n: 3">
            <template v-for="[v, l] in GAME_MODES" :key="v">
              <input :id="`ngM${v}`" v-model="choice.mode" type="radio" name="mode" :value="v" /><label :for="`ngM${v}`">{{ l }}</label>
            </template>
          </div>
          <p id="ngModeNote" class="ng-note">{{ modeNote }}</p>
        </div>
        <div class="ng-card">
          <div class="ng-title">Debug controls</div>
          <div class="ng-seg" style="--n: 2">
            <input id="ngDbgOff" v-model="choice.debug" type="radio" name="debug" :value="false" /><label for="ngDbgOff">Off</label>
            <input id="ngDbgOn" v-model="choice.debug" type="radio" name="debug" :value="true" /><label for="ngDbgOn">On</label>
          </div>
          <p class="ng-note">Lets every seated player edit tokens, deal cards and look through decks.</p>
        </div>
        <div class="ng-actions table-actions">
          <button id="ngStart" class="primary" type="submit" :disabled="busy">Create game</button>
          <button type="button" :disabled="busy" @click="emit('cancel')">Cancel</button>
        </div>
        <div id="setupError" class="err">{{ error }}</div>
      </div>
    </form>
  </div>
</template>

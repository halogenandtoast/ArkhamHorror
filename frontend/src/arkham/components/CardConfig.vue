<script lang="ts" setup>
import { computed, nextTick, onMounted, onUnmounted, ref } from 'vue';
import { OnClickOutside } from '@vueuse/components';
import { useCardStore } from '@/stores/cards';
import { useDbCardStore } from '@/stores/dbCards';
import { cardArt } from '@/arkham/cardImages';
import { replaceIcons } from '@/arkham/helpers';
import { useI18n } from 'vue-i18n';
import * as Api from '@/arkham/api';
import type { Game } from '@/arkham/types/Game';
import type { CardOption, OptionValue } from '@/arkham/types/CardDef';

/* A card's player-configurable options. Renders nothing at all unless the card
 * declares options in its CardDef (`cdOptions` on the backend), so it is safe to
 * mount unconditionally on any card component. */
const props = defineProps<{
  game: Game
  playerId: string
  cardCode: string
  /* Which corner offsets to use. Cards are small at the tightest breakpoint, so
   * the caller can ask for the compact glyph. */
  compact?: boolean
}>();

const { t, te } = useI18n();
const cardStore = useCardStore();
const dbCardStore = useDbCardStore();

const frame = ref<HTMLElement | null>(null);
const panelRef = ref<HTMLElement | null>(null);
const shown = ref(false);
const panelPosition = ref<Record<string, string>>({});

const options = computed<CardOption[]>(() =>
  cardStore.cards.find((c) => c.cardCode === props.cardCode)?.options ?? []
);

const investigator = computed(() =>
  Object.values(props.game.investigators).find((i) => i.playerId === props.playerId)
);

const chosen = computed<Record<string, OptionValue>>(() =>
  investigator.value?.settings?.perCardSettings?.[props.cardCode]?.cardOptions ?? {}
);

const defaultOf = (option: CardOption): OptionValue =>
  option.type.tag === 'toggle' ? option.type.default : option.type.default;

const valueOf = (option: CardOption): OptionValue =>
  chosen.value[option.key] ?? defaultOf(option);

const isOn = (option: CardOption): boolean => valueOf(option) === true;

/* Options are grouped by the ability they scope to. An ability's group is
 * headed by that ability's printed text so it's unambiguous which ability the
 * toggle governs; options with no ability apply to the card as a whole and lead
 * the list without a heading. */
type OptionGroup = { ability: number | null; text: string | null; options: CardOption[] };

const groups = computed<OptionGroup[]>(() => {
  const cardless = options.value.filter((o) => o.ability === undefined);
  const abilities = [...new Set(options.value.flatMap((o) => o.ability === undefined ? [] : [o.ability]))]
    .sort((a, b) => a - b);

  return [
    ...(cardless.length ? [{ ability: null, text: null, options: cardless }] : []),
    ...abilities.map((n) => ({
      ability: n,
      text: abilityText(n),
      options: options.value.filter((o) => o.ability === n),
    })),
  ];
});

function abilityText(n: number): string | null {
  const key = `cardOption.${props.cardCode}.abilities.${n}`;
  const text = te(key) ? t(key) : null;
  return text ? replaceIcons(text).replace(/_([^_]*)_/g, '<b>$1</b>') : null;
}

/* Teal gear = at least one option is off its default. This is the only signal
 * left on the board once the panel closes, so it has to mean "I changed
 * something here", not merely "this card is configurable". */
const configured = computed(() =>
  options.value.some((o) => valueOf(o) !== defaultOf(o))
);

// dbCards are keyed by the bare ArkhamDB code; game card codes carry a 'c' prefix.
const cardName = computed(() => dbCardStore.getDbCard(cardArt(props.cardCode))?.name ?? '');

const label = (option: CardOption) => `cardOption.${props.cardCode}.${option.key}.label`;

async function set(option: CardOption, value: OptionValue) {
  const iid = investigator.value?.id;
  if (!iid) return;
  await Api.setCardOption(props.game.id, iid, props.cardCode, option.key, value);
}

const toggle = (option: CardOption) => set(option, !isOn(option));

const valuesOf = (option: CardOption): string[] =>
  option.type.tag === 'choice' ? option.type.values : [];

/* Falls back to the raw value id so a new choice value is legible before its
 * string lands. */
const valueLabel = (option: CardOption, value: string) => {
  const key = `cardOption.${props.cardCode}.${option.key}.values.${value}`;
  return te(key) ? t(key) : value;
};

const inputId = (option: CardOption, value: string) =>
  `cfg-${props.cardCode}-${option.key}-${value}`;

function calculatePosition() {
  if (!frame.value) return;
  const rect = frame.value.getBoundingClientRect();
  const menuWidth = panelRef.value?.getBoundingClientRect().width ?? 330;
  const margin = 8;
  const maxLeft = Math.max(margin, window.innerWidth - menuWidth - margin);
  panelPosition.value = {
    bottom: `${window.innerHeight - rect.top + margin}px`,
    left: `${Math.min(Math.max(rect.left + rect.width / 2 - menuWidth / 2, margin), maxLeft)}px`,
  };
}

function open() {
  shown.value = true;
  nextTick(calculatePosition);
}

const close = () => { shown.value = false };

function updatePosition() {
  if (shown.value) calculatePosition();
}

onMounted(() => {
  window.addEventListener('resize', updatePosition);
  window.addEventListener('scroll', updatePosition, true);
});

onUnmounted(() => {
  window.removeEventListener('resize', updatePosition);
  window.removeEventListener('scroll', updatePosition, true);
});
</script>

<template>
  <button
    v-if="options.length > 0"
    ref="frame"
    type="button"
    class="card-config-gear"
    :class="{ 'card-config-gear--on': configured, 'card-config-gear--compact': compact }"
    :aria-label="$t('cardOption.configure')"
    v-tooltip="$t('cardOption.configure')"
    @click.stop.prevent="shown ? close() : open()"
  >
    <font-awesome-icon icon="gear" />
  </button>

  <Teleport to="body">
    <OnClickOutside v-if="shown" @trigger="close" :options="{ ignore: [frame] }">
      <div
        ref="panelRef"
        class="card-config no-card-overlay"
        :style="panelPosition"
    >
        <div class="card-config__header">
          <h2 class="card-config__title">{{ cardName }}</h2>
        </div>

        <div class="card-config__body">
          <section v-for="group in groups" :key="group.ability ?? 'card'" class="card-config__section">
            <div v-if="group.text" class="card-config__ability" v-html="group.text" />
            <div class="toggle-list">
              <div v-for="option in group.options" :key="option.key" class="toggle-row">
                <div class="toggle-text">
                  <div class="toggle-name">{{ $t(label(option)) }}</div>
                </div>

                <div v-if="option.type.tag === 'toggle'" class="segmented toggle-control">
                  <input
                    type="radio"
                    :id="inputId(option, 'on')"
                    :name="inputId(option, 'group')"
                    :checked="isOn(option)"
                    @change="set(option, true)"
                  />
                  <label :for="inputId(option, 'on')">{{ $t('On') }}</label>
                  <input
                    type="radio"
                    :id="inputId(option, 'off')"
                    :name="inputId(option, 'group')"
                    :checked="!isOn(option)"
                    @change="set(option, false)"
                  />
                  <label :for="inputId(option, 'off')">{{ $t('Off') }}</label>
                </div>

                <div v-else class="segmented toggle-control">
                  <template v-for="value in valuesOf(option)" :key="value">
                    <input
                      type="radio"
                      :id="inputId(option, value)"
                      :name="inputId(option, 'group')"
                      :checked="valueOf(option) === value"
                      @change="set(option, value)"
                    />
                    <label :for="inputId(option, value)">{{ valueLabel(option, value) }}</label>
                  </template>
                </div>
              </div>
            </div>
          </section>
        </div>

      </div>
    </OnClickOutside>
  </Teleport>
</template>

<style scoped>
/* Bottom-left of the card frame is the one corner nothing else uses. The glyph
   is bare — no pill, no border — with drop-shadows so it survives light art. The
   button itself is padded out well past the glyph for a usable touch target. */
.card-config-gear {
  position: absolute;
  left: 0;
  bottom: 0;
  z-index: var(--z-index-3);
  display: grid;
  place-items: center;
  width: 28px;
  height: 28px;
  padding: 0;
  margin: 0;
  border: 0;
  background: none;
  cursor: pointer;
  color: rgba(255, 255, 255, 0.62);
  font-size: 15px;
  filter:
    drop-shadow(0 0 1px rgba(0, 0, 0, 0.95))
    drop-shadow(0 1px 2px rgba(0, 0, 0, 0.85));
  transition: color 0.15s ease;
}

.card-config-gear :deep(svg) {
  transition: transform 0.15s ease;
}

.card-config-gear:hover {
  color: var(--text);
}

.card-config-gear:hover :deep(svg) {
  transform: rotate(35deg);
}

.card-config-gear--on {
  color: var(--highlight);
  filter:
    drop-shadow(0 0 1px rgba(0, 0, 0, 0.95))
    drop-shadow(0 0 4px rgba(45, 212, 191, 0.75));
}

.card-config-gear--compact {
  width: 24px;
  height: 24px;
  font-size: 13px;
}

@media (max-width: 800px) and (orientation: portrait) {
  .card-config-gear {
    font-size: 13px;
  }
}
</style>

<style>
/* Unscoped: the panel is teleported to body. Deliberately mirrors
   arkham/components/Settings.vue — same header/section/toggle-row/segmented
   vocabulary — so card options read as the same kind of thing as game settings. */
.card-config {
  position: fixed;
  z-index: var(--z-modal-overlay);
  display: flex;
  flex-direction: column;
  width: min(330px, calc(100vw - 16px));
  max-height: 70vh;
  background: var(--background);
  color: var(--text);
  border: 1px solid var(--box-border);
  border-radius: 5px;
  box-shadow: 0 8px 30px rgba(0, 0, 0, 0.6);
  overflow: hidden;
}

.card-config__header {
  flex-shrink: 0;
  padding: 8px 14px;
  background: var(--background-dark);
  border-bottom: 1px solid var(--box-border);
}

.card-config__title {
  margin: 0;
  font-family: Teutonic, serif;
  font-size: 17px;
  color: var(--text);
  text-transform: none;
}

.card-config__body {
  flex: 1 1 auto;
  min-height: 0;
  overflow-y: auto;
  padding: 12px 14px;
  display: flex;
  flex-direction: column;
  gap: 16px;
}

.card-config__section {
  display: flex;
  flex-direction: column;
}

/* The ability's printed text heads its section the way `.section-title` does in
   the settings pane, so the settings below plainly belong to it — but set as
   card text rather than a uppercase label, since it is a sentence. */
.card-config__ability {
  margin: 0 0 8px;
  padding-bottom: 7px;
  font-size: 12px;
  line-height: 1.45;
  color: var(--title);
  border-bottom: 1px solid var(--box-border);
}

.card-config .toggle-list {
  display: flex;
  flex-direction: column;
  gap: 6px;
}

.card-config .toggle-row {
  display: grid;
  grid-template-columns: 1fr auto;
  gap: 12px;
  align-items: center;
  padding: 8px 12px;
  background: var(--box-background);
  border: 1px solid var(--box-border);
  border-radius: 5px;
}

.card-config .toggle-row:hover {
  background: var(--background-mid);
}

.card-config .toggle-text {
  min-width: 0;
}

.card-config .toggle-name {
  font-size: 13px;
  font-weight: 500;
  color: var(--text);
}

.card-config .toggle-control {
  min-width: 110px;
  flex-shrink: 0;
  justify-self: end;
}

.card-config .segmented {
  display: grid;
  grid-auto-flow: column;
  grid-auto-columns: 1fr;
  border-radius: 5px;
  background: var(--background-dark);
  border: 1px solid var(--box-border);
  padding: 2px;
  gap: 2px;
}

.card-config .segmented input[type='radio'] {
  display: none;
}

.card-config .segmented label {
  display: flex;
  align-items: center;
  justify-content: center;
  padding: 5px 8px;
  text-transform: uppercase;
  letter-spacing: 0.06em;
  font-size: 11px;
  font-weight: 600;
  white-space: nowrap;
  user-select: none;
  cursor: pointer;
  border-radius: 3px;
  color: var(--background-light);
  margin: 0;
}

.card-config .segmented label:hover {
  color: var(--text);
}

.card-config .segmented input[type='radio']:checked + label {
  background: var(--button-1);
  color: var(--text);
}

.card-config .segmented input[type='radio']:checked + label:hover {
  background: var(--button-1-highlight);
}

@media (max-width: 700px) {
  .card-config .toggle-row {
    grid-template-columns: 1fr;
    gap: 10px;
  }
  .card-config .toggle-control {
    width: 100%;
  }
}
</style>

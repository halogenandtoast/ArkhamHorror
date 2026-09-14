import { computed, type Ref, unref } from 'vue';
import { useI18n } from 'vue-i18n';
import { useCardStore } from '@/stores/cards';
import { useDbCardStore } from '@/stores/dbCards';
import { cardArt } from '@/arkham/cardImages';
import { replaceIcons } from '@/arkham/helpers';
import * as Api from '@/arkham/api';
import type { Game } from '@/arkham/types/Game';
import type { CardOption, OptionValue } from '@/arkham/types/CardDef';

/* Options are grouped by the ability they scope to. An ability's group is
 * headed by that ability's printed text so it's unambiguous which ability the
 * setting governs; options with no ability apply to the card as a whole and
 * lead the list without a heading. */
export type OptionGroup = {
  ability: number | null
  text: string | null
  options: CardOption[]
}

/* Card codes carry a 'c' prefix everywhere the frontend sees them (ToJSON
 * CardCode adds it); dbCards are keyed by the bare ArkhamDB code. */
export const cardOptionName = (cardCode: string): string =>
  useDbCardStore().getDbCard(cardArt(cardCode))?.name ?? cardCode;

/* Every card that declares options, whether or not it is in play. Sorted by
 * localized name so the filter list reads the same way the player does. */
export function useConfigurableCards() {
  const cardStore = useCardStore();
  return computed(() =>
    cardStore.cards
      .filter((c) => (c.options?.length ?? 0) > 0)
      .map((c) => ({ cardCode: c.cardCode, name: cardOptionName(c.cardCode) }))
      .sort((a, b) => a.name.localeCompare(b.name))
  );
}

/* Diacritic- and case-insensitive so a filter typed without accents still
 * matches the localized name ("masque" -> "Masqué"). */
export const normalizeForSearch = (value: string): string =>
  value.normalize('NFD').replace(/\p{Diacritic}/gu, '').toLowerCase();

export function useCardOptions(
  game: Ref<Game> | Game,
  playerId: Ref<string> | string,
  cardCode: Ref<string | null> | string,
) {
  const { t, te } = useI18n();
  const cardStore = useCardStore();

  const code = computed(() => unref(cardCode));

  const options = computed<CardOption[]>(() => {
    const c = code.value;
    if (!c) return [];
    return cardStore.cards.find((def) => def.cardCode === c)?.options ?? [];
  });

  const investigator = computed(() =>
    Object.values(unref(game).investigators).find((i) => i.playerId === unref(playerId))
  );

  const chosen = computed<Record<string, OptionValue>>(() => {
    const c = code.value;
    if (!c) return {};
    return investigator.value?.settings?.perCardSettings?.[c]?.cardOptions ?? {};
  });

  const defaultOf = (option: CardOption): OptionValue => option.type.default;
  const valueOf = (option: CardOption): OptionValue => chosen.value[option.key] ?? defaultOf(option);
  const isOn = (option: CardOption): boolean => valueOf(option) === true;

  const valuesOf = (option: CardOption): string[] =>
    option.type.tag === 'choice' ? option.type.values : [];

  function abilityText(n: number): string | null {
    const key = `cardOption.${code.value}.abilities.${n}`;
    const text = te(key) ? t(key) : null;
    return text ? replaceIcons(text).replace(/_([^_]*)_/g, '<b>$1</b>') : null;
  }

  const groups = computed<OptionGroup[]>(() => {
    const cardless = options.value.filter((o) => o.ability === undefined);
    const abilities = [
      ...new Set(options.value.flatMap((o) => (o.ability === undefined ? [] : [o.ability]))),
    ].sort((a, b) => a - b);

    return [
      ...(cardless.length ? [{ ability: null, text: null, options: cardless }] : []),
      ...abilities.map((n) => ({
        ability: n,
        text: abilityText(n),
        options: options.value.filter((o) => o.ability === n),
      })),
    ];
  });

  /* At least one option is off its default. On the board this is the only
   * signal left once the popover closes, so it has to mean "I changed
   * something here", not merely "this card is configurable". */
  const configured = computed(() => options.value.some((o) => valueOf(o) !== defaultOf(o)));

  const label = (option: CardOption) => `cardOption.${code.value}.${option.key}.label`;

  /* Falls back to the raw value id so a new choice value stays legible before
   * its string lands. */
  const valueLabel = (option: CardOption, value: string) => {
    const key = `cardOption.${code.value}.${option.key}.values.${value}`;
    return te(key) ? t(key) : value;
  };

  const inputId = (option: CardOption, value: string) =>
    `cfg-${code.value}-${option.key}-${value}`;

  async function set(option: CardOption, value: OptionValue) {
    const iid = investigator.value?.id;
    const c = code.value;
    if (!iid || !c) return;
    await Api.setCardOption(unref(game).id, iid, c, option.key, value);
  }

  const toggle = (option: CardOption) => set(option, !isOn(option));

  return {
    options,
    groups,
    configured,
    valueOf,
    isOn,
    valuesOf,
    label,
    valueLabel,
    inputId,
    set,
    toggle,
  };
}

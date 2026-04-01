import type { Asset } from '@/arkham/types/Asset';
import type { Enemy } from '@/arkham/types/Enemy';
import type { Game } from '@/arkham/types/Game';

export const THE_BEYOND_CARD_CODE = 'c90052';
export type SpiritGhostKey = 'deck' | 'wisp' | 'mourn' | 'watcher';
export type SpiritGhostMotion = {
  ghostDuration: string;
  ghostDelay: string;
  mistInnerDuration: string;
  mistInnerDelay: string;
  mistOuterDuration: string;
  mistOuterDelay: string;
};

export function isSpiritDeckHost(asset: Asset): boolean {
  return asset.cardCode === THE_BEYOND_CARD_CODE && !!asset.spiritDeck;
}

export function isManifestedSpiritAsset(asset: Asset): boolean {
  return (asset.modifiers ?? []).some(
    (modifier) => modifier.type.tag === 'OtherModifier' && modifier.type.contents === 'IsSpirit',
  );
}

export function isManifestedSpiritEnemy(enemy: Enemy, game: Game): boolean {
  if (enemy.placement.tag !== 'AttachedToAsset') {
    return false;
  }

  const [hostAssetId] = enemy.placement.contents;
  return game.assets[hostAssetId]?.cardCode === THE_BEYOND_CARD_CODE;
}

export function spiritGhostKeyForCardCode(cardCode: string): SpiritGhostKey {
  const normalized = cardCode.replace(/^c/, '');
  let total = 0;

  for (const ch of normalized) {
    total += ch.charCodeAt(0);
  }

  switch (total % 3) {
    case 0:
      return 'wisp';
    case 1:
      return 'mourn';
    default:
      return 'watcher';
  }
}

function hashSpiritSeed(seed: string): number {
  let total = 0;

  for (let i = 0; i < seed.length; i++) {
    total = (total * 33 + seed.charCodeAt(i)) % 9973;
  }

  return total;
}

export function spiritGhostMotionForSeed(seed: string): SpiritGhostMotion {
  const hash = hashSpiritSeed(seed);
  const ghostDuration = 9.6 + (hash % 17) * 0.23;
  const ghostDelay = -((hash % 11) * 0.73);
  const mistInnerDuration = 7.1 + (hash % 13) * 0.19;
  const mistInnerDelay = -((hash % 9) * 0.61);
  const mistOuterDuration = 12.6 + (hash % 19) * 0.21;
  const mistOuterDelay = -((hash % 7) * 0.87);

  return {
    ghostDuration: `${ghostDuration.toFixed(2)}s`,
    ghostDelay: `${ghostDelay.toFixed(2)}s`,
    mistInnerDuration: `${mistInnerDuration.toFixed(2)}s`,
    mistInnerDelay: `${mistInnerDelay.toFixed(2)}s`,
    mistOuterDuration: `${mistOuterDuration.toFixed(2)}s`,
    mistOuterDelay: `${mistOuterDelay.toFixed(2)}s`,
  };
}

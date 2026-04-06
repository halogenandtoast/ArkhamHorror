import type { Asset } from '@/arkham/types/Asset';
import type { Enemy } from '@/arkham/types/Enemy';
import type { Game } from '@/arkham/types/Game';

export const THE_BEYOND_CARD_CODE = 'c90052';

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

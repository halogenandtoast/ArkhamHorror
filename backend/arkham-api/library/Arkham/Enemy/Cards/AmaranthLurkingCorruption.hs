module Arkham.Enemy.Cards.AmaranthLurkingCorruption (amaranthLurkingCorruption) where

import Arkham.Ability
import Arkham.Campaigns.TheScarletKeys.Helpers
import Arkham.Campaigns.TheScarletKeys.Key.Matcher
import Arkham.Card
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Query (getLead)
import Arkham.Helpers.Window
import Arkham.Matcher
import Arkham.Message (ReplaceStrategy (..))
import Arkham.Message.Lifted.Choose
import Arkham.Placement

newtype AmaranthLurkingCorruption = AmaranthLurkingCorruption EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

amaranthLurkingCorruption :: EnemyCard AmaranthLurkingCorruption
amaranthLurkingCorruption = enemy AmaranthLurkingCorruption Cards.amaranthLurkingCorruption (3, PerPlayer 3, 4) (1, 2)

instance HasAbilities AmaranthLurkingCorruption where
  getAbilities (AmaranthLurkingCorruption a) =
    extend1 a $ mkAbility a 1 $ forced $ AssetDealtDamageOrHorror #when (SourceIsEnemy $ be a) AnyAsset

instance RunMessage AmaranthLurkingCorruption where
  runMessage msg e@(AmaranthLurkingCorruption attrs) = runQueueT $ case msg of
    Flip _ _ (isTarget attrs -> True) -> do
      corruptionRevealed <- genCard Cards.amaranthCorruptionRevealed
      push $ ReplaceEnemy attrs.id corruptionRevealed Swap
      pure e
    UseCardAbility _ (isSource attrs -> True) 1 (damagedAsset -> asset) _ -> do
      assetDefeated (attrs.ability 1) asset
      skeys <- select $ ScarletKeyWithPlacement (AttachedToEnemy attrs.id)
      lead <- getLead
      chooseOrRunOneAtATimeM lead $ targets skeys shift
      pure e
    _ -> AmaranthLurkingCorruption <$> liftRunMessage msg attrs

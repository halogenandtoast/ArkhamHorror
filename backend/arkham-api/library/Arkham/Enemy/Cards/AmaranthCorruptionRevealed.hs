module Arkham.Enemy.Cards.AmaranthCorruptionRevealed (amaranthCorruptionRevealed) where

import Arkham.Ability
import Arkham.Campaigns.TheScarletKeys.Helpers
import Arkham.Campaigns.TheScarletKeys.Key.Matcher
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyAttacks)
import Arkham.Helpers.Query (getLead)
import Arkham.Helpers.Window (damagedAsset)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Placement

newtype AmaranthCorruptionRevealed = AmaranthCorruptionRevealed EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

amaranthCorruptionRevealed :: EnemyCard AmaranthCorruptionRevealed
amaranthCorruptionRevealed = enemy AmaranthCorruptionRevealed Cards.amaranthCorruptionRevealed (4, PerPlayer 6, 4) (2, 1)

instance HasAbilities AmaranthCorruptionRevealed where
  getAbilities (AmaranthCorruptionRevealed a) =
    extend
      a
      [ mkAbility a 1 $ forced $ EnemyAttacks #when Anyone AnyEnemyAttack (be a)
      , mkAbility a 2 $ silent $ AssetDealtDamageOrHorror #when (SourceIsEnemy $ be a) AnyAsset
      ]

instance RunMessage AmaranthCorruptionRevealed where
  runMessage msg e@(AmaranthCorruptionRevealed attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      skeys <- select $ ScarletKeyWithPlacement (AttachedToEnemy attrs.id)
      lead <- getLead
      chooseOrRunOneAtATimeM lead $ targets skeys shift
      pure e
    UseCardAbility _ (isSource attrs -> True) 2 (damagedAsset -> asset) _ -> do
      assetDefeated (attrs.ability 1) asset
      pure e
    _ -> AmaranthCorruptionRevealed <$> liftRunMessage msg attrs

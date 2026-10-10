module Arkham.Homebrew.AgesUnwound.Enemies.HoundOfUnmaking (houndOfUnmaking) where

import Arkham.Ability
import Arkham.Enemy.Import.Lifted hiding (EnemyAttacks)
import Arkham.Homebrew.AgesUnwound.CardDefs.Enemies qualified as Cards
import Arkham.Homebrew.AgesUnwound.Helpers (campaignI18n)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier

newtype HoundOfUnmaking = HoundOfUnmaking EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | /Time Ends Here/. Massive and Retaliate are on the card def.
houndOfUnmaking :: EnemyCard HoundOfUnmaking
houndOfUnmaking = enemy HoundOfUnmaking Cards.houndOfUnmaking

{- | "Forced - When Hound of Unmaking attacks you during the enemy phase: Either
discard an asset you control or the attack deals you +2 damage. /
Forced - At the end of the round: Heal 1 damage from Hound of Unmaking."
-}
instance HasAbilities HoundOfUnmaking where
  getAbilities (HoundOfUnmaking a) =
    extend
      a
      [ restricted a 1 (DuringPhase #enemy) $ forced $ EnemyAttacks #when You AnyEnemyAttack (be a)
      , restricted a 2 (thisExists a $ EnemyWithDamage (atLeast 1)) $ forced $ RoundEnds #when
      ]

instance RunMessage HoundOfUnmaking where
  runMessage msg e@(HoundOfUnmaking attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      canDiscard <- selectAny $ assetControlledBy iid <> DiscardableAsset
      chooseOrRunOneM iid $ campaignI18n do
        when canDiscard
          $ labeled "houndOfUnmaking.discardAsset"
          $ chooseAndDiscardAssetMatching iid (attrs.ability 1) (assetControlledBy iid <> DiscardableAsset)
        labeled "houndOfUnmaking.takeExtraDamage"
          $ enemyAttackModifier (attrs.ability 1) attrs (DamageDealt 2)
      pure e
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      healDamage attrs (attrs.ability 2) 1
      pure e
    _ -> HoundOfUnmaking <$> liftRunMessage msg attrs

module Arkham.Homebrew.AgesUnwound.Enemies.TheMyriadGentlemanMasterOfTheHouse (
  theMyriadGentlemanMasterOfTheHouse,
) where

import Arkham.Ability
import Arkham.Classes.HasGame
import Arkham.Enemy.Import.Lifted hiding (EnemyAttacks)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelfWhen)
import Arkham.Homebrew.AgesUnwound.CardDefs.Enemies qualified as Cards
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.AgesUnwound.Scenarios.TheMyriadGentleman.Helpers
import Arkham.Homebrew.AgesUnwound.Traits (pattern Myriad)
import Arkham.I18n
import Arkham.Matcher

newtype TheMyriadGentlemanMasterOfTheHouse = TheMyriadGentlemanMasterOfTheHouse EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theMyriadGentlemanMasterOfTheHouse :: EnemyCard TheMyriadGentlemanMasterOfTheHouse
theMyriadGentlemanMasterOfTheHouse =
  enemy TheMyriadGentlemanMasterOfTheHouse Cards.theMyriadGentleman_043

-- | "The Myriad Gentleman cannot leave Study."
instance HasModifiersFor TheMyriadGentlemanMasterOfTheHouse where
  getModifiersFor (TheMyriadGentlemanMasterOfTheHouse a) = do
    atStudy <- a.id <=~> EnemyAt (locationIs Locations.study)
    modifySelfWhen a atStudy [CannotMove, CannotBeMoved]

{- | "Forced - When The Myriad Gentleman attacks during the enemy phase: Instead
of its standard damage and horror, it deals X damage and\/or horror, where X is
the number of [[Myriad]] enemies in play (max 5)."
-}
instance HasAbilities TheMyriadGentlemanMasterOfTheHouse where
  getAbilities (TheMyriadGentlemanMasterOfTheHouse a) =
    extend1 a
      $ restricted a 1 (DuringPhase #enemy)
      $ forced
      $ EnemyAttacks #when You AnyEnemyAttack (be a)

instance RunMessage TheMyriadGentlemanMasterOfTheHouse where
  runMessage msg e@(TheMyriadGentlemanMasterOfTheHouse attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      x <- getMyriadAttackAmount
      {- "X damage and/or horror" is X points split between the two, not X of each
      -- X of each would be a flat 10 at four players. The attacked investigator
      -- divides, which is the only seat the card names. -}
      chooseAmounts
        iid
        (scenarioI18n $ ikey' "label.divideMyriadAttack")
        (TotalAmountTarget x)
        [("$damage", (0, x)), ("$horror", (0, x))]
        attrs
      pure e
    ResolveAmounts _ choices (isTarget attrs -> True) -> do
      let damage = getChoiceAmount "$damage" choices
      let horror = getChoiceAmount "$horror" choices
      -- Printed damage and horror are 1 each, and the modifier is additive.
      enemyAttackModifiers (attrs.ability 1) attrs [DamageDealt (damage - 1), HorrorDealt (horror - 1)]
      pure e
    _ -> TheMyriadGentlemanMasterOfTheHouse <$> liftRunMessage msg attrs

getMyriadAttackAmount :: HasGame m => m Int
getMyriadAttackAmount = min 5 <$> selectCount (EnemyWithTrait Myriad)

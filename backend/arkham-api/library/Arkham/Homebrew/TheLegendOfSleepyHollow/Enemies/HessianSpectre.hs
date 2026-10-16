module Arkham.Homebrew.TheLegendOfSleepyHollow.Enemies.HessianSpectre (hessianSpectre) where

import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelfWhen)
import Arkham.Homebrew.TheLegendOfSleepyHollow.CardDefs.Enemies qualified as Cards
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher

newtype HessianSpectre = HessianSpectre EnemyAttrs
  deriving anyclass (IsEnemy, RunMessage)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

hessianSpectre :: EnemyCard HessianSpectre
hessianSpectre = enemy HessianSpectre Cards.hessianSpectre

{- | "If an investigator engaged with Hessian Spectre has 2 or fewer cards in his
or her hand, Hessian Spectre gets +1 damage and retaliate."
-}
instance HasModifiersFor HessianSpectre where
  getModifiersFor (HessianSpectre a) = do
    starving <- selectAny $ investigatorEngagedWith a <> HandWith (LengthIs $ atMost 2)
    modifySelfWhen a starving [DamageDealt 1, AddKeyword Keyword.Retaliate]

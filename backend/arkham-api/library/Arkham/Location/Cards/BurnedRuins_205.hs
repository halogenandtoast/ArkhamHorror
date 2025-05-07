module Arkham.Location.Cards.BurnedRuins_205 (burnedRuins_205) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (burnedRuins_205)
import Arkham.Location.Helpers (drawCardUnderneathAction)
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype BurnedRuins_205 = BurnedRuins_205 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

burnedRuins_205 :: LocationCard BurnedRuins_205
burnedRuins_205 = location BurnedRuins_205 Cards.burnedRuins_205 2 (Static 3)

instance HasAbilities BurnedRuins_205 where
  getAbilities (BurnedRuins_205 a) =
    extendRevealed
      a
      [ drawCardUnderneathAction a
      , mkAbility a 1 $ forced $ SkillTestResult #after You (WhileInvestigating $ be a) #failure
      ]

instance RunMessage BurnedRuins_205 where
  runMessage msg l@(BurnedRuins_205 attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      when (attrs.clues > 0) do
        removeClues (attrs.ability 1) attrs 1
        placeDoom (attrs.ability 1) attrs 1
      pure l
    _ -> BurnedRuins_205 <$> liftRunMessage msg attrs

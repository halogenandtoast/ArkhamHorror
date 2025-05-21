module Arkham.Location.Cards.ChoeurGothique_292 (choeurGothique_292) where

import Arkham.Ability
import Arkham.Agenda.Sequence (AgendaSide (A))
import Arkham.GameValue
import Arkham.Helpers.Investigator
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype ChoeurGothique_292 = ChoeurGothique_292 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

choeurGothique_292 :: LocationCard ChoeurGothique_292
choeurGothique_292 = location ChoeurGothique_292 Cards.choeurGothique_292 3 (PerPlayer 1)

instance HasAbilities ChoeurGothique_292 where
  getAbilities (ChoeurGothique_292 a) =
    extendRevealed1 a
      $ groupLimit PerGame
      $ restricted a 1 (Here <> exists (HealableInvestigator (toSource a) #damage You))
      $ actionAbilityWithCost (DoomCost (toSource a) (AgendaMatcherTarget $ AgendaWithSide A) 1)

instance RunMessage ChoeurGothique_292 where
  runMessage msg l@(ChoeurGothique_292 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      canHeal <- canHaveDamageHealed (attrs.ability 1) iid
      when canHeal $ healDamage iid (toSource attrs) 2
      pure l
    _ -> ChoeurGothique_292 <$> liftRunMessage msg attrs

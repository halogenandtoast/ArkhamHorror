module Arkham.Location.Cards.ChoeurGothique_293 (choeurGothique_293, ChoeurGothique_293 (..)) where

import Arkham.Ability
import Arkham.Agenda.Sequence (AgendaSide (C))
import Arkham.GameValue
import Arkham.Helpers.Investigator
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype ChoeurGothique_293 = ChoeurGothique_293 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

choeurGothique_293 :: LocationCard ChoeurGothique_293
choeurGothique_293 = location ChoeurGothique_293 Cards.choeurGothique_293 3 (PerPlayer 1)

instance HasAbilities ChoeurGothique_293 where
  getAbilities (ChoeurGothique_293 a) =
    extendRevealed1 a
      $ groupLimit PerGame
      $ restricted a 1 (Here <> exists (HealableInvestigator (a.ability 1) #horror You))
      $ actionAbilityWithCost (DoomCost (toSource a) (AgendaMatcherTarget $ AgendaWithSide C) 1)

instance RunMessage ChoeurGothique_293 where
  runMessage msg l@(ChoeurGothique_293 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      canHeal <- canHaveHorrorHealed (attrs.ability 1) iid
      when canHeal $ healHorror iid (attrs.ability 1) 2
      pure l
    _ -> ChoeurGothique_293 <$> liftRunMessage msg attrs

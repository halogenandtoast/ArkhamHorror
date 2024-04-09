module Arkham.Location.Cards.ChoeurGothique_293 (
  choeurGothique_293,
  ChoeurGothique_293 (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Agenda.Sequence (AgendaSide (C))
import Arkham.Classes
import Arkham.Damage
import Arkham.GameValue
import Arkham.Helpers.Investigator
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher

newtype ChoeurGothique_293 = ChoeurGothique_293 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

choeurGothique_293 :: LocationCard ChoeurGothique_293
choeurGothique_293 =
  location ChoeurGothique_293 Cards.choeurGothique_293 3 (PerPlayer 1)

instance HasAbilities ChoeurGothique_293 where
  getAbilities (ChoeurGothique_293 a) =
    withBaseAbilities
      a
      [ limitedAbility (GroupLimit PerGame 1)
          $ restrictedAbility
            a
            1
            ( Here
                <> InvestigatorExists
                  (HealableInvestigator (toSource a) HorrorType You)
            )
          $ ActionAbility []
          $ ActionCost 1
          <> DoomCost (toSource a) (AgendaMatcherTarget $ AgendaWithSide C) 1
      ]

instance RunMessage ChoeurGothique_293 where
  runMessage msg l@(ChoeurGothique_293 attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      canHeal <- canHaveHorrorHealed (attrs.ability 1) iid
      pushWhen canHeal $ HealHorror (toTarget iid) (attrs.ability 1) 2
      pure l
    _ -> ChoeurGothique_293 <$> runMessage msg attrs

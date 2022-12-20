module Arkham.Location.Cards.ChoeurGothique_292
  ( choeurGothique_292
  , ChoeurGothique_292(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Agenda.Sequence ( AgendaSide (A) )
import Arkham.Classes
import Arkham.Cost
import Arkham.Criteria
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Target

newtype ChoeurGothique_292 = ChoeurGothique_292 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

choeurGothique_292 :: LocationCard ChoeurGothique_292
choeurGothique_292 =
  location ChoeurGothique_292 Cards.choeurGothique_292 3 (PerPlayer 1)

instance HasAbilities ChoeurGothique_292 where
  getAbilities (ChoeurGothique_292 a) = withBaseAbilities
    a
    [ limitedAbility (GroupLimit PerGame 1)
      $ restrictedAbility
          a
          1
          (Here <> InvestigatorExists (You <> InvestigatorWithAnyDamage))
      $ ActionAbility Nothing
      $ ActionCost 1
      <> DoomCost (toSource a) (AgendaMatcherTarget $ AgendaWithSide A) 1
    ]

instance RunMessage ChoeurGothique_292 where
  runMessage msg l@(ChoeurGothique_292 attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source ->
      l <$ push (HealDamage (InvestigatorTarget iid) (toSource attrs) 2)
    _ -> ChoeurGothique_292 <$> runMessage msg attrs

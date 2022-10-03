module Arkham.Location.Cards.LeMarais218
  ( leMarais218
  , LeMarais218(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Card
import Arkham.Classes
import Arkham.Criteria
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Target
import Arkham.Timing qualified as Timing

newtype LeMarais218 = LeMarais218 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

leMarais218 :: LocationCard LeMarais218
leMarais218 = location LeMarais218 Cards.leMarais218 1 (PerPlayer 1)

instance HasAbilities LeMarais218 where
  getAbilities (LeMarais218 attrs) = withBaseAbilities
    attrs
    [ restrictedAbility
        attrs
        1
        Here
        (ForcedAbility $ Enters Timing.After You $ LocationWithId $ toId attrs)
    | locationRevealed attrs
    ]

instance RunMessage LeMarais218 where
  runMessage msg l@(LeMarais218 attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      push $ CreateEffect
        (toCardCode attrs)
        Nothing
        source
        (InvestigatorTarget iid)
      pure l
    _ -> LeMarais218 <$> runMessage msg attrs

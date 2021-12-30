module Arkham.Location.Cards.TwistedUnderbrush
  ( TwistedUnderbrush(..)
  , twistedUnderbrush
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards (twistedUnderbrush)
import Arkham.Classes
import Arkham.Cost
import Arkham.Criteria
import Arkham.GameValue
import Arkham.Location.Attrs
import Arkham.Location.Helpers
import Arkham.Message

newtype TwistedUnderbrush = TwistedUnderbrush LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

twistedUnderbrush :: LocationCard TwistedUnderbrush
twistedUnderbrush = location
  TwistedUnderbrush
  Cards.twistedUnderbrush
  3
  (PerPlayer 1)
  Moon
  [Diamond, Moon]

instance HasAbilities TwistedUnderbrush where
  getAbilities (TwistedUnderbrush attrs) =
    withBaseAbilities attrs $
      [ restrictedAbility attrs 1 Here $ ActionAbility Nothing $ ActionCost 1
      | locationRevealed attrs
      ]

instance (LocationRunner env) => RunMessage env TwistedUnderbrush where
  runMessage msg l@(TwistedUnderbrush attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> l <$ pushAll
      [ TakeResources iid 2 False
      , InvestigatorAssignDamage iid source DamageAny 0 1
      ]
    _ -> TwistedUnderbrush <$> runMessage msg attrs

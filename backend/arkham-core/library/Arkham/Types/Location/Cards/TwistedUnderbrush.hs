module Arkham.Types.Location.Cards.TwistedUnderbrush
  ( TwistedUnderbrush(..)
  , twistedUnderbrush
  ) where

import Arkham.Prelude

import Arkham.Location.Cards qualified as Cards (twistedUnderbrush)
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Message

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

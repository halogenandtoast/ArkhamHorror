module Arkham.Location.Cards.TwistedUnderbrush
  ( TwistedUnderbrush(..)
  , twistedUnderbrush
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards ( twistedUnderbrush )
import Arkham.Location.Helpers
import Arkham.Location.Runner

newtype TwistedUnderbrush = TwistedUnderbrush LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

twistedUnderbrush :: LocationCard TwistedUnderbrush
twistedUnderbrush =
  location TwistedUnderbrush Cards.twistedUnderbrush 3 (PerPlayer 1)

instance HasAbilities TwistedUnderbrush where
  getAbilities (TwistedUnderbrush attrs) =
    withBaseAbilities attrs
      $ [ restrictedAbility attrs 1 Here $ ActionAbility Nothing $ ActionCost 1
        | locationRevealed attrs
        ]

instance RunMessage TwistedUnderbrush where
  runMessage msg l@(TwistedUnderbrush attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> l <$ pushAll
      [ TakeResources iid 2 (toAbilitySource attrs 1) False
      , InvestigatorAssignDamage iid source DamageAny 0 1
      ]
    _ -> TwistedUnderbrush <$> runMessage msg attrs

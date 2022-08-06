module Arkham.Location.Cards.Attic where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Game.Helpers
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards ( attic )
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Timing qualified as Timing

newtype Attic = Attic LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

attic :: LocationCard Attic
attic = location Attic Cards.attic 1 (PerPlayer 2)

instance HasAbilities Attic where
  getAbilities (Attic a) =
    withBaseAbilities a
      $ [ mkAbility a 1
          $ ForcedAbility
          $ Enters Timing.After You
          $ LocationWithId
          $ toId a
        ]

instance RunMessage Attic where
  runMessage msg a@(Attic attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      a <$ push (InvestigatorAssignDamage iid source DamageAny 0 1)
    _ -> Attic <$> runMessage msg attrs

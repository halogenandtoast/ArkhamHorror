module Arkham.Location.Cards.Cellar where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Game.Helpers
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards ( cellar )
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Timing qualified as Timing

newtype Cellar = Cellar LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cellar :: LocationCard Cellar
cellar = location Cellar Cards.cellar 4 (PerPlayer 2)

instance HasAbilities Cellar where
  getAbilities (Cellar a) =
    withBaseAbilities a
      $ [ mkAbility a 1
          $ ForcedAbility
          $ Enters Timing.After You
          $ LocationWithId
          $ toId a
        ]

instance RunMessage Cellar where
  runMessage msg a@(Cellar attrs) = case msg of
    UseCardAbility iid (isAbility attrs 1 -> True) _ _ ->
      a <$ push (InvestigatorAssignDamage iid (toSource attrs) DamageAny 1 0)
    _ -> Cellar <$> runMessage msg attrs

module Arkham.Types.Location.Cards.Attic where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (attic)
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Game.Helpers
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Matcher
import Arkham.Types.Message
import qualified Arkham.Types.Timing as Timing

newtype Attic = Attic LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

attic :: LocationCard Attic
attic = location Attic Cards.attic 1 (PerPlayer 2) Triangle [Square]

instance HasAbilities Attic where
  getAbilities (Attic a) = withBaseAbilities a $
    [ mkAbility a 1
      $ ForcedAbility
      $ Enters Timing.After You
      $ LocationWithId
      $ toId a
    ]

instance LocationRunner env => RunMessage env Attic where
  runMessage msg a@(Attic attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      a <$ push (InvestigatorAssignDamage iid source DamageAny 0 1)
    _ -> Attic <$> runMessage msg attrs

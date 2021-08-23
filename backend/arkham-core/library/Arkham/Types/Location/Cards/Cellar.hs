module Arkham.Types.Location.Cards.Cellar where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (cellar)
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Game.Helpers
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationSymbol
import Arkham.Types.Matcher
import Arkham.Types.Message
import qualified Arkham.Types.Timing as Timing

newtype Cellar = Cellar LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cellar :: LocationCard Cellar
cellar = location Cellar Cards.cellar 4 (PerPlayer 2) Plus [Square]

instance HasAbilities env Cellar where
  getAbilities i window (Cellar a) = withBaseAbilities i window a $ pure
    [ mkAbility a 1
      $ ForcedAbility
      $ Enters Timing.After You
      $ LocationWithId
      $ toId a
    ]

instance LocationRunner env => RunMessage env Cellar where
  runMessage msg a@(Cellar attrs@LocationAttrs {..}) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      a <$ push (InvestigatorAssignDamage iid (toSource attrs) DamageAny 1 0)
    _ -> Cellar <$> runMessage msg attrs

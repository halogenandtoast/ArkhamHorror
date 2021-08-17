module Arkham.Types.Location.Cards.SlaughteredWoods
  ( slaughteredWoods
  , SlaughteredWoods(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (slaughteredWoods)
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationSymbol
import Arkham.Types.Message hiding (RevealLocation)
import Arkham.Types.Query
import qualified Arkham.Types.Timing as Timing
import Arkham.Types.Window

newtype SlaughteredWoods = SlaughteredWoods LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

slaughteredWoods :: LocationCard SlaughteredWoods
slaughteredWoods = locationWith
  SlaughteredWoods
  Cards.slaughteredWoods
  2
  (PerPlayer 1)
  NoSymbol
  []
  ((revealedSymbolL .~ Plus)
  . (revealedConnectedSymbolsL .~ setFromList [Triangle, Hourglass])
  )

instance HasModifiersFor env SlaughteredWoods

forcedAbility :: LocationAttrs -> Ability
forcedAbility a = mkAbility (toSource a) 1 LegacyForcedAbility

instance ActionRunner env => HasAbilities env SlaughteredWoods where
  getAbilities iid (Window Timing.After (RevealLocation who _)) (SlaughteredWoods attrs)
    | iid == who
    = do
      actionRemainingCount <- unActionRemainingCount <$> getCount iid
      pure [ locationAbility (forcedAbility attrs) | actionRemainingCount == 0 ]
  getAbilities iid window (SlaughteredWoods attrs) =
    getAbilities iid window attrs

instance LocationRunner env => RunMessage env SlaughteredWoods where
  runMessage msg l@(SlaughteredWoods attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      l <$ push (InvestigatorAssignDamage iid source DamageAny 0 2)
    _ -> SlaughteredWoods <$> runMessage msg attrs

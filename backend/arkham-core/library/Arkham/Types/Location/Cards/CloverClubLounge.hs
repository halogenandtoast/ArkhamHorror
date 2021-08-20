module Arkham.Types.Location.Cards.CloverClubLounge
  ( cloverClubLounge
  , CloverClubLounge(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (cloverClubLounge)
import Arkham.Types.Ability
import Arkham.Types.ActId
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Game.Helpers
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import qualified Arkham.Types.Timing as Timing
import Arkham.Types.Trait
import Arkham.Types.Window

newtype CloverClubLounge = CloverClubLounge LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cloverClubLounge :: LocationCard CloverClubLounge
cloverClubLounge = location
  CloverClubLounge
  Cards.cloverClubLounge
  2
  (Static 0)
  Circle
  [Moon, Square, Triangle]

ability :: LocationAttrs -> Ability
ability attrs =
  (mkAbility
      (toSource attrs)
      1
      (ActionAbility Nothing
      $ Costs
          [ ActionCost 1
          , HandDiscardCost 1 (Just AssetType) (singleton Ally) mempty
          ]
      )
    )
    { abilityLimit = PlayerLimit PerGame 1
    }

instance ActionRunner env => HasAbilities env CloverClubLounge where
  getAbilities iid window@(Window Timing.When NonFast) (CloverClubLounge attrs@LocationAttrs {..})
    | locationRevealed
    = withBaseActions iid window attrs $ do
      step <- unActStep <$> getStep ()
      pure [ locationAbility (ability attrs) | step == 1 ]
  getAbilities iid window (CloverClubLounge attrs) =
    getAbilities iid window attrs

instance LocationRunner env => RunMessage env CloverClubLounge where
  runMessage msg l@(CloverClubLounge attrs@LocationAttrs {..}) = case msg of
    UseCardAbility iid source _ 1 _
      | isSource attrs source && locationRevealed -> l <$ push (GainClues iid 2)
    _ -> CloverClubLounge <$> runMessage msg attrs

module Arkham.Types.Location.Cards.CloverClubLounge
  ( cloverClubLounge
  , CloverClubLounge(..)
  )
where

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
import Arkham.Types.LocationId
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Trait
import Arkham.Types.Window

newtype CloverClubLounge = CloverClubLounge LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cloverClubLounge :: LocationId -> CloverClubLounge
cloverClubLounge = CloverClubLounge . baseAttrs
  Cards.cloverClubLounge
  2
  (Static 0)
  Circle
  [Moon, Square, Triangle]

instance HasModifiersFor env CloverClubLounge where
  getModifiersFor = noModifiersFor

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

instance ActionRunner env => HasActions env CloverClubLounge where
  getActions iid NonFast (CloverClubLounge attrs@LocationAttrs {..})
    | locationRevealed = withBaseActions iid NonFast attrs $ do
      step <- unActStep <$> getStep
      pure
        [ ActivateCardAbilityAction iid (ability attrs)
        | iid `member` locationInvestigators && step == 1
        ]
  getActions iid window (CloverClubLounge attrs) = getActions iid window attrs

instance LocationRunner env => RunMessage env CloverClubLounge where
  runMessage msg l@(CloverClubLounge attrs@LocationAttrs {..}) = case msg of
    UseCardAbility iid source _ 1 _
      | isSource attrs source && locationRevealed -> l
      <$ unshiftMessage (GainClues iid 2)
    _ -> CloverClubLounge <$> runMessage msg attrs

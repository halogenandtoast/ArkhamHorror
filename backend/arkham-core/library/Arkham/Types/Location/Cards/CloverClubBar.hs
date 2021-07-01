module Arkham.Types.Location.Cards.CloverClubBar
  ( cloverClubBar
  , CloverClubBar(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (cloverClubBar)
import Arkham.Types.Ability
import Arkham.Types.ActId
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Game.Helpers
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationId
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.ScenarioLogKey
import Arkham.Types.Window

newtype CloverClubBar = CloverClubBar LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cloverClubBar :: LocationId -> CloverClubBar
cloverClubBar = CloverClubBar . baseAttrs
  Cards.cloverClubBar
  3
  (Static 0)
  Square
  [Triangle, Circle]

instance HasModifiersFor env CloverClubBar where
  getModifiersFor = noModifiersFor

ability :: LocationAttrs -> Ability
ability attrs = (mkAbility
                  (toSource attrs)
                  1
                  (ActionAbility Nothing $ Costs [ActionCost 1, ResourceCost 2]
                  )
                )
  { abilityLimit = PlayerLimit PerGame 1
  }

instance ActionRunner env => HasActions env CloverClubBar where
  getActions iid NonFast (CloverClubBar attrs@LocationAttrs {..})
    | locationRevealed = withBaseActions iid NonFast attrs $ do
      step <- unActStep <$> getStep
      pure
        [ ActivateCardAbilityAction iid (ability attrs)
        | iid `member` locationInvestigators && step == 1
        ]
  getActions iid window (CloverClubBar attrs) = getActions iid window attrs

instance LocationRunner env => RunMessage env CloverClubBar where
  runMessage msg l@(CloverClubBar attrs@LocationAttrs {..}) = case msg of
    UseCardAbility iid source _ 1 _
      | isSource attrs source && locationRevealed -> l <$ unshiftMessages
        [GainClues iid 2, DrawCards iid 2 False, Remember $ HadADrink iid]
    _ -> CloverClubBar <$> runMessage msg attrs

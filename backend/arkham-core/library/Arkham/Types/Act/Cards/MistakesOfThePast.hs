module Arkham.Types.Act.Cards.MistakesOfThePast
  ( MistakesOfThePast(..)
  , mistakesOfThePast
  ) where

import Arkham.Prelude

import qualified Arkham.Act.Cards as Cards
import qualified Arkham.Asset.Cards as Assets
import qualified Arkham.Location.Cards as Locations
import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Helpers
import Arkham.Types.Act.Runner
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Matcher
import Arkham.Types.Message

newtype MistakesOfThePast = MistakesOfThePast ActAttrs
  deriving anyclass (IsAct, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mistakesOfThePast :: ActCard MistakesOfThePast
mistakesOfThePast = act
  (2, A)
  MistakesOfThePast
  Cards.mistakesOfThePast
  (Just $ GroupClueCost (PerPlayer 2) Anywhere)

instance ActRunner env => RunMessage env MistakesOfThePast where
  runMessage msg a@(MistakesOfThePast attrs) = case msg of
    AdvanceAct aid _ | aid == toId a && onSide B attrs -> do
      locations <- selectList $ RevealedLocation <> LocationWithTitle
        "Historical Society"
      hiddenLibrary <- getSetAsideCard Locations.hiddenLibrary
      mrPeabody <- getSetAsideCard Assets.mrPeabody
      leadInvestigatorId <- getLeadInvestigatorId
      investigatorIds <- getInvestigatorIds
      playerCount <- getPlayerCount
      a <$ pushAll
        ([ PlaceCluesUpToClueValue location playerCount
         | location <- locations
         ]
        <> [ chooseOne
             leadInvestigatorId
             [ TakeControlOfSetAsideAsset iid mrPeabody
             | iid <- investigatorIds
             ]
           , PlaceLocation hiddenLibrary
           , NextAct aid "03125"
           ]
        )
    _ -> MistakesOfThePast <$> runMessage msg attrs

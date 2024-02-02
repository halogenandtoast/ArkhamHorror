module Arkham.Act.Cards.MistakesOfThePast (
  MistakesOfThePast (..),
  mistakesOfThePast,
) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Helpers
import Arkham.Act.Runner
import Arkham.Asset.Cards qualified as Assets
import Arkham.Classes
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher

newtype MistakesOfThePast = MistakesOfThePast ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData, HasAbilities)

mistakesOfThePast :: ActCard MistakesOfThePast
mistakesOfThePast =
  act
    (2, A)
    MistakesOfThePast
    Cards.mistakesOfThePast
    (Just $ GroupClueCost (PerPlayer 2) Anywhere)

instance RunMessage MistakesOfThePast where
  runMessage msg a@(MistakesOfThePast attrs) = case msg of
    AdvanceAct aid _ _ | aid == toId a && onSide B attrs -> do
      locations <-
        selectList
          $ RevealedLocation
          <> LocationWithTitle
            "Historical Society"
      mrPeabody <- getSetAsideCard Assets.mrPeabody
      lead <- getLeadPlayer
      investigatorIds <- getInvestigatorIds
      playerCount <- getPlayerCount
      placeHiddenLibrary <- placeSetAsideLocation_ Locations.hiddenLibrary
      pushAll
        $ [ PlaceCluesUpToClueValue location (toSource attrs) playerCount
          | location <- locations
          ]
        <> [ chooseOne
              lead
              [ targetLabel iid [TakeControlOfSetAsideAsset iid mrPeabody]
              | iid <- investigatorIds
              ]
           , placeHiddenLibrary
           , AdvanceActDeck (actDeckId attrs) (toSource attrs)
           ]
      pure a
    _ -> MistakesOfThePast <$> runMessage msg attrs

module Arkham.Act.Cards.SearchForAlejandro
  ( SearchForAlejandro(..)
  , searchForAlejandro
  ) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Asset.Cards qualified as Assets
import Arkham.Card
import Arkham.Classes
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message
import Arkham.Placement

newtype SearchForAlejandro = SearchForAlejandro ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

searchForAlejandro :: ActCard SearchForAlejandro
searchForAlejandro =
  act (1, C) SearchForAlejandro Cards.searchForAlejandro
    $ Just
    $ GroupClueCost (PerPlayer 1)
    $ LocationWithTitle "Easttown"

instance RunMessage SearchForAlejandro where
  runMessage msg a@(SearchForAlejandro attrs) = case msg of
    AdvanceAct aid _ _ | aid == toId attrs && onSide D attrs -> do
      velmasDiner <- selectJust $ locationIs Locations.velmasDiner
      henryDeveau <- genCard Assets.henryDeveau
      assetId <- getRandom
      pushAll
        [ CreateAssetAt assetId henryDeveau (AtLocation velmasDiner)
        , AdvanceActDeck (actDeckId attrs) (toSource attrs)
        ]
      pure a
    _ -> SearchForAlejandro <$> runMessage msg attrs

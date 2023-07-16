module Arkham.Act.Cards.TheUnvisitedIsle (
  TheUnvisitedIsle (..),
  theUnvisitedIsle,
) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.CampaignLogKey
import Arkham.Classes
import Arkham.Field
import Arkham.Helpers.Log
import Arkham.Helpers.Query
import Arkham.Location.Brazier
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Message
import Arkham.Movement

newtype TheUnvisitedIsle = TheUnvisitedIsle ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

theUnvisitedIsle :: ActCard TheUnvisitedIsle
theUnvisitedIsle = act (1, A) TheUnvisitedIsle Cards.theUnvisitedIsle (Just $ GroupClueCost (PerPlayer 3) Anywhere)

instance RunMessage TheUnvisitedIsle where
  runMessage msg a@(TheUnvisitedIsle attrs) = case msg of
    AdvanceAct aid _ _ | aid == toId attrs && onSide B attrs -> do
      -- cards <- shuffleM <$> selectList (SetAsideCardMatch $ CardWithTitle "Unvisited Isle")
      -- pairs <- zip cards <$> getInvestigators
      -- sidedWithTheCoven <- getHasRecord TheInvestigatorsSidedWithTheCoven
      -- for_ pairs $ \(unvisitedIsle, investigator) -> do
      --   (lid, placement) <- placeLocation unvisitedIsle
      --   pushAll $
      --     placement
      --       : [PutLocationInFrontOf investigator lid, MoveTo $ move attrs investigator lid]
      --         <> [UpdateLocation lid (LocationBrazier ?=. Lit) | sidedWithTheCoven]

      pure a
    _ -> TheUnvisitedIsle <$> runMessage msg attrs

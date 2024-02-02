module Arkham.Act.Cards.FindingAWayInside (
  FindingAWayInside (..),
  findingAWayInside,
) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Acts
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Helpers
import Arkham.Act.Runner
import Arkham.Asset.Cards qualified as Assets
import Arkham.Card
import Arkham.Classes
import Arkham.Matcher hiding (RevealLocation)

newtype FindingAWayInside = FindingAWayInside ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData, HasAbilities)

findingAWayInside :: ActCard FindingAWayInside
findingAWayInside =
  act
    (1, A)
    FindingAWayInside
    Cards.findingAWayInside
    (Just $ GroupClueCost (Static 2) Anywhere)

instance RunMessage FindingAWayInside where
  runMessage msg a@(FindingAWayInside attrs@ActAttrs {..}) = case msg of
    AdvanceAct aid source@(LocationSource _) advanceMode | aid == actId && onSide A attrs -> do
      -- When advanced from Museum Halls we don't spend clues
      lead <- getLeadPlayer
      push $ chooseOne lead [targetLabel aid [AdvanceAct aid source advanceMode]]
      pure $ FindingAWayInside $ attrs & sequenceL .~ Sequence 1 B
    AdvanceAct aid _ _ | aid == actId && onSide A attrs -> do
      -- otherwise we do the default
      FindingAWayInside <$> runMessage msg attrs
    AdvanceAct aid source _ | aid == actId && onSide B attrs && isSource attrs source -> do
      lead <- getLeadPlayer
      investigatorIds <- getInvestigatorIds
      adamLynch <- genCard Assets.adamLynch
      museumHallsId <-
        fromJustNote "missing museum halls"
          <$> selectOne (LocationWithTitle "Museum Halls")
      pushAll
        [ chooseOne
            lead
            [ targetLabel
              iid
              [TakeControlOfSetAsideAsset iid adamLynch]
            | iid <- investigatorIds
            ]
        , RevealLocation Nothing museumHallsId
        , AdvanceToAct actDeckId Acts.nightAtTheMuseum A (toSource attrs)
        ]
      pure a
    AdvanceAct aid _ _ | aid == actId && onSide B attrs -> do
      museumHallsId <-
        fromJustNote "missing museum halls"
          <$> selectOne (LocationWithTitle "Museum Halls")
      pushAll
        [ RevealLocation Nothing museumHallsId
        , AdvanceToAct actDeckId Acts.breakingAndEntering A (toSource attrs)
        ]
      pure a
    _ -> FindingAWayInside <$> runMessage msg attrs

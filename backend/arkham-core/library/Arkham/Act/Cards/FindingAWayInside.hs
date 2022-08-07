module Arkham.Act.Cards.FindingAWayInside
  ( FindingAWayInside(..)
  , findingAWayInside
  ) where

import Arkham.Prelude

import Arkham.Act.Types
import Arkham.Act.Cards qualified as Acts
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Helpers
import Arkham.Act.Runner
import Arkham.Asset.Cards qualified as Assets
import Arkham.Card
import Arkham.Card.EncounterCard
import Arkham.Classes
import Arkham.GameValue
import Arkham.Matcher hiding (RevealLocation)
import Arkham.Message
import Arkham.Source
import Arkham.Target

newtype FindingAWayInside = FindingAWayInside ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

findingAWayInside :: ActCard FindingAWayInside
findingAWayInside = act
  (1, A)
  FindingAWayInside
  Cards.findingAWayInside
  (Just $ GroupClueCost (Static 2) Anywhere)

instance RunMessage FindingAWayInside where
  runMessage msg a@(FindingAWayInside attrs@ActAttrs {..}) = case msg of
    AdvanceAct aid source@(LocationSource _) advanceMode | aid == actId && onSide A attrs ->
      do
      -- When advanced from Museum Halls we don't spend clues
        leadInvestigatorId <- getLeadInvestigatorId
        push (chooseOne leadInvestigatorId [AdvanceAct aid source advanceMode])
        pure $ FindingAWayInside $ attrs & sequenceL .~ Sequence 1 B
    AdvanceAct aid _ _ | aid == actId && onSide A attrs ->
      -- otherwise we do the default
      FindingAWayInside <$> runMessage msg attrs
    AdvanceAct aid source _
      | aid == actId && onSide B attrs && isSource attrs source -> do
        leadInvestigatorId <- getLeadInvestigatorId
        investigatorIds <- getInvestigatorIds
        adamLynch <- EncounterCard <$> genEncounterCard Assets.adamLynch
        museumHallsId <- fromJustNote "missing museum halls"
          <$> selectOne (LocationWithTitle "Museum Halls")
        a <$ pushAll
          [ chooseOne
            leadInvestigatorId
            [ TargetLabel
                (InvestigatorTarget iid)
                [TakeControlOfSetAsideAsset iid adamLynch]
            | iid <- investigatorIds
            ]
          , RevealLocation Nothing museumHallsId
          , AdvanceToAct actDeckId Acts.nightAtTheMuseum A (toSource attrs)
          ]
    AdvanceAct aid _ _ | aid == actId && onSide B attrs -> do
      museumHallsId <- fromJustNote "missing museum halls"
        <$> selectOne (LocationWithTitle "Museum Halls")
      a <$ pushAll
        [ RevealLocation Nothing museumHallsId
        , AdvanceToAct actDeckId Acts.breakingAndEntering A (toSource attrs)
        ]
    _ -> FindingAWayInside <$> runMessage msg attrs

module Arkham.Types.Act.Cards.FindingAWayInside
  ( FindingAWayInside(..)
  , findingAWayInside
  ) where

import Arkham.Prelude

import qualified Arkham.Act.Cards as Cards
import qualified Arkham.Asset.Cards as Assets
import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Helpers
import Arkham.Types.Act.Runner
import Arkham.Types.Card
import Arkham.Types.Card.EncounterCard
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Matcher hiding (RevealLocation)
import Arkham.Types.Message
import Arkham.Types.Source
import Arkham.Types.Target

newtype FindingAWayInside = FindingAWayInside ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasActions, HasModifiersFor env)

findingAWayInside :: ActCard FindingAWayInside
findingAWayInside = act
  (1, A)
  FindingAWayInside
  Cards.findingAWayInside
  (Just $ GroupClueCost (Static 2) Nothing)

instance ActRunner env => RunMessage env FindingAWayInside where
  runMessage msg a@(FindingAWayInside attrs@ActAttrs {..}) = case msg of
    AdvanceAct aid source@(LocationSource _) | aid == actId && onSide A attrs ->
      do
      -- When advanced from Museum Halls we don't spend clues
        leadInvestigatorId <- getLeadInvestigatorId
        push (chooseOne leadInvestigatorId [AdvanceAct aid source])
        pure $ FindingAWayInside $ attrs & sequenceL .~ Act 1 B
    AdvanceAct aid _ | aid == actId && onSide A attrs ->
      -- otherwise we do the default
      FindingAWayInside <$> runMessage msg attrs
    AdvanceAct aid source
      | aid == actId && onSide B attrs && isSource attrs source -> do
        leadInvestigatorId <- getLeadInvestigatorId
        investigatorIds <- getInvestigatorIds
        adamLynch <- EncounterCard <$> genEncounterCard Assets.adamLynch
        museumHallsId <- fromJustNote "missing museum halls"
          <$> getId (LocationWithTitle "Museum Halls")
        a <$ pushAll
          [ chooseOne
            leadInvestigatorId
            [ TargetLabel
                (InvestigatorTarget iid)
                [TakeControlOfSetAsideAsset iid adamLynch]
            | iid <- investigatorIds
            ]
          , RevealLocation Nothing museumHallsId
          , NextAct aid "02123"
          ]
    AdvanceAct aid _ | aid == actId && onSide B attrs -> do
      museumHallsId <- fromJustNote "missing museum halls"
        <$> getId (LocationWithTitle "Museum Halls")
      a <$ pushAll [RevealLocation Nothing museumHallsId, NextAct aid "02124"]
    _ -> FindingAWayInside <$> runMessage msg attrs

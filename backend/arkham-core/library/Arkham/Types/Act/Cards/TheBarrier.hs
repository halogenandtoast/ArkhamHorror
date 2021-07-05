module Arkham.Types.Act.Cards.TheBarrier where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Assets
import Arkham.EncounterCard
import qualified Arkham.Enemy.Cards as Enemies
import Arkham.PlayerCard
import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Helpers
import Arkham.Types.Act.Runner
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationMatcher
import Arkham.Types.Message

newtype TheBarrier = TheBarrier ActAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasModifiersFor env)

theBarrier :: TheBarrier
theBarrier = TheBarrier $ baseAttrs "01109" "The Barrier" (Act 2 A) Nothing

instance ActionRunner env => HasActions env TheBarrier where
  getActions i window (TheBarrier x) = getActions i window x

instance ActRunner env => RunMessage env TheBarrier where
  runMessage msg a@(TheBarrier attrs@ActAttrs {..}) = case msg of
    AdvanceAct aid _ | aid == actId && onSide A attrs -> do
      hallwayId <- fromJustNote "must exist"
        <$> getLocationIdWithTitle "Hallway"
      investigatorIds <- getSetList hallwayId
      requiredClueCount <- getPlayerCountValue (PerPlayer 3)
      unshiftMessages
        (SpendClues requiredClueCount investigatorIds
        : [ chooseOne iid [AdvanceAct aid $ toSource attrs]
          | iid <- investigatorIds
          ]
        )
      pure $ TheBarrier $ attrs & sequenceL .~ Act 2 B
    AdvanceAct aid _ | aid == actId && onSide B attrs -> do
      hallwayId <- getJustLocationIdByName "Hallway"
      ghoulPriest <- EncounterCard <$> genEncounterCard Enemies.ghoulPriest
      litaChantler <- PlayerCard <$> genPlayerCard Assets.litaChantler
      parlorId <- getJustLocationIdByName "Parlor"
      a <$ unshiftMessages
        [ RevealLocation Nothing parlorId
        , CreateStoryAssetAt litaChantler parlorId
        , CreateEnemyAt ghoulPriest hallwayId Nothing
        , NextAct aid "01110"
        ]
    EndRoundWindow -> do
      investigatorIds <- getSetList @InvestigatorId
        (LocationWithTitle "Hallway")
      leadInvestigatorId <- getLeadInvestigatorId
      totalSpendableClueCount <- getSpendableClueCount investigatorIds
      requiredClueCount <- getPlayerCountValue (PerPlayer 3)
      if totalSpendableClueCount >= requiredClueCount
        then a <$ unshiftMessage
          (chooseOne
            leadInvestigatorId
            [ AdvanceAct actId (toSource attrs)
            , Continue "Continue without advancing act"
            ]
          )
        else pure a
    _ -> TheBarrier <$> runMessage msg attrs

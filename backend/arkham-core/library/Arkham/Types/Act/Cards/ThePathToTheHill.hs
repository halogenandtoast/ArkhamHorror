module Arkham.Types.Act.Cards.ThePathToTheHill
  ( ThePathToTheHill(..)
  , thePathToTheHill
  ) where

import Arkham.Prelude

import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Helpers
import Arkham.Types.Act.Runner
import Arkham.Types.ActId
import Arkham.Types.CampaignLogKey
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.Target

newtype ThePathToTheHill = ThePathToTheHill ActAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasModifiersFor env)

thePathToTheHill :: ThePathToTheHill
thePathToTheHill =
  ThePathToTheHill $ baseAttrs "02277" "The Path to the Hill" (Act 1 A) Nothing

instance ActionRunner env => HasActions env ThePathToTheHill where
  getActions i window (ThePathToTheHill x) = do
    clueCount <- unSpendableClueCount <$> getCount ()
    requiredClueCount <- getPlayerCountValue (PerPlayer 2)
    if clueCount >= requiredClueCount
      then pure [Force $ AdvanceAct (actId x) (toSource x)]
      else getActions i window x

instance ActRunner env => RunMessage env ThePathToTheHill where
  runMessage msg a@(ThePathToTheHill attrs@ActAttrs {..}) = case msg of
    AdvanceAct aid _ | aid == actId && onSide A attrs -> do
      investigatorIds <- getInvestigatorIds
      unshiftMessages =<< advanceActSideA investigatorIds (PerPlayer 2) attrs
      pure
        . ThePathToTheHill
        $ attrs
        & (sequenceL .~ Act (unActStep $ actStep actSequence) B)
    AdvanceAct aid _ | aid == actId && onSide B attrs -> do
      locationIds <- getSetList ()
      ascendingPathId <- fromJustNote "must exist"
        <$> getLocationIdWithTitle "Ascending Path"
      useV1 <- getHasRecord TheInvestigatorsRestoredSilasBishop
      useV2 <- liftM2
        (||)
        (getHasRecord TheInvestigatorsFailedToRecoverTheNecronomicon)
        (getHasRecord TheNecronomiconWasStolen)
      let
        nextActId = case (useV1, useV2) of
          (True, _) -> "02278"
          (False, True) -> "02279"
          (False, False) -> "02280"
      a <$ unshiftMessages
        (map (RemoveAllClues . LocationTarget) locationIds
        ++ [RevealLocation Nothing ascendingPathId, NextAct actId nextActId]
        )
    _ -> ThePathToTheHill <$> runMessage msg attrs

{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Act.Cards.UncoveringTheConspiracy where

import Arkham.Import

import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Helpers
import Arkham.Types.Act.Runner
import qualified Data.HashSet as HashSet

newtype UncoveringTheConspiracy = UncoveringTheConspiracy Attrs
  deriving newtype (Show, ToJSON, FromJSON)

uncoveringTheConspiracy :: UncoveringTheConspiracy
uncoveringTheConspiracy = UncoveringTheConspiracy
  $ baseAttrs "01123" "Uncovering the Conspiracy" (Act 1 A)

instance ActionRunner env => HasActions env UncoveringTheConspiracy where
  getActions iid NonFast (UncoveringTheConspiracy x@Attrs {..}) = do
    canAffordActions <- getCanAffordCost
      iid
      (toSource x)
      (ActionCost 1 Nothing mempty)
    requiredClues <- getPlayerCountValue (PerPlayer 2)
    totalSpendableClues <- getSpendableClueCount =<< getInvestigatorIds
    if totalSpendableClues >= requiredClues
      then pure
        [ ActivateCardAbilityAction
            iid
            (mkAbility (ActSource actId) 1 (ActionAbility 1 Nothing))
        | canAffordActions
        ]
      else getActions iid NonFast x
  getActions iid window (UncoveringTheConspiracy attrs) =
    getActions iid window attrs

instance ActRunner env => RunMessage env UncoveringTheConspiracy where
  runMessage msg a@(UncoveringTheConspiracy attrs@Attrs {..}) = case msg of
    AdvanceAct aid | aid == actId && actSequence == Act 1 A -> do
      leadInvestigatorId <- getLeadInvestigatorId
      unshiftMessage (chooseOne leadInvestigatorId [AdvanceAct aid])
      pure
        $ UncoveringTheConspiracy
        $ attrs
        & (sequenceL .~ Act 1 B)
        & (flippedL .~ True)
    AdvanceAct aid | aid == actId && actSequence == Act 1 B ->
      a <$ unshiftMessage (Resolution 1)
    AddToVictory _ -> do
      victoryDisplay <- HashSet.map unVictoryDisplayCardCode <$> getSet ()
      let
        cultists =
          setFromList ["01121b", "01137", "01138", "01139", "01140", "01141"]
      a <$ when
        (cultists `HashSet.isSubsetOf` victoryDisplay)
        (unshiftMessage (AdvanceAct actId))
    UseCardAbility iid (ActSource aid) _ 1 | aid == actId -> do
      investigatorIds <- getInvestigatorIds
      requiredClues <- getPlayerCountValue (PerPlayer 2)
      a <$ unshiftMessages
        [ SpendClues requiredClues investigatorIds
        , UseScenarioSpecificAbility iid 1
        ]
    _ -> UncoveringTheConspiracy <$> runMessage msg attrs

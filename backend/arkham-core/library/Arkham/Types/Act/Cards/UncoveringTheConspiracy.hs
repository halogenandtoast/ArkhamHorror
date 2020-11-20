{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Act.Cards.UncoveringTheConspiracy where

import Arkham.Import

import Arkham.Types.Act.Attrs
import qualified Arkham.Types.Act.Attrs as Act
import Arkham.Types.Act.Helpers
import Arkham.Types.Act.Runner
import qualified Data.HashSet as HashSet

newtype UncoveringTheConspiracy = UncoveringTheConspiracy Attrs
  deriving newtype (Show, ToJSON, FromJSON)

uncoveringTheConspiracy :: UncoveringTheConspiracy
uncoveringTheConspiracy = UncoveringTheConspiracy
  $ baseAttrs "01123" "Uncovering the Conspiracy" "Act 1a"

instance ActionRunner env => HasActions env UncoveringTheConspiracy where
  getActions iid NonFast (UncoveringTheConspiracy x@Attrs {..}) = do
    hasActionsRemaining <- getHasActionsRemaining iid Nothing mempty
    requiredClues <- getPlayerCountValue (PerPlayer 2)
    totalSpendableClues <- getSpendableClueCount =<< getInvestigatorIds
    if totalSpendableClues >= requiredClues
      then pure
        [ ActivateCardAbilityAction
            iid
            (mkAbility (ActSource actId) 1 (ActionAbility 1 Nothing))
        | hasActionsRemaining
        ]
      else getActions iid NonFast x
  getActions iid window (UncoveringTheConspiracy attrs) =
    getActions iid window attrs

instance ActRunner env => RunMessage env UncoveringTheConspiracy where
  runMessage msg a@(UncoveringTheConspiracy attrs@Attrs {..}) = case msg of
    AdvanceAct aid | aid == actId && actSequence == "Act 1a" -> do
      leadInvestigatorId <- getLeadInvestigatorId
      unshiftMessage (chooseOne leadInvestigatorId [AdvanceAct aid])
      pure
        $ UncoveringTheConspiracy
        $ attrs
        & (Act.sequence .~ "Act 1b")
        & (flipped .~ True)
    AdvanceAct aid | aid == actId && actSequence == "Act 1b" -> do
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

{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Act.Cards.UncoveringTheConspiracy where

import Arkham.Json
import Arkham.Types.Ability
import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Runner
import Arkham.Types.Classes
import Arkham.Types.FastWindow
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.Source
import ClassyPrelude hiding (sequence)
import qualified Data.HashSet as HashSet
import Lens.Micro

newtype UncoveringTheConspiracy = UncoveringTheConspiracy Attrs
  deriving newtype (Show, ToJSON, FromJSON)

uncoveringTheConspiracy :: UncoveringTheConspiracy
uncoveringTheConspiracy = UncoveringTheConspiracy
  $ baseAttrs "01123" "Uncovering the Conspiracy" "Act 1a"

instance (ActionRunner env investigator) => HasActions env investigator UncoveringTheConspiracy where
  getActions i NonFast (UncoveringTheConspiracy x@Attrs {..})
    | hasActionsRemaining i = do
      totalSpendableClueCount <- unSpendableClueCount
        <$> asks (getCount AllInvestigators)
      if totalSpendableClueCount >= 2
        then pure
          [ ActivateCardAbilityAction
              (getId () i)
              (mkAbility (ActSource actId) 1 (ActionAbility 1 Nothing))
          ]
        else getActions i NonFast x
  getActions i window (UncoveringTheConspiracy attrs) =
    getActions i window attrs

instance (ActRunner env) => RunMessage env UncoveringTheConspiracy where
  runMessage msg a@(UncoveringTheConspiracy attrs@Attrs {..}) = case msg of
    AdvanceAct aid | aid == actId && actSequence == "Act 1a" -> do
      pure
        $ UncoveringTheConspiracy
        $ attrs
        & (sequence .~ "Act 1b")
        & (flipped .~ True)
    UseCardAbility iid _ (ActSource aid) 1 | aid == actId -> do
      investigatorIds <- HashSet.toList <$> asks (getSet ())
      a <$ unshiftMessages
        [SpendClues 2 investigatorIds, UseScenarioSpecificAbility iid 1]
    _ -> UncoveringTheConspiracy <$> runMessage msg attrs

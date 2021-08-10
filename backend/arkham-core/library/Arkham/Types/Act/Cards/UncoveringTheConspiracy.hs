module Arkham.Types.Act.Cards.UncoveringTheConspiracy
  ( UncoveringTheConspiracy(..)
  , uncoveringTheConspiracy
  ) where

import Arkham.Prelude

import qualified Arkham.Act.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Helpers
import Arkham.Types.Act.Runner
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Message
import Arkham.Types.Resolution
import Arkham.Types.Source
import qualified Data.HashSet as HashSet

newtype UncoveringTheConspiracy = UncoveringTheConspiracy ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasModifiersFor env)

uncoveringTheConspiracy :: ActCard UncoveringTheConspiracy
uncoveringTheConspiracy =
  act (1, A) UncoveringTheConspiracy Cards.uncoveringTheConspiracy Nothing

instance HasActions UncoveringTheConspiracy where
  getActions (UncoveringTheConspiracy a) =
    mkAbility
        a
        1
        (ActionAbility
          Nothing
          (ActionCost 1 <> GroupClueCost (PerPlayer 2) Nothing)
        )
      : getActions a

instance ActRunner env => RunMessage env UncoveringTheConspiracy where
  runMessage msg a@(UncoveringTheConspiracy attrs@ActAttrs {..}) = case msg of
    AdvanceAct aid _ | aid == actId && onSide A attrs -> do
      leadInvestigatorId <- getLeadInvestigatorId
      push (chooseOne leadInvestigatorId [AdvanceAct aid $ toSource attrs])
      pure $ UncoveringTheConspiracy $ attrs & sequenceL .~ Act 1 B
    AdvanceAct aid _ | aid == actId && onSide B attrs ->
      a <$ push (ScenarioResolution $ Resolution 1)
    After (AddToVictory _) -> do
      victoryDisplay <- mapSet unVictoryDisplayCardCode <$> getSet ()
      let
        cultists =
          setFromList ["01121b", "01137", "01138", "01139", "01140", "01141"]
      a <$ when
        (cultists `HashSet.isSubsetOf` victoryDisplay)
        (push (AdvanceAct actId $ toSource attrs))
    UseCardAbility iid (ActSource aid) _ 1 _ | aid == actId -> do
      a <$ pushAll [UseScenarioSpecificAbility iid Nothing 1]
    _ -> UncoveringTheConspiracy <$> runMessage msg attrs

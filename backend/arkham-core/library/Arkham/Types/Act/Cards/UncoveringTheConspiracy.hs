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
import Arkham.Types.Cost
import Arkham.Types.GameValue
import Arkham.Types.Message
import Arkham.Types.Resolution
import Arkham.Types.Source
import Arkham.Types.Window
import Data.Set (isSubsetOf)

newtype UncoveringTheConspiracy = UncoveringTheConspiracy ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasModifiersFor env)

uncoveringTheConspiracy :: ActCard UncoveringTheConspiracy
uncoveringTheConspiracy =
  act (1, A) UncoveringTheConspiracy Cards.uncoveringTheConspiracy Nothing

instance ActionRunner env => HasActions env UncoveringTheConspiracy where
  getActions iid NonFast (UncoveringTheConspiracy x@ActAttrs {..}) = do
    requiredClues <- getPlayerCountValue (PerPlayer 2)
    totalSpendableClues <- getSpendableClueCount =<< getInvestigatorIds
    if totalSpendableClues >= requiredClues
      then pure
        [ UseAbility
            iid
            (mkAbility
              (ActSource actId)
              1
              (ActionAbility Nothing $ ActionCost 1)
            )
        ]
      else getActions iid NonFast x
  getActions iid window (UncoveringTheConspiracy attrs) =
    getActions iid window attrs

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
        (cultists `isSubsetOf` victoryDisplay)
        (push (AdvanceAct actId $ toSource attrs))
    UseCardAbility iid (ActSource aid) _ 1 _ | aid == actId -> do
      investigatorIds <- getInvestigatorIds
      requiredClues <- getPlayerCountValue (PerPlayer 2)
      a <$ pushAll
        [ SpendClues requiredClues investigatorIds
        , UseScenarioSpecificAbility iid Nothing 1
        ]
    _ -> UncoveringTheConspiracy <$> runMessage msg attrs

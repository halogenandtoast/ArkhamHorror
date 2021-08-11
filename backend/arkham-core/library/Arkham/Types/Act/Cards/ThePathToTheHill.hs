module Arkham.Types.Act.Cards.ThePathToTheHill
  ( ThePathToTheHill(..)
  , thePathToTheHill
  ) where

import Arkham.Prelude

import qualified Arkham.Act.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Helpers
import Arkham.Types.Act.Runner
import Arkham.Types.CampaignLogKey
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Message
import Arkham.Types.Restriction
import Arkham.Types.Target

newtype ThePathToTheHill = ThePathToTheHill ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasModifiersFor env)

thePathToTheHill :: ActCard ThePathToTheHill
thePathToTheHill = act
  (1, A)
  ThePathToTheHill
  Cards.thePathToTheHill
  (Just $ GroupClueCost (PerPlayer 2) Nothing)

instance HasActions ThePathToTheHill where
  getActions (ThePathToTheHill x) =
    restrictedAbility
        x
        1
        (InvestigatorsHaveSpendableClues $ AtLeast (PerPlayer 2))
        (Objective $ ForcedAbility AnyWindow)
      : getActions x

instance ActRunner env => RunMessage env ThePathToTheHill where
  runMessage msg a@(ThePathToTheHill attrs@ActAttrs {..}) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source ->
      a <$ push (AdvanceAct (toId attrs) (toSource attrs))
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
      a <$ pushAll
        (map (RemoveAllClues . LocationTarget) locationIds
        ++ [RevealLocation Nothing ascendingPathId, NextAct actId nextActId]
        )
    _ -> ThePathToTheHill <$> runMessage msg attrs

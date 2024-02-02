module Arkham.Act.Cards.ThePathToTheHill (
  ThePathToTheHill (..),
  thePathToTheHill,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes
import Arkham.Matcher hiding (RevealLocation)

newtype ThePathToTheHill = ThePathToTheHill ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

thePathToTheHill :: ActCard ThePathToTheHill
thePathToTheHill = act (1, A) ThePathToTheHill Cards.thePathToTheHill Nothing

instance HasAbilities ThePathToTheHill where
  getAbilities (ThePathToTheHill x)
    | onSide A x =
        [ mkAbility x 1 $ Objective $ ForcedAbilityWithCost AnyWindow (GroupClueCost (PerPlayer 2) Anywhere)
        ]
  getAbilities _ = []

instance RunMessage ThePathToTheHill where
  runMessage msg a@(ThePathToTheHill attrs@ActAttrs {..}) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      push $ AdvanceAct (toId attrs) (toSource attrs) AdvancedWithClues
      pure a
    AdvanceAct aid _ _ | aid == actId && onSide B attrs -> do
      locations <- selectTargets Anywhere
      ascendingPath <- selectJust $ LocationWithTitle "Ascending Path"
      pushAll
        $ map (RemoveAllClues (toSource attrs)) locations
        <> [ RevealLocation Nothing ascendingPath
           , advanceActDeck attrs
           ]

      pure a
    _ -> ThePathToTheHill <$> runMessage msg attrs

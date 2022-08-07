module Arkham.Act.Cards.DiscoveringTheTruth
  ( DiscoveringTheTruth(..)
  , discoveringTheTruth
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Types
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes
import Arkham.Investigator.Types ( Field (..) )
import Arkham.Matcher
import Arkham.Message hiding ( InvestigatorEliminated )
import Arkham.Projection
import Arkham.Timing qualified as Timing

newtype DiscoveringTheTruth = DiscoveringTheTruth ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

discoveringTheTruth :: ActCard DiscoveringTheTruth
discoveringTheTruth =
  act (1, A) DiscoveringTheTruth Cards.discoveringTheTruth Nothing

instance HasAbilities DiscoveringTheTruth where
  getAbilities (DiscoveringTheTruth a) =
    [mkAbility a 1 $ ForcedAbility $ InvestigatorEliminated Timing.When You]

instance RunMessage DiscoveringTheTruth where
  runMessage msg a@(DiscoveringTheTruth attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      clueCount <- field InvestigatorClues iid
      a
        <$ pushAll
             [ InvestigatorDiscardAllClues iid
             , PlaceClues (toTarget attrs) clueCount
             ]
    _ -> DiscoveringTheTruth <$> runMessage msg attrs

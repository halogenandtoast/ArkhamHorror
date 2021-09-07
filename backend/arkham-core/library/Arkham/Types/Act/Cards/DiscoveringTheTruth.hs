module Arkham.Types.Act.Cards.DiscoveringTheTruth
  ( DiscoveringTheTruth(..)
  , discoveringTheTruth
  ) where

import Arkham.Prelude

import qualified Arkham.Act.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Runner
import Arkham.Types.Classes
import Arkham.Types.Matcher
import Arkham.Types.Message hiding (InvestigatorEliminated)
import Arkham.Types.Query
import qualified Arkham.Types.Timing as Timing

newtype DiscoveringTheTruth = DiscoveringTheTruth ActAttrs
  deriving anyclass (IsAct, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

discoveringTheTruth :: ActCard DiscoveringTheTruth
discoveringTheTruth =
  act (1, A) DiscoveringTheTruth Cards.discoveringTheTruth Nothing

instance HasAbilities DiscoveringTheTruth where
  getAbilities (DiscoveringTheTruth a) =
    [mkAbility a 1 $ ForcedAbility $ InvestigatorEliminated Timing.When You]

instance ActRunner env => RunMessage env DiscoveringTheTruth where
  runMessage msg a@(DiscoveringTheTruth attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      clueCount <- unClueCount <$> getCount iid
      a
        <$ pushAll
             [ InvestigatorDiscardAllClues iid
             , PlaceClues (toTarget attrs) clueCount
             ]
    _ -> DiscoveringTheTruth <$> runMessage msg attrs

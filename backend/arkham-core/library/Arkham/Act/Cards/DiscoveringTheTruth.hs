module Arkham.Act.Cards.DiscoveringTheTruth (
  DiscoveringTheTruth (..),
  discoveringTheTruth,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection
import Arkham.Timing qualified as Timing

newtype DiscoveringTheTruth = DiscoveringTheTruth ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

discoveringTheTruth :: ActCard DiscoveringTheTruth
discoveringTheTruth =
  act (1, A) DiscoveringTheTruth Cards.discoveringTheTruth Nothing

instance HasAbilities DiscoveringTheTruth where
  getAbilities (DiscoveringTheTruth a) =
    [mkAbility a 1 $ ForcedAbility $ InvestigatorEliminated Timing.When You]

instance RunMessage DiscoveringTheTruth where
  runMessage msg a@(DiscoveringTheTruth attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      clueCount <- field InvestigatorClues iid
      pushAll
        [ InvestigatorDiscardAllClues (toAbilitySource attrs 1) iid
        , PlaceClues (toAbilitySource attrs 1) (toTarget attrs) clueCount
        ]
      pure a
    _ -> DiscoveringTheTruth <$> runMessage msg attrs

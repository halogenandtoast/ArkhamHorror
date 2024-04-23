module Arkham.Act.Cards.DiscoveringTheTruth (DiscoveringTheTruth (..), discoveringTheTruth) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Projection

newtype DiscoveringTheTruth = DiscoveringTheTruth ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

discoveringTheTruth :: ActCard DiscoveringTheTruth
discoveringTheTruth = act (1, A) DiscoveringTheTruth Cards.discoveringTheTruth Nothing

instance HasAbilities DiscoveringTheTruth where
  getAbilities (DiscoveringTheTruth a) =
    [mkAbility a 1 $ forced $ InvestigatorEliminated #when You]

instance RunMessage DiscoveringTheTruth where
  runMessage msg a@(DiscoveringTheTruth attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      clueCount <- field InvestigatorClues iid
      pushAll
        [ InvestigatorDiscardAllClues (attrs.ability 1) iid
        , PlaceClues (attrs.ability 1) (toTarget attrs) clueCount
        ]
      pure a
    _ -> DiscoveringTheTruth <$> runMessage msg attrs

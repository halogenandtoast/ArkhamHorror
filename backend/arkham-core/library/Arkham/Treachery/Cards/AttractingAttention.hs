module Arkham.Treachery.Cards.AttractingAttention (
  attractingAttention,
  AttractingAttention (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection
import Arkham.Scenarios.UndimensionedAndUnseen.Helpers
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype AttractingAttention = AttractingAttention TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

attractingAttention :: TreacheryCard AttractingAttention
attractingAttention = treachery AttractingAttention Cards.attractingAttention

instance RunMessage AttractingAttention where
  runMessage msg t@(AttractingAttention attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      mlid <- field InvestigatorLocation iid
      for_ mlid $ \lid -> do
        broodOfYogSothoth <- getBroodOfYogSothoth

        pushIfAny broodOfYogSothoth
          $ chooseOneAtATime iid
          $ [ targetLabel eid [MoveToward (EnemyTarget eid) (LocationWithId lid)]
            | eid <- broodOfYogSothoth
            ]
      pure t
    _ -> AttractingAttention <$> runMessage msg attrs

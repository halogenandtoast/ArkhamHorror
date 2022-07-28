module Arkham.Treachery.Cards.AttractingAttention
  ( attractingAttention
  , AttractingAttention(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Investigator.Types ( Field (..) )
import Arkham.Matcher
import Arkham.Message
import Arkham.Projection
import Arkham.Scenarios.UndimensionedAndUnseen.Helpers
import Arkham.Target
import Arkham.Treachery.Runner
import Arkham.Treachery.Cards qualified as Cards

newtype AttractingAttention = AttractingAttention TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

attractingAttention :: TreacheryCard AttractingAttention
attractingAttention = treachery AttractingAttention Cards.attractingAttention

instance RunMessage AttractingAttention where
  runMessage msg t@(AttractingAttention attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      mlid <- field InvestigatorLocation iid
      for_ mlid $ \lid -> do
        broodOfYogSothoth <- getBroodOfYogSothoth

        pushAll
          [ chooseOneAtATime
              iid
              [ MoveToward (EnemyTarget eid) (LocationWithId lid)
              | eid <- broodOfYogSothoth
              ]
          | notNull broodOfYogSothoth
          ]
      pure t
    _ -> AttractingAttention <$> runMessage msg attrs

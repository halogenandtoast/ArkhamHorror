module Arkham.Treachery.Cards.ChillFromBelow where

import Arkham.Classes
import Arkham.Investigator.Types (Field (..))
import Arkham.Prelude
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype ChillFromBelow = ChillFromBelow TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

chillFromBelow :: TreacheryCard ChillFromBelow
chillFromBelow = treachery ChillFromBelow Cards.chillFromBelow

instance RunMessage ChillFromBelow where
  runMessage msg t@(ChillFromBelow attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      push $ revelationSkillTest sid iid attrs #willpower (Fixed 3)
      pure t
    FailedThisSkillTestBy iid source@(isSource attrs -> True) n -> do
      handCount <- fieldMap InvestigatorHand length iid
      pushAll
        $ toMessage (randomDiscardN iid attrs (min n handCount))
        : [ InvestigatorAssignDamage iid source DamageAny (n - handCount) 0
          | n - handCount > 0
          ]
      pure t
    _ -> ChillFromBelow <$> runMessage msg attrs

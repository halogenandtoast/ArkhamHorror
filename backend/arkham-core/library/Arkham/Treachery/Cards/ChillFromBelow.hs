module Arkham.Treachery.Cards.ChillFromBelow where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Investigator.Types (Field (..))
import Arkham.Message
import Arkham.Projection
import Arkham.SkillType
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype ChillFromBelow = ChillFromBelow TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

chillFromBelow :: TreacheryCard ChillFromBelow
chillFromBelow = treachery ChillFromBelow Cards.chillFromBelow

instance RunMessage ChillFromBelow where
  runMessage msg t@(ChillFromBelow attrs) = case msg of
    Revelation iid source
      | isSource attrs source ->
          t <$ push (RevelationSkillTest iid source SkillWillpower 3)
    FailedSkillTest iid _ source@(isSource attrs -> True) SkillTestInitiatorTarget {} _ n ->
      do
        handCount <- fieldMap InvestigatorHand length iid
        pushAll $
          toMessage (randomDiscardN iid attrs (min n handCount))
            : [ InvestigatorAssignDamage iid source DamageAny (n - handCount) 0
              | n - handCount > 0
              ]
        pure t
    _ -> ChillFromBelow <$> runMessage msg attrs

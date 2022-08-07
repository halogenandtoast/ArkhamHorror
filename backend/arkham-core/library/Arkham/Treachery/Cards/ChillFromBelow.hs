module Arkham.Treachery.Cards.ChillFromBelow where

import Arkham.Prelude

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Classes
import Arkham.Message
import Arkham.SkillType
import Arkham.Target
import Arkham.Projection
import Arkham.Investigator.Types ( Field(..) )
import Arkham.Treachery.Runner

newtype ChillFromBelow = ChillFromBelow TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

chillFromBelow :: TreacheryCard ChillFromBelow
chillFromBelow = treachery ChillFromBelow Cards.chillFromBelow

instance RunMessage ChillFromBelow where
  runMessage msg t@(ChillFromBelow attrs) = case msg of
    Revelation iid source | isSource attrs source ->
      t <$ push (RevelationSkillTest iid source SkillWillpower 3)
    FailedSkillTest iid _ source SkillTestInitiatorTarget{} _ n
      | isSource attrs source -> do
        handCount <- fieldMap InvestigatorHand length iid
        if handCount < n
          then
            pushAll
            $ replicate handCount (RandomDiscard iid)
            <> [InvestigatorAssignDamage iid source DamageAny (n - handCount) 0]
          else pushAll $ replicate n (RandomDiscard iid)
        pure t
    _ -> ChillFromBelow <$> runMessage msg attrs

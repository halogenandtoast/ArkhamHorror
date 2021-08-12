module Arkham.Types.Treachery.Cards.ChillFromBelow where

import Arkham.Prelude

import qualified Arkham.Treachery.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.SkillType
import Arkham.Types.Target
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype ChillFromBelow = ChillFromBelow TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor env, HasActions)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

chillFromBelow :: TreacheryCard ChillFromBelow
chillFromBelow = treachery ChillFromBelow Cards.chillFromBelow

instance (TreacheryRunner env) => RunMessage env ChillFromBelow where
  runMessage msg t@(ChillFromBelow attrs@TreacheryAttrs {..}) = case msg of
    Revelation iid source | isSource attrs source -> t <$ pushAll
      [ RevelationSkillTest iid source SkillWillpower 3
      , Discard (TreacheryTarget treacheryId)
      ]
    FailedSkillTest iid _ source SkillTestInitiatorTarget{} _ n
      | isSource attrs source -> do
        handCount <- unCardCount <$> getCount iid
        if handCount < n
          then
            pushAll
            $ replicate handCount (RandomDiscard iid)
            <> [InvestigatorAssignDamage iid source DamageAny (n - handCount) 0]
          else pushAll $ replicate n (RandomDiscard iid)
        pure t
    _ -> ChillFromBelow <$> runMessage msg attrs

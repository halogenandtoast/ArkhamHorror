module Arkham.Event.Cards.LetMeHandleThis (letMeHandleThis, LetMeHandleThis (..)) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.EncounterCard
import Arkham.Helpers.Window (cardDrawn)
import Arkham.Matcher
import Arkham.Modifier

newtype LetMeHandleThis = LetMeHandleThis EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

letMeHandleThis :: EventCard LetMeHandleThis
letMeHandleThis = event LetMeHandleThis Cards.letMeHandleThis

instance RunMessage LetMeHandleThis where
  runMessage msg e@(LetMeHandleThis attrs@EventAttrs {..}) = runQueueT case msg of
    InvestigatorPlayEvent iid eid _ (cardDrawn -> card) _ | eid == eventId -> do
      lift $ changeEncounterCardDrawer card.id iid
      when card.isRevelation do
        selectOne (TreacheryWithCardId card.id) >>= traverse_ \tid -> do
          revelationModifier attrs iid tid $ AnySkillValue 2
      pure e
    _ -> LetMeHandleThis <$> liftRunMessage msg attrs

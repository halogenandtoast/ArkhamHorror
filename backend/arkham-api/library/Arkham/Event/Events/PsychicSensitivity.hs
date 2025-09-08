module Arkham.Event.Events.PsychicSensitivity (psychicSensitivity) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Projection
import Arkham.Name
import Arkham.Helpers.Window (cardDrawn)
import Arkham.Investigator.Types (Field(..))

newtype PsychicSensitivity = PsychicSensitivity EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

psychicSensitivity :: EventCard PsychicSensitivity
psychicSensitivity = event PsychicSensitivity Cards.psychicSensitivity

instance RunMessage PsychicSensitivity where
  runMessage msg e@(PsychicSensitivity attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      let card = cardDrawn attrs.windows
      cancelCardEffects attrs card
      discardCard iid attrs card
      quietCancelCardDraw card
      underGloria <- field InvestigatorCardsUnderneath iid
      for_ (find ((== card.title) . toTitle) underGloria) (addToDiscard iid . only)
      drawCards iid attrs 1
      pure e
    _ -> PsychicSensitivity <$> liftRunMessage msg attrs

module Arkham.Homebrew.AgesUnwound.Treacheries.UnevenAcceleration (unevenAcceleration) where

import Arkham.Helpers.Investigator (getCanLoseActions)
import Arkham.Homebrew.AgesUnwound.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.AgesUnwound.Helpers (campaignI18n)
import Arkham.I18n
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Import.Lifted

newtype UnevenAcceleration = UnevenAcceleration TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unevenAcceleration :: TreacheryCard UnevenAcceleration
unevenAcceleration = treachery UnevenAcceleration Cards.unevenAcceleration

{- | "Revelation - Each investigator must choose one: Lose an action. / Gain an
action. Draw the top card of the encounter deck."

"Lose an action" is 'loseActions', so an investigator whose only action left is
a restricted additional one still gives it up (FAQ 1.10); with nothing at all to
lose the option cannot be taken, or it would dodge the draw for free.
-}
instance RunMessage UnevenAcceleration where
  runMessage msg t@(UnevenAcceleration attrs) = runQueueT $ case msg of
    Revelation _ (isSource attrs -> True) -> do
      eachInvestigator \iid -> do
        canLose <- getCanLoseActions iid
        chooseOneM iid $ campaignI18n do
          unscoped $ countVar 1 $ labeledValidate canLose "loseActions" $ loseActions iid attrs 1
          labeled "unevenAcceleration.gainActionAndDraw" do
            gainActions iid attrs 1
            drawEncounterCard iid attrs
      pure t
    _ -> UnevenAcceleration <$> liftRunMessage msg attrs

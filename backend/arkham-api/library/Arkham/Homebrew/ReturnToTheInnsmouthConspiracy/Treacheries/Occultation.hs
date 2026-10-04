module Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Treacheries.Occultation (occultation) where

import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Helpers
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Import.Lifted

newtype Occultation = Occultation TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

occultation :: TreacheryCard Occultation
occultation = treachery Occultation Cards.occultation

{- | The cancel is offered before the doom is placed rather than as a real cancel
window: the only effect to cancel is the doom, so paying up front and skipping it
is the same outcome without needing the source to be cancelable.
-}
instance RunMessage Occultation where
  runMessage msg t@(Occultation attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      -- "A Deep One investigator at any location may take 2 horror and lose 2
      -- resources to cancel this effect."
      payers <- select $ deepOneInvestigator <> InvestigatorWithResources (atLeast 2)
      if null payers
        then placeDoomOnAgendaAndCheckAdvance 1
        else chooseOneM iid $ campaignI18n $ scope "occultation" do
          labeled "placeDoom" $ placeDoomOnAgendaAndCheckAdvance 1
          targets payers \iid' -> do
            assignHorror iid' attrs 2
            push $ LoseResources iid' (toSource attrs) 2
      pure t
    _ -> Occultation <$> liftRunMessage msg attrs

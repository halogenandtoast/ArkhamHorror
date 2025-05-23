module Arkham.Treachery.Cards.UmordhothsWrath (umordhothsWrath) where

import Arkham.Helpers.Message.Discard.Lifted
import Arkham.I18n
import Arkham.Investigator.Types (Field (..))
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype UmordhothsWrath = UmordhothsWrath TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

umordhothsWrath :: TreacheryCard UmordhothsWrath
umordhothsWrath = treachery UmordhothsWrath Cards.umordhothsWrath

instance RunMessage UmordhothsWrath where
  runMessage msg t@(UmordhothsWrath attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #willpower (Fixed 5)
      pure t
    FailedThisSkillTestBy _iid (isSource attrs -> True) n -> do
      doStep n msg
      pure t
    DoStep n msg'@(FailedThisSkillTest iid (isSource attrs -> True)) | n > 0 -> do
      hasCards <- fieldMap InvestigatorHand notNull iid
      if hasCards
        then chooseOneM iid $ withI18n do
          countVar 1 $ labeled' "discardCardsFromHand" $ chooseAndDiscardCard iid attrs
          numberVar "damage" 1
            $ numberVar "horror" 1
            $ labeled' "takeDamageAndHorror"
            $ assignDamageAndHorror iid attrs 1 1
        else assignDamageAndHorror iid attrs 1 1

      doStep (n - 1) msg'
      pure t
    _ -> UmordhothsWrath <$> liftRunMessage msg attrs

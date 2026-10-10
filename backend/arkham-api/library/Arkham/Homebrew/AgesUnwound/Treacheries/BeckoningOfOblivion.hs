module Arkham.Homebrew.AgesUnwound.Treacheries.BeckoningOfOblivion (beckoningOfOblivion) where

import Arkham.Calculation
import Arkham.Homebrew.AgesUnwound.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.AgesUnwound.Helpers (getStandardActions)
import Arkham.I18n
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Import.Lifted

newtype BeckoningOfOblivion = BeckoningOfOblivion TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

beckoningOfOblivion :: TreacheryCard BeckoningOfOblivion
beckoningOfOblivion = treachery BeckoningOfOblivion Cards.beckoningOfOblivion

{- | "Revelation - Test [intellect] or [willpower] (X), where X is the number of
standard actions you have. For each point you fail by, either lose 1 action or
take 1 direct horror."

The difficulty is fixed when the test is set up, so spending an action to a
chosen "lose 1 action" below cannot retroactively lower it.
-}
instance RunMessage BeckoningOfOblivion where
  runMessage msg t@(BeckoningOfOblivion attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      n <- getStandardActions iid
      sid <- getRandom
      chooseRevelationSkillTest sid iid attrs [#intellect, #willpower] (Fixed n)
      pure t
    FailedThisSkillTestBy _iid (isSource attrs -> True) n -> do
      doStep n msg
      pure t
    DoStep n (FailedThisSkillTest iid (isSource attrs -> True)) | n > 0 -> do
      canLose <- (> 0) <$> getStandardActions iid
      chooseOrRunOneM iid $ withI18n do
        when canLose $ countVar 1 $ labeled "loseActions" $ loseStandardActions iid attrs 1
        countVar 1 $ labeled "takeDirectHorror" $ directDamageAndHorror iid attrs 0 1
      doNextStep msg
      pure t
    _ -> BeckoningOfOblivion <$> liftRunMessage msg attrs

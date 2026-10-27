module Arkham.Homebrew.AgesUnwound.Treacheries.BeckoningOfOblivion (beckoningOfOblivion) where

import Arkham.Calculation
import Arkham.Helpers.Action (getUnrestrictedActions)
import Arkham.Helpers.Investigator (getCanLoseActions)
import Arkham.Homebrew.AgesUnwound.CardDefs.Treacheries qualified as Cards
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

Only the difficulty counts /standard/ actions, which is what the card says.
"Lose N actions" is 'loseActions', not 'loseUnrestrictedActions': the campaign's
/standard action/ term governs the effects that *count* actions, while losing
follows the core rule -- remaining actions go first, and an investigator left
with only additional actions loses those, whatever their limitation (FAQ 1.10).
-}
instance RunMessage BeckoningOfOblivion where
  runMessage msg t@(BeckoningOfOblivion attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      n <- getUnrestrictedActions iid
      sid <- getRandom
      chooseRevelationSkillTest sid iid attrs [#intellect, #willpower] (Fixed n)
      pure t
    FailedThisSkillTestBy _iid (isSource attrs -> True) n -> do
      doStep n msg
      pure t
    DoStep n (FailedThisSkillTest iid (isSource attrs -> True)) | n > 0 -> do
      canLose <- getCanLoseActions iid
      chooseOrRunOneM iid $ withI18n do
        countVar 1 $ labeledValidate canLose "loseActions" $ loseActions iid attrs 1
        countVar 1 $ labeled "takeDirectHorror" $ directDamageAndHorror iid attrs 0 1
      doNextStep msg
      pure t
    _ -> BeckoningOfOblivion <$> liftRunMessage msg attrs

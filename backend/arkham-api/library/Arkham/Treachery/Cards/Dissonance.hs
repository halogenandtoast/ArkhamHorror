module Arkham.Treachery.Cards.Dissonance (dissonance) where

import Arkham.Card.CardType
import Arkham.Helpers.Message.Discard.Lifted
import Arkham.Helpers.SkillTest.Lifted (beginSkillTest)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted hiding (beginSkillTest)

newtype Dissonance = Dissonance TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dissonance :: TreacheryCard Dissonance
dissonance = treachery Dissonance Cards.dissonance

instance RunMessage Dissonance where
  runMessage msg t@(Dissonance attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      beginSkillTest sid iid (toSource attrs) iid #willpower (Fixed 3)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      loseActions iid attrs 1
      chooseOneM iid do
        labeled "Discard all non-weakness assets from your hand" do
          discardAll iid attrs (CardWithType AssetType <> not_ WeaknessCard)
        labeled "Discard all non-weakness events from your hand" do
          discardAll iid attrs (CardWithType EventType <> not_ WeaknessCard)
        labeled "Discard all non-weakness skills from your hand" do
          discardAll iid attrs (CardWithType SkillType <> not_ WeaknessCard)
      pure t
    _ -> Dissonance <$> liftRunMessage msg attrs

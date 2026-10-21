module Arkham.Treachery.Cards.TracesToNowhere.OverYourShoulder (overYourShoulder) where

import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.TracesToNowhere.Helpers
import Arkham.Treachery.CardDefs.TracesToNowhere qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype OverYourShoulder = OverYourShoulder TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

overYourShoulder :: TreacheryCard OverYourShoulder
overYourShoulder = treachery OverYourShoulder Cards.overYourShoulder

instance RunMessage OverYourShoulder where
  runMessage msg t@(OverYourShoulder attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      allies <- select $ #ally <> AssetAt (locationWithInvestigator iid)
      chooseOrRunOneM iid $ scenarioI18n do
        labeledValidate (notNull allies) "overYourShoulder.abductAlly" do
          chooseTargetM iid allies \ally -> abductAsset ally
        labeled "overYourShoulder.takeHorror" $ directHorror iid attrs 1
      pure t
    _ -> OverYourShoulder <$> liftRunMessage msg attrs

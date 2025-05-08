module Arkham.Treachery.Cards.WallsClosingIn (wallsClosingIn) where

import Arkham.Choose
import Arkham.I18n
import Arkham.Location.Types (Field (..))
import Arkham.Message.Lifted.Choose
import Arkham.Scenario.Deck
import Arkham.Scenarios.TheUnspeakableOath.Helpers
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype WallsClosingIn = WallsClosingIn TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wallsClosingIn :: TreacheryCard WallsClosingIn
wallsClosingIn = treachery WallsClosingIn Cards.wallsClosingIn

instance RunMessage WallsClosingIn where
  runMessage msg t@(WallsClosingIn attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #willpower
        $ InvestigatorLocationMaybeFieldCalculation iid LocationShroud
      pure t
    FailedThisSkillTestBy iid (isSource attrs -> True) n -> do
      chooseOneM iid do
        withI18n $ countVar n $ labeled' "takeHorror" $ assignHorror iid attrs n
        scenarioI18n $ labeled' "wallsClosingIn.chooseEnemy" do
          push $ ChooseFrom iid $ chooseRandom attrs MonstersDeck 1
      pure t
    ChoseCards _ chosen | isTarget attrs chosen.target -> do
      placeUnderneath ActDeckTarget chosen.cards
      pure t
    _ -> WallsClosingIn <$> liftRunMessage msg attrs

module Arkham.Treachery.Cards.PassageIntoTheVeil (passageIntoTheVeil) where

import Arkham.Enemy.Cards qualified as Enemies
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.TheMiskatonicMuseum.Helpers
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype PassageIntoTheVeil = PassageIntoTheVeil TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

passageIntoTheVeil :: TreacheryCard PassageIntoTheVeil
passageIntoTheVeil = treachery PassageIntoTheVeil Cards.passageIntoTheVeil

instance RunMessage PassageIntoTheVeil where
  runMessage msg t@(PassageIntoTheVeil attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      huntingHorrorAtYourLocation <-
        selectAny $ enemyIs Enemies.huntingHorror <> at_ (locationWithInvestigator iid)
      sid <- getRandom
      revelationSkillTest sid iid attrs #willpower
        $ Fixed (if huntingHorrorAtYourLocation then 5 else 3)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> scenarioI18n do
      assets <- select $ assetControlledBy iid <> #ally
      chooseOneM iid do
        unscoped $ countVar 5 $ labeled' "discardTopOfYourDeck" $ discardTopOfDeck iid attrs 5
        labeled' "passageIntoTheVeil.damage" do
          directDamage iid attrs 1
          for_ assets \aid -> dealAssetDamage aid attrs 1
      pure t
    _ -> PassageIntoTheVeil <$> liftRunMessage msg attrs

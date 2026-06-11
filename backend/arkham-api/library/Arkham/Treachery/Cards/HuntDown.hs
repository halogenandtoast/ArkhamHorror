module Arkham.Treachery.Cards.HuntDown (huntDown) where

import Arkham.Enemy.Types (Field (..))
import Arkham.Helpers.Investigator (getJustLocation)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.WarOfTheOuterGods.Helpers
import Arkham.Trait (Trait (Mutated))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype HuntDown = HuntDown TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

huntDown :: TreacheryCard HuntDown
huntDown = treachery HuntDown Cards.huntDown

instance RunMessage HuntDown where
  runMessage msg t@(HuntDown attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      mutated <- selectWithField EnemyFight $ EnemyWithTrait Mutated
      let highest = maximumEx $ 0 : mapMaybe snd mutated
      let enemies = [eid | (eid, fight) <- mutated, fight == Just highest]
      if null enemies
        then gainSurge attrs
        else do
          lid <- getJustLocation iid
          chooseOrRunOneM iid $ targets enemies \enemy -> do
            placeMutations attrs enemy 1
            readyThis enemy
            push $ MoveUntil lid (toTarget enemy)
            push $ EnemyEngageInvestigator enemy iid
            initiateEnemyAttack enemy attrs iid
      pure t
    _ -> HuntDown <$> liftRunMessage msg attrs

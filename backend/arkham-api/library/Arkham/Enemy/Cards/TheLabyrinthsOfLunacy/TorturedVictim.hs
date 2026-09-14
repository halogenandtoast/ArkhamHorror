module Arkham.Enemy.Cards.TheLabyrinthsOfLunacy.TorturedVictim (torturedVictim) where

import Arkham.Enemy.CardDefs.TheLabyrinthsOfLunacy qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Message.Discard.Lifted (randomDiscard)
import Arkham.I18n
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.TheLabyrinthsOfLunacy.Helpers

newtype TorturedVictim = TorturedVictim EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

torturedVictim :: EnemyCard TorturedVictim
torturedVictim = enemy TorturedVictim Cards.torturedVictim

instance RunMessage TorturedVictim where
  runMessage msg e@(TorturedVictim attrs) = runQueueT $ scenarioI18n $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      chooseOneM iid $ scope "torturedVictim" do
        labeled "discardRandomCard" $ randomDiscard iid attrs
        labeled "takeAttack" $ initiateEnemyAttack attrs attrs iid
      pure e
    _ -> TorturedVictim <$> liftRunMessage msg attrs

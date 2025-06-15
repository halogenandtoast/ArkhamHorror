module Arkham.Treachery.Cards.ColdStreak (coldStreak) where

import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype ColdStreak = ColdStreak TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

coldStreak :: TreacheryCard ColdStreak
coldStreak = treachery ColdStreak Cards.coldStreak

instance RunMessage ColdStreak where
  runMessage msg t@(ColdStreak attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      resources <- field InvestigatorResources iid
      let lost = min 4 resources
          need = 4 - lost
      loseResources iid attrs lost
      replicateM_ need $ chooseOne iid
        [Label "Take 1 damage" [assignDamage iid attrs 1]
        ,Label "Take 1 horror" [assignHorror iid attrs 1]
        ]
      pure t
    _ -> ColdStreak <$> liftRunMessage msg attrs

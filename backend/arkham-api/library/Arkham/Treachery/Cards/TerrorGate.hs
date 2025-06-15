module Arkham.Treachery.Cards.TerrorGate (terrorGate) where

import Arkham.Ability
import Arkham.Helpers.Agenda (getCurrentAgendaStep)
import Arkham.Message.Lifted.Choose
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype TerrorGate = TerrorGate TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

terrorGate :: TreacheryCard TerrorGate
terrorGate = treachery TerrorGate Cards.terrorGate

instance RunMessage TerrorGate where
  runMessage msg t@(TerrorGate attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      step <- getCurrentAgendaStep
      sid <- getRandom
      let difficulty = step + 1 -- base 2 plus 1? Wait: 1 plus current agenda number -> step +1
      revelationSkillTest sid iid attrs #willpower (Fixed difficulty)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      chooseOne iid
        [ Label "Take 2 damage" [assignDamage iid attrs 2]
        , Label "Discard 2 cards" [chooseAndDiscardCards iid attrs 2]
        , Label "Lose 3 resources" [loseResources iid attrs 3]
        , Label "Take 2 horror" [assignHorror iid attrs 2]
        , Label "Lose 1 action" [loseActions iid attrs 1]
        ]
      pure t
    _ -> TerrorGate <$> liftRunMessage msg attrs

module Arkham.Treachery.Cards.ImperceptableCreature (imperceptableCreature) where

import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Placement
import Arkham.Scenarios.UndimensionedAndUnseen.Helpers
import Arkham.Trait (Trait (Elite))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype ImperceptableCreature = ImperceptableCreature TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

imperceptableCreature :: TreacheryCard ImperceptableCreature
imperceptableCreature = treachery ImperceptableCreature Cards.imperceptableCreature

instance HasModifiersFor ImperceptableCreature where
  getModifiersFor (ImperceptableCreature a) = do
    for_ a.attached.enemy \eid -> do
      modified_ a eid [AddTrait Elite]

instance RunMessage ImperceptableCreature where
  runMessage msg t@(ImperceptableCreature attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      brood <-
        select
          $ InPlayEnemy
          $ EnemyWithTitle broodTitle
          <> not_ (EnemyWithAttachedTreachery $ treacheryIs Cards.imperceptableCreature)
      chooseTargetM iid brood \selected -> do
        removeTokens attrs selected #clue 1
        place attrs (AttachedToEnemy selected)
      pure t
    _ -> ImperceptableCreature <$> liftRunMessage msg attrs

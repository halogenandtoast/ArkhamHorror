module Arkham.Act.Cards.QuestForTheSableGlass (questForTheSableGlass) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype QuestForTheSableGlass = QuestForTheSableGlass ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

questForTheSableGlass :: ActCard QuestForTheSableGlass
questForTheSableGlass = act (1, A) QuestForTheSableGlass Cards.questForTheSableGlass Nothing

instance RunMessage QuestForTheSableGlass where
  runMessage msg a@(QuestForTheSableGlass attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> QuestForTheSableGlass <$> liftRunMessage msg attrs

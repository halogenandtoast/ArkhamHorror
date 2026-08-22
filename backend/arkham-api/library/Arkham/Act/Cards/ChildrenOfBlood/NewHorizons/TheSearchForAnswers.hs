module Arkham.Act.Cards.ChildrenOfBlood.NewHorizons.TheSearchForAnswers (theSearchForAnswers) where

import Arkham.Act.CardDefs.ChildrenOfBlood.NewHorizons qualified as Cards
import Arkham.Act.Import.Lifted

newtype TheSearchForAnswers = TheSearchForAnswers ActAttrs
  deriving anyclass (IsAct, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theSearchForAnswers :: ActCard TheSearchForAnswers
theSearchForAnswers = act (2, A) TheSearchForAnswers Cards.theSearchForAnswers Nothing

instance RunMessage TheSearchForAnswers where
  runMessage msg a@(TheSearchForAnswers attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> TheSearchForAnswers <$> liftRunMessage msg attrs

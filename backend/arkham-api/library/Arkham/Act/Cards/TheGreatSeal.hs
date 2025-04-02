module Arkham.Act.Cards.TheGreatSeal (theGreatSeal) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Matcher

newtype TheGreatSeal = TheGreatSeal ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theGreatSeal :: ActCard TheGreatSeal
theGreatSeal = act (2, A) TheGreatSeal Cards.theGreatSeal Nothing

instance HasAbilities TheGreatSeal where
  getAbilities (TheGreatSeal a) =
    extend
      a
      [ restricted a 1 (exists $ mconcat $ "The Gate of Y'quaa" : map LocationWithActiveSeal [minBound ..])
          $ Objective
          $ forced AnyWindow
      ]

instance RunMessage TheGreatSeal where
  runMessage msg a@(TheGreatSeal attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      push R1
      pure a
    _ -> TheGreatSeal <$> liftRunMessage msg attrs

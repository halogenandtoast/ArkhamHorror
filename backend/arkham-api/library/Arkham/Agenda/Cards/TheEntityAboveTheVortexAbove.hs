module Arkham.Agenda.Cards.TheEntityAboveTheVortexAbove (theEntityAboveTheVortexAbove) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Query
import Arkham.Matcher
import Arkham.Trait

newtype TheEntityAboveTheVortexAbove = TheEntityAboveTheVortexAbove AgendaAttrs
  deriving anyclass IsAgenda
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theEntityAboveTheVortexAbove :: AgendaCard TheEntityAboveTheVortexAbove
theEntityAboveTheVortexAbove =
  agenda (2, C) TheEntityAboveTheVortexAbove Cards.theEntityAboveTheVortexAbove (Static 6)

instance HasModifiersFor TheEntityAboveTheVortexAbove where
  getModifiersFor (TheEntityAboveTheVortexAbove a) =
    modifySelect a (EnemyWithTrait Monster) [EnemyFight 1]

instance HasAbilities TheEntityAboveTheVortexAbove where
  getAbilities (TheEntityAboveTheVortexAbove a) =
    [groupLimit PerRound $ mkAbility a 1 $ FastAbility $ GroupClueCost (PerPlayer 1) Anywhere]

instance RunMessage TheEntityAboveTheVortexAbove where
  runMessage msg a@(TheEntityAboveTheVortexAbove attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide D attrs -> True) -> do
      toDiscard GameSource attrs
      openThePathAbove <- getSetAsideCard Acts.openThePathAbove
      push $ AddAct 2 openThePathAbove
      pure a
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      placeDoom (attrs.ability 1) attrs 1
      push AdvanceAgendaIfThresholdSatisfied
      eachInvestigator \iid -> drawCards iid (attrs.ability 1) 1
      pure a
    _ -> TheEntityAboveTheVortexAbove <$> liftRunMessage msg attrs

module Arkham.Agenda.Cards.BrethrenOfAsh (brethrenOfAsh) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted hiding (EnemyDefeated, InvestigatorDefeated)
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Matcher

newtype BrethrenOfAsh = BrethrenOfAsh AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

brethrenOfAsh :: AgendaCard BrethrenOfAsh
brethrenOfAsh = agendaWith (3, A) BrethrenOfAsh Cards.brethrenOfAsh (Static 0) (doomThresholdL .~ Nothing)

instance HasAbilities BrethrenOfAsh where
  getAbilities (BrethrenOfAsh a)
    | onSide A a =
        [ forcedAbility a 1 $ WouldPlaceDoomCounter #when AnySource AnyTarget
        , restricted a 2 (notExists UneliminatedInvestigator)
            $ forced
            $ InvestigatorDefeated #when ByAny Anyone
        , mkAbility a 3
            $ Objective
            $ forced
            $ IfEnemyDefeated #after Anyone ByAny
            $ enemyIs Enemies.elokossMotherOfFlame
        ]
    | otherwise = []

instance RunMessage BrethrenOfAsh where
  runMessage msg a@(BrethrenOfAsh attrs) = runQueueT $ case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws _ -> do
      cancelWindowBatch ws
      eachInvestigator \iid -> directDamage iid attrs 1
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advanceAgenda attrs
      pure a
    UseThisAbility _ (isSource attrs -> True) 3 -> do
      push R1
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      eachInvestigator $ kill attrs
      pure a
    _ -> BrethrenOfAsh <$> liftRunMessage msg attrs

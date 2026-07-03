module Arkham.Agenda.Cards.LambsToTheSlaughter (lambsToTheSlaughter) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modified_)
import Arkham.Helpers.Query (getPlayerCount)
import Arkham.Helpers.SkillTest (getSkillTestTarget)
import Arkham.Matcher
import Arkham.Trait (Trait (Resident))

newtype LambsToTheSlaughter = LambsToTheSlaughter AgendaAttrs
  deriving anyclass IsAgenda
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lambsToTheSlaughter :: AgendaCard LambsToTheSlaughter
lambsToTheSlaughter = agenda (1, A) LambsToTheSlaughter Cards.lambsToTheSlaughter (Static 5)

instance HasModifiersFor LambsToTheSlaughter where
  getModifiersFor (LambsToTheSlaughter a) = do
    modified_ a ScenarioTarget [ScenarioModifier "cannotTriggerCodex"]
    eachInvestigator \iid -> modified_ a iid [ScenarioModifier "cannotTriggerCodex"]

instance HasAbilities LambsToTheSlaughter where
  getAbilities (LambsToTheSlaughter a) =
    guard (onSide A a)
      *> [ onlyOnce
             $ restricted a 1 (InVictoryDisplay (CardWithTrait Resident) (AtLeast $ StaticWithPerPlayer 1 1))
             $ Objective
             $ forced AnyWindow
         , restricted a 2 (exists $ EnemyWithTrait Resident)
             $ freeReaction
             $ SkillTestResult
               #after
               You
               (WhileEvadingAnEnemy $ EnemyWithTrait Resident)
               (SuccessResult $ atLeast 2)
         ]

checkLambsToTheSlaughterObjective :: ReverseQueue m => m ()
checkLambsToTheSlaughterObjective = do
  n <- selectCount $ VictoryDisplayCardMatch $ basic $ CardWithTrait Resident
  target <- (+ 1) <$> getPlayerCount
  when (n >= target) $ push R3

instance RunMessage LambsToTheSlaughter where
  runMessage msg a@(LambsToTheSlaughter attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      checkLambsToTheSlaughterObjective
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      getSkillTestTarget >>= traverse_ \case
        EnemyTarget eid -> addToVictory iid eid
        _ -> pure ()
      checkLambsToTheSlaughterObjective
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      -- Back in the Fold: each investigator resigns.
      eachInvestigator resign
      pure a
    _ -> LambsToTheSlaughter <$> liftRunMessage msg attrs

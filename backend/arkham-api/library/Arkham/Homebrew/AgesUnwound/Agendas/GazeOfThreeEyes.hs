module Arkham.Homebrew.AgesUnwound.Agendas.GazeOfThreeEyes (gazeOfThreeEyes) where

import Arkham.Ability
import Arkham.Agenda.Import.Lifted
import Arkham.Helpers.Cost (getSpendableClueCount)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Homebrew.AgesUnwound.CardDefs.Agendas qualified as Cards
import Arkham.Homebrew.AgesUnwound.Scenarios.NightOfFire.Helpers
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype GazeOfThreeEyes = GazeOfThreeEyes AgendaAttrs
  deriving anyclass IsAgenda
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

gazeOfThreeEyes :: AgendaCard GazeOfThreeEyes
gazeOfThreeEyes = agenda (3, A) GazeOfThreeEyes Cards.gazeOfThreeEyes (Static 5)

{- | "Doom on cards other than this agenda subtracts from the total doom in play
instead of adding to it."
-}
instance HasModifiersFor GazeOfThreeEyes where
  getModifiersFor (GazeOfThreeEyes a) = modifySelf a [OtherDoomSubtracts]

instance HasAbilities GazeOfThreeEyes where
  getAbilities (GazeOfThreeEyes a) =
    [forcedAbility a 1 $ TurnEnds #when (You <> not_ InvestigatorThatMovedDuringTurn)]

instance RunMessage GazeOfThreeEyes where
  runMessage msg a@(GazeOfThreeEyes attrs) = runQueueT $ nightOfFireI18n $ case msg of
    -- "Forced - At the end of your turn, if you did not move at least once
    -- during your turn: Test [agility] (3). If you fail, choose two of: spend a
    -- clue, take 1 damage and take 1 horror."
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #agility (Fixed 3)
      pure a
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      clues <- getSpendableClueCount [iid]
      chooseNM iid 2 $ unscoped $ countVar 1 do
        labeledValidate (clues >= 1) "spendClues" $ spendClues iid 1
        labeled "takeDamage" $ assignDamage iid (attrs.ability 1) 1
        labeled "takeHorror" $ assignHorror iid (attrs.ability 1) 1
      pure a
    -- "Morning Breaks. -> R2"
    AdvanceAgenda (isSide B attrs -> True) -> do
      push R2
      pure a
    _ -> GazeOfThreeEyes <$> liftRunMessage msg attrs

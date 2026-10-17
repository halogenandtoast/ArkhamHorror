module Arkham.Homebrew.AgesUnwound.Agendas.GazeOfThreeEyes (gazeOfThreeEyes) where

import Arkham.Ability
import Arkham.Agenda.Import.Lifted
import Arkham.Helpers.Cost (getSpendableClueCount)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Homebrew.AgesUnwound.CardDefs.Agendas qualified as Cards
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype GazeOfThreeEyes = GazeOfThreeEyes AgendaAttrs
  deriving anyclass IsAgenda
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

gazeOfThreeEyes :: AgendaCard GazeOfThreeEyes
gazeOfThreeEyes = agenda (3, A) GazeOfThreeEyes Cards.gazeOfThreeEyes (Static 5)

instance HasModifiersFor GazeOfThreeEyes where
  getModifiersFor (GazeOfThreeEyes a) = modifySelf a [OtherDoomSubtracts]

instance HasAbilities GazeOfThreeEyes where
  getAbilities (GazeOfThreeEyes a) =
    [forcedAbility a 1 $ TurnEnds #when (You <> not_ InvestigatorThatMovedDuringTurn)]

instance RunMessage GazeOfThreeEyes where
  runMessage msg a@(GazeOfThreeEyes attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #agility (Fixed 3)
      pure a
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      clues <- getSpendableClueCount [iid]
      chooseNM iid 2 $ withI18n $ countVar 1 do
        labeledValidate (clues >= 1) "spendClues" $ spendClues iid 1
        labeled "takeDamage" $ assignDamage iid (attrs.ability 1) 1
        labeled "takeHorror" $ assignHorror iid (attrs.ability 1) 1
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      push R2
      pure a
    _ -> GazeOfThreeEyes <$> liftRunMessage msg attrs

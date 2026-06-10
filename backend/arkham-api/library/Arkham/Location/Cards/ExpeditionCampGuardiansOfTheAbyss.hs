module Arkham.Location.Cards.ExpeditionCampGuardiansOfTheAbyss (expeditionCampGuardiansOfTheAbyss) where

import Arkham.Ability
import Arkham.Campaigns.GuardiansOfTheAbyss.Helpers (campaignI18n)
import Arkham.Card (toCard)
import Arkham.Campaigns.TheForgottenAge.Helpers (getExplorationDeck)
import Arkham.Deck qualified as Deck
import Arkham.Helpers (Deck (..))
import Arkham.Helpers.Scenario (getEncounterDeck)
import Arkham.I18n
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Message.Lifted.Choose
import Arkham.Scenario.Deck

newtype ExpeditionCampGuardiansOfTheAbyss = ExpeditionCampGuardiansOfTheAbyss LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

expeditionCampGuardiansOfTheAbyss :: LocationCard ExpeditionCampGuardiansOfTheAbyss
expeditionCampGuardiansOfTheAbyss =
  location ExpeditionCampGuardiansOfTheAbyss Cards.expeditionCampGuardiansOfTheAbyss 2 (Static 0)

instance HasAbilities ExpeditionCampGuardiansOfTheAbyss where
  getAbilities (ExpeditionCampGuardiansOfTheAbyss a) =
    extendRevealed1 a
      $ groupLimit PerRound
      $ skillTestAbility
      $ restricted a 1 Here parleyAction_

instance RunMessage ExpeditionCampGuardiansOfTheAbyss where
  runMessage msg l@(ExpeditionCampGuardiansOfTheAbyss attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) attrs #intellect (Fixed 2)
      pure l
    PassedThisSkillTestBy iid (isAbilitySource attrs 1 -> True) n | n > 0 -> do
      explorationDeck <- getExplorationDeck
      campaignI18n $ chooseOrRunOneM iid do
        when (notNull explorationDeck) do
          labeled' "lookAtExplorationDeck" $ doStep 1 msg
        labeled' "lookAtEncounterDeck" $ doStep 2 msg
      pure l
    DoStep step (PassedThisSkillTestBy iid (isAbilitySource attrs 1 -> True) n) -> do
      (cards, deck) <-
        if step == 1
          then (,Deck.ScenarioDeckByKey ExplorationDeck) . take n <$> getExplorationDeck
          else (,Deck.EncounterDeck) . map toCard . take n . unDeck <$> getEncounterDeck
      focusCards cards do
        push $ ShuffleCardsIntoTopOfDeck deck (length cards) []
        withI18n $ chooseUpToNM' iid 2 "done" do
          targets cards \card -> push $ PutCardOnBottomOfDeck iid deck card
      pure l
    _ -> ExpeditionCampGuardiansOfTheAbyss <$> liftRunMessage msg attrs

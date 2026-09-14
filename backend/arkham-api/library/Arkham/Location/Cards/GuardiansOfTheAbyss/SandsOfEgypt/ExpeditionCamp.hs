module Arkham.Location.Cards.GuardiansOfTheAbyss.SandsOfEgypt.ExpeditionCamp (expeditionCamp) where

import Arkham.Ability
import Arkham.Campaigns.GuardiansOfTheAbyss.Helpers (campaignI18n)
import Arkham.Campaigns.TheForgottenAge.Helpers (getExplorationDeck)
import Arkham.Card (toCard)
import Arkham.Deck qualified as Deck
import Arkham.Helpers (Deck (..))
import Arkham.Helpers.Scenario (getEncounterDeck)
import Arkham.I18n
import Arkham.Location.CardDefs.GuardiansOfTheAbyss.SandsOfEgypt qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Message.Lifted.Choose
import Arkham.Scenario.Deck

newtype ExpeditionCamp = ExpeditionCamp LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

expeditionCamp :: LocationCard ExpeditionCamp
expeditionCamp =
  symbolLabel
    $ location ExpeditionCamp Cards.expeditionCamp 2 (Static 0)

instance HasAbilities ExpeditionCamp where
  getAbilities (ExpeditionCamp a) =
    extendRevealed1 a
      $ groupLimit PerRound
      $ skillTestAbility
      $ restricted a 1 Here parleyAction_

instance RunMessage ExpeditionCamp where
  runMessage msg l@(ExpeditionCamp attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) attrs #intellect (Fixed 2)
      pure l
    PassedThisSkillTestBy iid (isAbilitySource attrs 1 -> True) n | n > 0 -> do
      explorationDeck <- getExplorationDeck
      campaignI18n $ chooseOrRunOneM iid do
        when (notNull explorationDeck) do
          labeled "lookAtExplorationDeck" $ doStep 1 msg
        labeled "lookAtEncounterDeck" $ doStep 2 msg
      pure l
    DoStep step (PassedThisSkillTestBy iid (isAbilitySource attrs 1 -> True) n) -> do
      (cards, deck) <-
        if step == 1
          then (,Deck.ScenarioDeckByKey ExplorationDeck) . take n <$> getExplorationDeck
          else (,Deck.EncounterDeck) . map toCard . take n . unDeck <$> getEncounterDeck
      focusCards cards do
        push $ ShuffleCardsIntoTopOfDeck deck (length cards) []
        withI18n $ chooseUpToNM iid 2 "done" do
          targets cards \card -> push $ PutCardOnBottomOfDeck iid deck card
      pure l
    _ -> ExpeditionCamp <$> liftRunMessage msg attrs

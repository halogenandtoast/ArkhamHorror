module Arkham.Homebrew.TheFallOfTheHouseOfUsher.Locations.TheTarn (theTarn) where

import Arkham.Ability
import Arkham.Deck qualified as Deck
import Arkham.Helpers.Scenario (scenarioField)
import Arkham.Helpers.SkillTest.Lifted (parley)
import Arkham.Homebrew.TheFallOfTheHouseOfUsher.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.TheFallOfTheHouseOfUsher.Helpers
import Arkham.I18n
import Arkham.Location.Import.Lifted
import Arkham.Message.Lifted.Choose
import Arkham.Scenario.Types (Field (ScenarioDiscard))

newtype TheTarn = TheTarn LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theTarn :: LocationCard TheTarn
theTarn = location TheTarn Cards.theTarn 4 (PerPlayer 1)

instance HasAbilities TheTarn where
  getAbilities (TheTarn a) =
    extendRevealed1 a
      $ scenarioI18n
      $ withI18nTooltip "theTarn.parley"
      $ groupLimit PerRound
      $ skillTestAbility
      $ restricted a 1 Here parleyAction_

instance RunMessage TheTarn where
  runMessage msg l@(TheTarn attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      parley sid iid (attrs.ability 1) attrs #willpower (Fixed 3)
      pure l
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      discards <- scenarioField ScenarioDiscard
      withI18n $ chooseSomeM iid "done" $ scenarioI18n $ scope "theTarn" do
        questionLabeled "shuffleBack"
        targets discards \card -> shuffleCardsIntoDeck Deck.EncounterDeck [card]
      pure l
    FailedThisSkillTest _ (isAbilitySource attrs 1 -> True) -> do
      shuffleEncounterDiscardBackIn
      pure l
    _ -> TheTarn <$> liftRunMessage msg attrs

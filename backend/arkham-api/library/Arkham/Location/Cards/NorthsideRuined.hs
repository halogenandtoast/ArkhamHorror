module Arkham.Location.Cards.NorthsideRuined (northsideRuined) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Message.Lifted.Choose
import Arkham.Scenario.Deck (ScenarioDeckKey (CthulhuDeck))
import Arkham.Scenarios.TheDoomOfArkhamPartII.Helpers (sharedI18n)
import Arkham.Strategy

newtype NorthsideRuined = NorthsideRuined LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

northsideRuined :: LocationCard NorthsideRuined
northsideRuined = location NorthsideRuined Cards.northsideRuined 4 (Static 1)

instance HasModifiersFor NorthsideRuined where
  getModifiersFor (NorthsideRuined a) = modifySelf a [CannotBeFullyFlooded]

instance HasAbilities NorthsideRuined where
  getAbilities (NorthsideRuined a) =
    extendRevealed1 a
      $ groupLimit PerRound
      $ skillTestAbility
      $ restricted a 1 (Here <> ScenarioDeckWithCard CthulhuDeck) actionAbility

instance RunMessage NorthsideRuined where
  runMessage msg l@(NorthsideRuined attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) attrs #willpower (Fixed 3)
      pure l
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      lookAt
        iid
        (attrs.ability 1)
        (ScenarioDeckTarget CthulhuDeck)
        [(FromTopOfDeck 3, PutBackInAnyOrder)]
        #any
        (defer attrs IsNotDraw)
      pure l
    SearchFound iid (isTarget attrs -> True) _ cards | notNull cards -> do
      focusCards cards do
        chooseOneM iid $ sharedI18n do
          labeled' "doNotDiscard" nothing
          targets cards \card -> do
            obtainCard card
            push $ ScenarioSpecific "discardCthulhuCard" (toJSON card)
      pure l
    _ -> NorthsideRuined <$> liftRunMessage msg attrs

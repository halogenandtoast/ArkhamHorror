module Arkham.Location.Cards.ArkhamGazette (arkhamGazette) where

import Arkham.Ability
import Arkham.Card (toCardId)
import Arkham.Deck qualified as Deck
import Arkham.Helpers (Deck (..))
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Scenario (getEncounterDeck)
import Arkham.I18n
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Token qualified as Token

newtype ArkhamGazette = ArkhamGazette LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arkhamGazette :: LocationCard ArkhamGazette
arkhamGazette = location ArkhamGazette Cards.arkhamGazette 2 (PerPlayer 1)

instance HasAbilities ArkhamGazette where
  getAbilities (ArkhamGazette a) =
    extendRevealed
      a
      [ restricted a 1 Here actionAbility
      , restricted a 2 Here $ FastAbility (SpendTokenCost Token.Newspaper (TargetIs $ toTarget a))
      ]

instance RunMessage ArkhamGazette where
  runMessage msg l@(ArkhamGazette attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      chooseOneM iid $ withI18n do
        for_ [#willpower, #intellect, #combat, #agility] \sk ->
          chooseTest sk 4 $ beginSkillTest sid iid (attrs.ability 1) iid sk (Fixed 4)
      pure l
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      gainClues iid (attrs.ability 1) 1
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      n <- perPlayer 1
      placeClues (attrs.ability 2) attrs n
      cards <- take 5 . unDeck <$> getEncounterDeck
      focusCards cards do
        chooseOneM iid do
          for_ cards \card -> cardLabeled card do
            unfocusCards
            obtainCard card
            push $ AddToVictory (Just iid) (CardIdTarget $ toCardId card)
            doStep 1 msg
      pure l
    DoStep 1 (UseThisAbility iid (isSource attrs -> True) 2) -> do
      cards <- take 4 . unDeck <$> getEncounterDeck
      focusCards cards do
        chooseOneAtATimeM iid do
          for_ cards \card -> cardLabeled card do
            chooseOneM iid $ withI18n do
              labeled' "putOnTopOfEncounterDeck" $ putCardOnTopOfDeck iid Deck.EncounterDeck card
              labeled' "putOnBottomOfEncounterDeck" $ putCardOnBottomOfDeck iid Deck.EncounterDeck card
        unfocusCards
      pure l
    _ -> ArkhamGazette <$> liftRunMessage msg attrs

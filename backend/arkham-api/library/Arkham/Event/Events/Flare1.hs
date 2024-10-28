module Arkham.Event.Events.Flare1 where

import Arkham.Asset.Helpers ()
import Arkham.Capability
import Arkham.Event.Cards qualified as Cards (flare1)
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Enemy
import Arkham.Helpers.Modifiers (ModifierType (..), getTotalSearchTargets)
import Arkham.Matcher
import Arkham.Strategy

newtype Flare1 = Flare1 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, Targetable, Sourceable)

flare1 :: EventCard Flare1
flare1 = event Flare1 Cards.flare1

instance RunMessage Flare1 where
  runMessage msg e@(Flare1 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      investigators <- select $ affectsOthers can.manipulate.deck
      fightableEnemies <- getFightableEnemyIds iid attrs
      chooseOrRunOneM iid do
        when (notNull fightableEnemies) do
          labeled "Fight" do
            sid <- getRandom
            skillTestModifiers sid attrs iid [SkillModifier #combat 3, DamageDealt 2]
            chooseFightEnemy sid iid attrs
            exile attrs
        labeled "Search for Ally" do
          push $ CheckAttackOfOpportunity iid False
          chooseTargetM iid investigators \x -> search x e x [fromTopOfDeck 9] #ally (defer e IsNotDraw)
      pure e
    SearchFound iid (isTarget attrs -> True) _ cards -> do
      targetCount <- getTotalSearchTargets iid cards 1
      when (null cards) $ continue iid []
      focusCards cards \unfocus -> do
        chooseNM iid targetCount do
          targets cards \card -> do
            push unfocus
            putCardIntoPlay iid card
            exile attrs
      pure e
    _ -> Flare1 <$> liftRunMessage msg attrs

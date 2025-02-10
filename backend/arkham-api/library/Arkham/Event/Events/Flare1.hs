module Arkham.Event.Events.Flare1 (flare1) where

import Arkham.Ability
import Arkham.Capability
import Arkham.Card
import Arkham.Event.Cards qualified as Cards (flare1)
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Enemy
import Arkham.Helpers.Modifiers hiding (skillTestModifiers)
import Arkham.Matcher
import Arkham.Message.Lifted.Action
import Arkham.Strategy

newtype Flare1 = Flare1 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, Targetable, Sourceable)

flare1 :: EventCard Flare1
flare1 = event Flare1 Cards.flare1

instance HasAbilities Flare1 where
  getAbilities (Flare1 x) = [restricted x 1 Never fightAction_]

instance RunMessage Flare1 where
  runMessage msg e@(Flare1 attrs) = runQueueT $ case msg of
    CardEnteredPlay iid (isCard attrs -> True) -> do
      fightableEnemies <- getFightableEnemyIds iid attrs
      chooseOrRunOneM iid do
        when (notNull fightableEnemies) do
          labeled "Fight" $ doStep 1 msg
        labeled "Search for Ally" $ doStep 2 msg
      pure e
    DoStep n (CardEnteredPlay _iid (isCard attrs -> True)) -> do
      pure $ Flare1 $ setMeta n attrs
    PlayThisEvent iid (is attrs -> True) -> do
      case getEventMeta @Int attrs of
        Just 1 -> do
          extendTakenActions [#fight]
          sid <- getRandom
          skillTestModifiers sid attrs iid [SkillModifier #combat 3, DamageDealt 2]
          chooseFightEnemy sid iid attrs
          exile attrs
        Just 2 -> do
          push $ CheckAttackOfOpportunity iid False
          investigators <- select $ affectsOthers can.manipulate.deck
          chooseTargetM iid investigators \x -> search x e x [fromTopOfDeck 9] #ally (defer e IsNotDraw)
        _ -> error "Invalid meta"
      pure e
    SearchFound iid (isTarget attrs -> True) _ cards -> do
      targetCount <- getTotalSearchTargets iid cards 1
      when (null cards) $ continue_ iid
      focusCards cards do
        chooseNM iid targetCount do
          targets cards \card -> do
            unfocusCards
            putCardIntoPlay iid card
            exile attrs
      pure e
    _ -> Flare1 <$> liftRunMessage msg attrs

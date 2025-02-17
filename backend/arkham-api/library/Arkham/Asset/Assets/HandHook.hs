module Arkham.Asset.Assets.HandHook (handHook) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Card
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Investigator.Projection ()
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier

newtype HandHook = HandHook AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

handHook :: AssetCard HandHook
handHook = asset HandHook Cards.handHook

instance HasAbilities HandHook where
  getAbilities (HandHook x) =
    [ restricted x 1 InYourHand $ fightAction (DiscardCardCost (toCard x))
    , controlled x 1 (not_ InYourHand) $ fightAction (exhaust x)
    ]

instance RunMessage HandHook where
  runMessage msg a@(HandHook attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      skillTestModifier sid (attrs.ability 1) iid (SkillModifier #agility 1)
      chooseFightEnemy sid iid (attrs.ability 1)
      pure a
    PassedThisSkillTest iid (isSource attrs -> True) -> do
      void $ runMaybeT do
        hand <- lift $ select $ InHandOf NotForPlay (be iid) <> basic DiscardableCard
        guard $ notNull hand
        lift $ withSkillTest \sid -> do
          chooseOneM iid do
            questionLabeled
              "Discard 1 card from your hand to deal +1 damage"
            labeled "Do not discard card" nothing
            targets hand \card -> do
              discardCard iid (attrs.ability 1) card
              skillTestModifier sid (attrs.ability 1) iid (DamageDealt 1)
      pure a
    _ -> HandHook <$> liftRunMessage msg attrs

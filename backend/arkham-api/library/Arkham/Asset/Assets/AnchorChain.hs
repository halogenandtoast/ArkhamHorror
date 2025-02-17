module Arkham.Asset.Assets.AnchorChain (anchorChain) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Card
import Arkham.Helpers.SkillTest (getSkillTestTargetedEnemy)
import Arkham.Investigator.Projection ()
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier

newtype AnchorChain = AnchorChain AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

anchorChain :: AssetCard AnchorChain
anchorChain = asset AnchorChain Cards.anchorChain

instance HasAbilities AnchorChain where
  getAbilities (AnchorChain x) =
    [ restricted x 1 InYourHand $ evadeAction (DiscardCardCost (toCard x))
    , controlled x 1 (not_ InYourHand) $ evadeAction (exhaust x)
    ]

instance RunMessage AnchorChain where
  runMessage msg a@(AnchorChain attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      skillTestModifier sid (attrs.ability 1) iid (SkillModifier #agility 1)
      chooseEvadeEnemy sid iid (attrs.ability 1)
      pure a
    PassedThisSkillTest iid (isSource attrs -> True) -> do
      void $ runMaybeT do
        enemy <- MaybeT getSkillTestTargetedEnemy
        hand <- lift $ select $ InHandOf NotForPlay (be iid) <> basic DiscardableCard
        liftGuardM $ not <$> isMatch enemy EliteEnemy
        guard $ notNull hand
        lift $ chooseOneM iid do
          questionLabeled
            "Discard 1 card from your hand to prevent that enemy from readying during the next upkeep phase?"
          labeled "Do not discard card" nothing
          targets hand \card -> do
            discardCard iid (attrs.ability 1) card
            nextPhaseModifier #upkeep (attrs.ability 1) enemy DoesNotReadyDuringUpkeep
      pure a
    _ -> AnchorChain <$> liftRunMessage msg attrs

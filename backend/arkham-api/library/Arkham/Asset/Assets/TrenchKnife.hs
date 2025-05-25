module Arkham.Asset.Assets.TrenchKnife (trenchKnife) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), controllerGets)
import Arkham.Matcher

newtype TrenchKnife = TrenchKnife AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

trenchKnife :: AssetCard TrenchKnife
trenchKnife = asset TrenchKnife Cards.trenchKnife

instance HasModifiersFor TrenchKnife where
  getModifiersFor (TrenchKnife attrs) = controllerGets attrs [ActionDoesNotCauseAttacksOfOpportunity #engage]

instance HasAbilities TrenchKnife where
  getAbilities (TrenchKnife attrs) = [restricted attrs 1 ControlsThis fightAction_]

instance RunMessage TrenchKnife where
  runMessage msg a@(TrenchKnife attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      sid <- getRandom
      enemyCount <- selectCount $ enemyEngagedWith iid
      skillTestModifier sid attrs iid (SkillModifier #combat enemyCount)
      chooseFightEnemy sid iid source
      pure a
    _ -> TrenchKnife <$> liftRunMessage msg attrs

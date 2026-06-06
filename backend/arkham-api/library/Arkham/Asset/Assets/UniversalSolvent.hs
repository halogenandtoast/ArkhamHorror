module Arkham.Asset.Assets.UniversalSolvent (universalSolvent) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Token (Token (Supply))
import Arkham.Asset.Import.Lifted
import Arkham.Enemy.Types (Field (EnemyRemainingHealth))
import Arkham.Helpers.SkillTest (getSkillTestTarget)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Trait (Trait (Obstacle))

newtype UniversalSolvent = UniversalSolvent AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

universalSolvent :: AssetCard UniversalSolvent
universalSolvent = asset UniversalSolvent Cards.universalSolvent

instance HasAbilities UniversalSolvent where
  getAbilities (UniversalSolvent a) =
    [ restricted a 1 (ControlsThis <> exists (TreacheryAt YourLocation <> TreacheryWithTrait Obstacle))
        $ actionAbilityWithCost (assetUseCost a Supply 1)
    , doesNotProvokeAttacksOfOpportunity
        $ restricted a 2 (ControlsThis <> exists (EnemyAt YourLocation <> NonEliteEnemy))
        $ actionAbilityWithCost (assetUseCost a Supply 1)
    ]

instance RunMessage UniversalSolvent where
  runMessage msg a@(UniversalSolvent attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) attrs #intellect (Fixed 1)
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      treacheries <- select $ TreacheryAt (locationWithInvestigator iid) <> TreacheryWithTrait Obstacle
      chooseTargetM iid treacheries $ toDiscardBy iid (attrs.ability 1)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      enemies <- select $ EnemyAt (locationWithInvestigator iid) <> NonEliteEnemy
      chooseTargetM iid enemies \eid -> do
        x <- fieldJust EnemyRemainingHealth eid
        sid <- getRandom
        beginSkillTest sid iid (attrs.ability 2) eid #intellect (Fixed $ x + 1)
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 2 -> True) -> do
      getSkillTestTarget >>= traverse_ (toDiscardBy iid (attrs.ability 2))
      pure a
    _ -> UniversalSolvent <$> liftRunMessage msg attrs

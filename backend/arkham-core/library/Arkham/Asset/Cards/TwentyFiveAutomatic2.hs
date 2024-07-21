module Arkham.Asset.Cards.TwentyFiveAutomatic2 (twentyFiveAutomatic2, TwentyFiveAutomatic2 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (EnemyEvaded)
import Arkham.Asset.Uses
import Arkham.Helpers.Modifiers (ModifierType (..), maybeModified)
import Arkham.Helpers.SkillTest (getSkillTestSource, getSkillTestTarget)
import Arkham.Matcher

newtype TwentyFiveAutomatic2 = TwentyFiveAutomatic2 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

twentyFiveAutomatic2 :: AssetCard TwentyFiveAutomatic2
twentyFiveAutomatic2 = asset TwentyFiveAutomatic2 Cards.twentyFiveAutomatic2

instance HasAbilities TwentyFiveAutomatic2 where
  getAbilities (TwentyFiveAutomatic2 attrs) =
    [ restrictedAbility attrs 1 ControlsThis $ fightAction $ assetUseCost attrs Ammo 1
    , controlledAbility attrs 2 (exists $ CanFightEnemy $ attrs.ability 1)
        $ freeReaction
        $ EnemyEvaded #after You
        $ EnemyAt YourLocation
    ]

instance HasModifiersFor TwentyFiveAutomatic2 where
  getModifiersFor (InvestigatorTarget iid) (TwentyFiveAutomatic2 attrs) = do
    maybeModified attrs do
      guard $ attrs `controlledBy` iid
      EnemyTarget eid' <- MaybeT getSkillTestTarget
      liftGuardM $ eid' <=~> ExhaustedEnemy
      source <- MaybeT getSkillTestSource
      guard $ isAbilitySource attrs 1 source
      pure [SkillModifier #combat 2, DamageDealt 1]
  getModifiersFor _ _ = pure []

instance RunMessage TwentyFiveAutomatic2 where
  runMessage msg a@(TwentyFiveAutomatic2 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      chooseFightEnemy iid (attrs.ability 1)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      chooseFightEnemy iid (attrs.ability 1)
      pure a
    _ -> TwentyFiveAutomatic2 <$> liftRunMessage msg attrs

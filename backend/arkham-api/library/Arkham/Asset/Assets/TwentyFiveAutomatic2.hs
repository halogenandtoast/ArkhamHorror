module Arkham.Asset.Assets.TwentyFiveAutomatic2 (twentyFiveAutomatic2, TwentyFiveAutomatic2 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (EnemyEvaded)
import Arkham.Asset.Uses
import Arkham.Helpers.Modifiers (ModifierType (..), maybeModified_)
import Arkham.Helpers.SkillTest (getSkillTestSource, getSkillTestTarget)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

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
  getModifiersFor (TwentyFiveAutomatic2 a) = case a.controller of
    Nothing -> pure mempty
    Just iid -> maybeModified_ a iid do
      EnemyTarget eid' <- MaybeT getSkillTestTarget
      liftGuardM $ eid' <=~> ExhaustedEnemy
      source <- MaybeT getSkillTestSource
      guard $ isAbilitySource a 1 source
      pure [SkillModifier #combat 2, DamageDealt 1]

instance RunMessage TwentyFiveAutomatic2 where
  runMessage msg a@(TwentyFiveAutomatic2 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      chooseFightEnemy sid iid (attrs.ability 1)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      let ab = fromJustNote "impossible" $ getAbilities a !!? 0
      chooseOneM iid $ abilityLabeled iid (decreaseAbilityActionCost ab 1) nothing
      pure a
    _ -> TwentyFiveAutomatic2 <$> liftRunMessage msg attrs

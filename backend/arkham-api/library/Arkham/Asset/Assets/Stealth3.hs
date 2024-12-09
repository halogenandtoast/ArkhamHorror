module Arkham.Asset.Assets.Stealth3 (stealth3, Stealth3 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Evade
import Arkham.Helpers.Modifiers (ModifierType (..), maybeModified_)
import Arkham.Helpers.SkillTest (getSkillTestSource, getSkillTestTarget)
import Arkham.Matcher hiding (DuringTurn)
import Arkham.SkillTestResult
import Arkham.Window qualified as Window

newtype Stealth3 = Stealth3 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stealth3 :: AssetCard Stealth3
stealth3 = asset Stealth3 Cards.stealth3

instance HasAbilities Stealth3 where
  getAbilities (Stealth3 attrs) =
    [ wantsSkillTest AnySkillTest
        $ controlledAbility attrs 1 (DuringTurn You)
        $ FastAbility' (exhaust attrs) [#evade]
    ]

instance HasModifiersFor Stealth3 where
  getModifiersFor (Stealth3 a) =
    getSkillTestTarget >>= \case
      Just (ProxyTarget (EnemyTarget eid) _) ->
        maybeModified_ a eid do
          source <- MaybeT getSkillTestSource
          guard $ isAbilitySource a 1 source
          pure [EnemyEvade (-2)]
      _ -> pure mempty

instance RunMessage Stealth3 where
  runMessage msg a@(Stealth3 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      pushM $ setTarget attrs <$> mkChooseEvade sid iid (attrs.ability 1)
      pure a
    AfterSkillTestEnds
      (isAbilitySource attrs 1 -> True)
      (ProxyTarget (EnemyTarget eid) _)
      (SucceededBy {}) -> do
        for_ attrs.controller \iid -> do
          canDisengage <- iid <=~> InvestigatorCanDisengage
          isYourTurn <- iid <=~> TurnInvestigator
          checkWhen $ Window.EnemyEvaded iid eid
          when isYourTurn $ turnModifier iid attrs eid (EnemyCannotEngage iid)
          when canDisengage do
            push $ DisengageEnemy iid eid
            push $ EnemyCheckEngagement eid
          checkAfter $ Window.EnemyEvaded iid eid
        pure a
    _ -> Stealth3 <$> liftRunMessage msg attrs

module Arkham.Asset.Assets.WellConnected3 (wellConnected3, wellConnected3Effect, WellConnected3 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (AssetExhausted)
import Arkham.Effect.Import
import Arkham.Helpers.Modifiers (ModifierType (..), maybeModified)
import Arkham.Helpers.SkillTest (getSkillTestId, getSkillTestInvestigator, withSkillTest)
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection

newtype WellConnected3 = WellConnected3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wellConnected3 :: AssetCard WellConnected3
wellConnected3 = asset WellConnected3 Cards.wellConnected3

instance HasAbilities WellConnected3 where
  getAbilities (WellConnected3 a) =
    [ wantsSkillTest (YourSkillTest #any)
        $ controlledAbility a 1 DuringAnySkillTest
        $ FastAbility (exhaust a)
    , playerLimit PerRound
        $ controlledAbility a 2 (thisExists a AssetExhausted)
        $ FastAbility
        $ ResourceCost 2
    ]

instance RunMessage WellConnected3 where
  runMessage msg a@(WellConnected3 attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      withSkillTest $ createCardEffect Cards.wellConnected3 Nothing attrs
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      readyThis attrs
      pure a
    _ -> WellConnected3 <$> liftRunMessage msg attrs

newtype WellConnected3Effect = WellConnected3Effect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wellConnected3Effect :: EffectArgs -> WellConnected3Effect
wellConnected3Effect = cardEffect WellConnected3Effect Cards.wellConnected3

instance HasModifiersFor WellConnected3Effect where
  getModifiersFor (InvestigatorTarget iid) (WellConnected3Effect a) = maybeModified a do
    sid <- MaybeT getSkillTestId
    iid' <- MaybeT getSkillTestInvestigator
    guard $ isTarget sid a.target
    guard $ iid == iid'
    resources <- lift $ field InvestigatorResources iid
    pure [AnySkillValue (resources `div` 4)]
  getModifiersFor _ _ = pure []

instance RunMessage WellConnected3Effect where
  runMessage msg e@(WellConnected3Effect attrs) = runQueueT $ case msg of
    SkillTestEnds sid _ _ | isTarget sid attrs.target -> disableReturn e
    _ -> WellConnected3Effect <$> liftRunMessage msg attrs

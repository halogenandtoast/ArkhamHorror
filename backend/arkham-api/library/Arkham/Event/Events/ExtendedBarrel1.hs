module Arkham.Event.Events.ExtendedBarrel1 (extendedBarrel1) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Helpers.SkillTest (getSkillTestInvestigator, inAttackSkillTest, getSkillTestSource)
import Arkham.Matcher
import Arkham.Message.Lifted.Upgrade

newtype ExtendedBarrel1 = ExtendedBarrel1 EventAttrs
  deriving anyclass (IsEvent, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

extendedBarrel1 :: EventCard ExtendedBarrel1
extendedBarrel1 = event ExtendedBarrel1 Cards.extendedBarrel1

instance HasModifiersFor ExtendedBarrel1 where
  getModifiersFor (ExtendedBarrel1 a) = do
    let
      usingAsset = do
        iid <- MaybeT getSkillTestInvestigator
        guard $ a.controller == iid
        attachedAsset <- MaybeT $ selectOne $ assetWithAttachedEvent a
        guardM $ lift inAttackSkillTest
        source <- MaybeT getSkillTestSource
        guard $ source.asset == Just attachedAsset
    validTest <- isJust <$> runMaybeT usingAsset
    when validTest $ modified_ a a.controller [AnySkillValue 1]

instance RunMessage ExtendedBarrel1 where
  runMessage msg e@(ExtendedBarrel1 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      assets <- getUpgradeTargets iid $ assetControlledBy iid <> #firearm
      chooseTargetM iid assets \asset -> place attrs $ AttachedToAsset asset Nothing
      pure e
    _ -> ExtendedBarrel1 <$> liftRunMessage msg attrs

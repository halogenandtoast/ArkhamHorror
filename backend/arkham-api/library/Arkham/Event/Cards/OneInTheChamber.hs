module Arkham.Event.Cards.OneInTheChamber (oneInTheChamber, oneInTheChamberEffect, OneInTheChamber (..)) where

import Arkham.Asset.Uses
import Arkham.Effect.Import
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), maybeModified)
import Arkham.Helpers.SkillTest (getSkillTestSource, inAttackSkillTest)
import Arkham.Helpers.Window (fromAsset)

newtype OneInTheChamber = OneInTheChamber EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

oneInTheChamber :: EventCard OneInTheChamber
oneInTheChamber = event OneInTheChamber Cards.oneInTheChamber

instance RunMessage OneInTheChamber where
  runMessage msg e@(OneInTheChamber attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      let asset = fromAsset attrs.windows
      placeTokens attrs asset Ammo 1
      delayIfSkillTest $ createCardEffect Cards.oneInTheChamber (effectMetaTarget asset) attrs iid
      pure e
    _ -> OneInTheChamber <$> liftRunMessage msg attrs

newtype OneInTheChamberEffect = OneInTheChamberEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

oneInTheChamberEffect :: EffectArgs -> OneInTheChamberEffect
oneInTheChamberEffect = cardEffect OneInTheChamberEffect Cards.oneInTheChamber

instance HasModifiersFor OneInTheChamberEffect where
  getModifiersFor target (OneInTheChamberEffect attrs) | attrs.target == target = do
    maybeModified attrs do
      liftGuardM inAttackSkillTest
      source <- MaybeT getSkillTestSource
      assetTarget <- hoistMaybe $ AssetTarget <$> source.asset
      guard $ Just assetTarget == attrs.metaTarget
      pure [AnySkillValue 3]
  getModifiersFor _ _ = pure []

instance RunMessage OneInTheChamberEffect where
  runMessage msg e@(OneInTheChamberEffect attrs) = runQueueT $ case msg of
    SkillTestEnded _ -> do
      whenM inAttackSkillTest do
        getSkillTestSource >>= traverse_ \source -> do
          for_ source.asset \aid -> when (Just (AssetTarget aid) == attrs.metaTarget) (disable attrs)
      pure e
    EndRound -> disableReturn e
    _ -> OneInTheChamberEffect <$> liftRunMessage msg attrs

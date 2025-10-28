module Arkham.Treachery.Cards.ThreadsOfReality (threadsOfReality) where

import Arkham.Ability
import Arkham.Asset.Types (Field (..))
import Arkham.Helpers.Modifiers (ModifierType (BlankExceptForcedAbilities), modified_)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Placement
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype ThreadsOfReality = ThreadsOfReality TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

threadsOfReality :: TreacheryCard ThreadsOfReality
threadsOfReality = treachery ThreadsOfReality Cards.threadsOfReality

instance HasModifiersFor ThreadsOfReality where
  getModifiersFor (ThreadsOfReality a) = case a.placement of
    AttachedToAsset aid _ -> modified_ a aid [BlankExceptForcedAbilities]
    _ -> pure mempty

instance HasAbilities ThreadsOfReality where
  getAbilities (ThreadsOfReality a) =
    [restricted a 1 OnSameLocation $ actionAbilityWithCost $ DiscardAssetCost $ AssetControlledBy You]

instance RunMessage ThreadsOfReality where
  runMessage msg t@(ThreadsOfReality attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      assets <- selectMax AssetCost $ assetControlledBy iid <> not_ PermanentAsset <> NonWeaknessAsset
      if null assets
        then gainSurge attrs
        else chooseOrRunOneM iid $ targets assets (attachTreachery attrs)
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> ThreadsOfReality <$> liftRunMessage msg attrs

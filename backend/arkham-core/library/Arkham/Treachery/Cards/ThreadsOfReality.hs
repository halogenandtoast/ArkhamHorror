module Arkham.Treachery.Cards.ThreadsOfReality (threadsOfReality, ThreadsOfReality (..)) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Types (Field (..))
import Arkham.Classes
import Arkham.Matcher
import Arkham.Message
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype ThreadsOfReality = ThreadsOfReality TreacheryAttrs
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

threadsOfReality :: TreacheryCard ThreadsOfReality
threadsOfReality = treachery ThreadsOfReality Cards.threadsOfReality

instance HasModifiersFor ThreadsOfReality where
  getModifiersFor target (ThreadsOfReality a) | Just target == a.attached = do
    pure $ toModifiers a [BlankExceptForcedAbilities]
  getModifiersFor _ _ = pure []

instance HasAbilities ThreadsOfReality where
  getAbilities (ThreadsOfReality a) =
    [ restrictedAbility a 1 OnSameLocation
        $ actionAbilityWithCost
        $ DiscardAssetCost
        $ AssetControlledBy You
    ]

instance RunMessage ThreadsOfReality where
  runMessage msg t@(ThreadsOfReality attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      assets <- selectMax AssetCost $ assetControlledBy iid <> not_ PermanentAsset <> NonWeaknessAsset
      player <- getPlayer iid
      if null assets
        then push $ gainSurge attrs
        else push $ chooseOrRunOne player $ targetLabels assets (only . attachTreachery attrs)
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> ThreadsOfReality <$> runMessage msg attrs

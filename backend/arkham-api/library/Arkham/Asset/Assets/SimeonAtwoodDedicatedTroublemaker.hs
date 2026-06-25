module Arkham.Asset.Assets.SimeonAtwoodDedicatedTroublemaker (simeonAtwoodDedicatedTroublemaker) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (AssetDefeated)
import Arkham.Campaigns.TheFeastOfHemlockVale.Helpers
import Arkham.Helpers.Modifiers
import Arkham.Helpers.SkillTest.Lifted (parley)
import Arkham.Matcher

newtype SimeonAtwoodDedicatedTroublemaker = SimeonAtwoodDedicatedTroublemaker AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

simeonAtwoodDedicatedTroublemaker :: AssetCard SimeonAtwoodDedicatedTroublemaker
simeonAtwoodDedicatedTroublemaker =
  assetWith SimeonAtwoodDedicatedTroublemaker Cards.simeonAtwoodDedicatedTroublemaker
    $ (healthL ?~ 3)
    . (sanityL ?~ 2)

instance HasModifiersFor SimeonAtwoodDedicatedTroublemaker where
  getModifiersFor (SimeonAtwoodDedicatedTroublemaker a) = controllerGets a [SkillModifier #agility 1]

instance HasAbilities SimeonAtwoodDedicatedTroublemaker where
  getAbilities (SimeonAtwoodDedicatedTroublemaker a) =
    [ skillTestAbility $ restricted a 1 (OnSameLocation <> youCanTriggerCodex 3) parleyAction_
    , mkAbility a 2 $ forced $ AssetDefeated #when ByAny (be a)
    ]

instance RunMessage SimeonAtwoodDedicatedTroublemaker where
  runMessage msg a@(SimeonAtwoodDedicatedTroublemaker attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      parley sid iid (attrs.ability 1) attrs #agility (Fixed 2)
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      codex iid (attrs.ability 1) 3
      pure a
    UseCardAbility _ (isSource attrs -> True) 2 ws _ -> do
      cancelWindowBatch ws
      removeFromGame attrs
      setCardAside attrs
      decreaseRelationshipLevel SimeonAtwood 1
      pure a
    _ -> SimeonAtwoodDedicatedTroublemaker <$> liftRunMessage msg attrs

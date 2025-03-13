module Arkham.Asset.Assets.SoundSupport3 (soundSupport3) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Healing
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Matcher

newtype SoundSupport3 = SoundSupport3 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

soundSupport3 :: AssetCard SoundSupport3
soundSupport3 = assetWith SoundSupport3 Cards.soundSupport3 $ (healthL ?~ 2) . (sanityL ?~ 2)

instance HasModifiersFor SoundSupport3 where
  getModifiersFor (SoundSupport3 a) = for_ a.controller \iid -> do
    modifySelect
      a
      (not_ (InvestigatorWithId iid) <> at_ (locationWithAsset a))
      [CanAssignDamageToAsset a.id, CanAssignHorrorToAsset a.id]

instance HasAbilities SoundSupport3 where
  getAbilities (SoundSupport3 x) =
    [ controlledAbility
        x
        1
        (exists $ oneOf $ [HealableAsset (x.ability 1) kind (be x) | kind <- [#horror, #damage]])
        $ freeReaction
        $ TurnBegins #when You
    ]

instance RunMessage SoundSupport3 where
  runMessage msg a@(SoundSupport3 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assetChooseHealDamageOrHorror (attrs.ability 1) iid attrs.id
      pure a
    _ -> SoundSupport3 <$> liftRunMessage msg attrs

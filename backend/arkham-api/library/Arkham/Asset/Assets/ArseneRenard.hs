module Arkham.Asset.Assets.ArseneRenard (arseneRenard) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Location (withLocationOf)
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Token qualified as Token

newtype ArseneRenard = ArseneRenard AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arseneRenard :: AssetCard ArseneRenard
arseneRenard = allyWith ArseneRenard Cards.arseneRenard (1, 3) noSlots

instance HasAbilities ArseneRenard where
  getAbilities (ArseneRenard a) =
    [ groupLimit PerGame $ restricted a 1 ControlsThis $ FastAbility Free
    , controlled
        a
        2
        ( TokensOnLocation YourLocation Token.Antiquity (atLeast 1)
            <> exists (EnemyAt YourLocation <> ExhaustedEnemy)
        )
        $ FastAbility Free
    ]

instance RunMessage ArseneRenard where
  runMessage msg a@(ArseneRenard attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      eachLocation \lid -> placeTokens (attrs.ability 1) lid Token.Antiquity 1
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      withLocationOf iid \lid -> removeTokens (attrs.ability 2) lid Token.Antiquity 1
      chooseOrRunOneM iid $ withI18n do
        countVar 1 $ labeled' "drawCards" $ drawCards iid (attrs.ability 2) 1
        countVar 2 $ labeled' "gainResources" $ gainResources iid (attrs.ability 2) 2
      pure a
    Flip _ ScenarioSource (isTarget attrs -> True) -> do
      pure $ ArseneRenard $ attrs & flippedL .~ True & visibleL .~ False
    Flip _ _ (isTarget attrs -> True) -> do
      let flipped = not $ view flippedL attrs
      pure $ ArseneRenard $ attrs & flippedL .~ flipped & visibleL .~ True
    _ -> ArseneRenard <$> liftRunMessage msg attrs

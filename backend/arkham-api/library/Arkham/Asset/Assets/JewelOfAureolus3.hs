module Arkham.Asset.Assets.JewelOfAureolus3 (jewelOfAureolus3) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (RevealChaosToken)
import Arkham.Capability
import Arkham.ChaosToken
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Taboo
import Arkham.Window qualified as Window

newtype JewelOfAureolus3 = JewelOfAureolus3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

jewelOfAureolus3 :: AssetCard JewelOfAureolus3
jewelOfAureolus3 = asset JewelOfAureolus3 Cards.jewelOfAureolus3

instance HasAbilities JewelOfAureolus3 where
  getAbilities (JewelOfAureolus3 x) =
    [ restricted x 1 ControlsThis
        $ triggered
          ( RevealChaosToken #when (colocatedWithMatch You)
              $ if tabooed TabooList20 x
                then IsSymbol
                else ChaosTokenMatchesAny (map ChaosTokenFaceIs [Skull, Cultist, Tablet, ElderThing, AutoFail])
          )
        $ exhaust x
    ]

instance RunMessage JewelOfAureolus3 where
  runMessage msg a@(JewelOfAureolus3 attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (Window.revealedChaosTokens -> tokens) _ -> do
      push $ If (Window.RevealChaosTokenAssetAbilityEffect iid tokens (toId attrs)) [Do msg]
      pure a
    Do (UseThisAbility iid (isSource attrs -> True) 1) -> do
      chooseOneM iid do
        whenM (can.draw.cards iid) do
          labeled "Draw 1 Card" $ drawCards iid (attrs.ability 1) 1
        whenM (can.gain.resources iid) do
          labeled "Take 2 Resources" $ gainResources iid (attrs.ability 1) 2
      pure a
    _ -> JewelOfAureolus3 <$> liftRunMessage msg attrs

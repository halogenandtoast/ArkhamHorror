module Arkham.Asset.Cards.SealOfTheSeventhSign5
  ( sealOfTheSeventhSign5
  , SealOfTheSeventhSign5(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Criteria
import Arkham.Matcher
import Arkham.Timing qualified as Timing
import Arkham.Token
import Arkham.Window (Window(..))
import Arkham.Window qualified as Window

newtype SealOfTheSeventhSign5 = SealOfTheSeventhSign5 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sealOfTheSeventhSign5 :: AssetCard SealOfTheSeventhSign5
sealOfTheSeventhSign5 = assetWith
  SealOfTheSeventhSign5
  Cards.sealOfTheSeventhSign5
  (discardWhenNoUsesL .~ True)

instance HasAbilities SealOfTheSeventhSign5 where
  getAbilities (SealOfTheSeventhSign5 a) =
    [ restrictedAbility a 1 (ControlsThis <> DuringSkillTest AnySkillTest)
        $ ForcedAbility
        $ RevealChaosToken Timing.After You
        $ TokenMatchesAny
        $ map TokenFaceIs [Skull, Cultist, Tablet, ElderThing]
    ]

instance RunMessage SealOfTheSeventhSign5 where
  runMessage msg a@(SealOfTheSeventhSign5 attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (Window.revealedTokens -> tokens) _
      -> do
        push $ If
          (Window.RevealTokenAssetAbilityEffect iid tokens (toId attrs))
          [SpendUses (toTarget attrs) Charge 1]
        pure a
    Discard _ (isTarget attrs -> True) -> do
      push $ RemoveFromGame (toTarget attrs)
      pure a
    _ -> SealOfTheSeventhSign5 <$> runMessage msg attrs

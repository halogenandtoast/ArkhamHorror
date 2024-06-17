module Arkham.Asset.Cards.CrypticGrimoireTextOfTheElderGuardian4 (
  crypticGrimoireTextOfTheElderGuardian4,
  CrypticGrimoireTextOfTheElderGuardian4 (..),
)
where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner hiding (PlayCard)
import Arkham.Capability
import Arkham.Draw.Types
import Arkham.Matcher
import Arkham.Prelude
import Arkham.SkillTest.Step

newtype CrypticGrimoireTextOfTheElderGuardian4 = CrypticGrimoireTextOfTheElderGuardian4 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

crypticGrimoireTextOfTheElderGuardian4 :: AssetCard CrypticGrimoireTextOfTheElderGuardian4
crypticGrimoireTextOfTheElderGuardian4 =
  asset CrypticGrimoireTextOfTheElderGuardian4 Cards.crypticGrimoireTextOfTheElderGuardian4

instance HasAbilities CrypticGrimoireTextOfTheElderGuardian4 where
  getAbilities (CrypticGrimoireTextOfTheElderGuardian4 x) =
    [ controlledAbility x 1 (DuringSkillTest $ SkillTestWithResolvedChaosTokenBy You #curse)
        $ freeReaction
        $ SkillTestStep #after ResolveChaosSymbolEffectsStep
    , controlledAbility x 2 (can.draw.cards You)
        $ ReactionAbility (WouldDrawEncounterCard #when You #any) (assetUseCost x Secret 5)
    ]

instance RunMessage CrypticGrimoireTextOfTheElderGuardian4 where
  runMessage msg a@(CrypticGrimoireTextOfTheElderGuardian4 attrs) = case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      n <- count ((== #curse) . (.face)) <$> getSkillTestResolvedChaosTokens
      push $ AddUses attrs.id Secret n
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      push $ ReplaceCurrentCardDraw iid $ newCardDraw (attrs.ability 1) iid 1
      pure a
    _ -> CrypticGrimoireTextOfTheElderGuardian4 <$> runMessage msg attrs

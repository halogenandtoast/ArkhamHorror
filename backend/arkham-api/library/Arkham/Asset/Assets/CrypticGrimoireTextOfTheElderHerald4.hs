module Arkham.Asset.Assets.CrypticGrimoireTextOfTheElderHerald4 (
  crypticGrimoireTextOfTheElderHerald4,
  CrypticGrimoireTextOfTheElderHerald4 (..),
)
where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner hiding (PlayCard)
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Prelude
import Arkham.SkillTest.Step
import Arkham.Trait (Trait (Insight))

newtype CrypticGrimoireTextOfTheElderHerald4 = CrypticGrimoireTextOfTheElderHerald4 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

crypticGrimoireTextOfTheElderHerald4 :: AssetCard CrypticGrimoireTextOfTheElderHerald4
crypticGrimoireTextOfTheElderHerald4 =
  asset CrypticGrimoireTextOfTheElderHerald4 Cards.crypticGrimoireTextOfTheElderHerald4

instance HasModifiersFor CrypticGrimoireTextOfTheElderHerald4 where
  getModifiersFor (CrypticGrimoireTextOfTheElderHerald4 a) = case a.controller of
    Just iid -> do
      yourTurn <- iid <=~> TurnInvestigator
      modifiedWhen_ a (yourTurn && a.use Secret >= 2) iid $ [CanReduceCostOf (CardWithTrait Insight) 1]
    Nothing -> pure mempty

instance HasAbilities CrypticGrimoireTextOfTheElderHerald4 where
  getAbilities (CrypticGrimoireTextOfTheElderHerald4 x) =
    [ controlledAbility x 1 (DuringSkillTest $ SkillTestWithResolvedChaosTokenBy You #curse)
        $ freeReaction
        $ SkillTestStep #after ResolveChaosSymbolEffectsStep
    , controlledAbility x 2 (DuringTurn You)
        $ ReactionAbility (PlayCard #when You $ basic $ withTrait Insight <> #event) (assetUseCost x Secret 2)
    ]

instance RunMessage CrypticGrimoireTextOfTheElderHerald4 where
  runMessage msg a@(CrypticGrimoireTextOfTheElderHerald4 attrs) = case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      n <- count ((== #curse) . (.face)) <$> getSkillTestResolvedChaosTokens
      push $ AddUses (attrs.ability 1) attrs.id Secret n
      pure a
    UseCardAbility iid (isSource attrs -> True) 2 (cardPlayed -> c) _ -> do
      pushM $ costModifier (attrs.ability 1) iid (ReduceCostOf (CardWithId c.id) 1)
      pure a
    _ -> CrypticGrimoireTextOfTheElderHerald4 <$> runMessage msg attrs

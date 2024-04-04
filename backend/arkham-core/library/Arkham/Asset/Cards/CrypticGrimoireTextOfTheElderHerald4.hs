module Arkham.Asset.Cards.CrypticGrimoireTextOfTheElderHerald4 (
  crypticGrimoireTextOfTheElderHerald4,
  CrypticGrimoireTextOfTheElderHerald4 (..),
)
where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner hiding (PlayCard)
import Arkham.Card
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Prelude
import Arkham.SkillTest.Step
import Arkham.Trait (Trait (Insight))

newtype CrypticGrimoireTextOfTheElderHerald4 = CrypticGrimoireTextOfTheElderHerald4 AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

crypticGrimoireTextOfTheElderHerald4 :: AssetCard CrypticGrimoireTextOfTheElderHerald4
crypticGrimoireTextOfTheElderHerald4 =
  asset CrypticGrimoireTextOfTheElderHerald4 Cards.crypticGrimoireTextOfTheElderHerald4

instance HasModifiersFor CrypticGrimoireTextOfTheElderHerald4 where
  getModifiersFor (InvestigatorTarget iid) (CrypticGrimoireTextOfTheElderHerald4 a) | controlledBy a iid = do
    yourTurn <- iid <=~> TurnInvestigator
    pure $ toModifiers a $ [CanReduceCostOf (CardWithTrait Insight) 1 | yourTurn, a.use Secret >= 2]
  getModifiersFor _ _ = pure []

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
      push $ AddUses attrs.id Secret n
      pure a
    UseCardAbility iid (isSource attrs -> True) 1 (cardPlayed -> c) _ -> do
      push $ costModifier (toAbilitySource attrs 1) iid (ReduceCostOf (CardWithId $ toCardId c) 1)
      pure a
    _ -> CrypticGrimoireTextOfTheElderHerald4 <$> runMessage msg attrs

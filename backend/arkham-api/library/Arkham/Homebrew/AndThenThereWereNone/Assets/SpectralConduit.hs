{- | Spectral Conduit, the memento Resolution 2 offers one investigator.

The second ability's "spend X offerings, where X is the difficulty of the test"
is a 'DynamicUseCost' over 'SkillTestDifficultyCalculation': the test is already
the current one when the @InitiatedSkillTest #when@ window opens, so the cost is
priced -- and checked for affordability -- against the difficulty actually in
force, modifiers included.
-}
module Arkham.Homebrew.AndThenThereWereNone.Assets.SpectralConduit (spectralConduit) where

import Arkham.Ability
import Arkham.Asset.Import.Lifted hiding (AssetDefeated, InvestigatorDefeated)
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Homebrew.AndThenThereWereNone.CardDefs.Assets qualified as Cards
import Arkham.Homebrew.AndThenThereWereNone.Helpers
import Arkham.Matcher
import Arkham.Token qualified as Token
import Arkham.Trait (Trait (Ally))

newtype SpectralConduit = SpectralConduit AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

spectralConduit :: AssetCard SpectralConduit
spectralConduit = asset SpectralConduit Cards.spectralConduit

instance HasAbilities SpectralConduit where
  getAbilities (SpectralConduit a) =
    scenarioI18n
      [ -- "{reaction} After a non-swarm enemy, investigator, or Ally asset at
        -- your location is defeated: Add 1 offering to Spectral Conduit."
        withI18nTooltip "spectralConduit.offering"
          $ restricted a 1 ControlsThis
          $ freeReaction
          $ oneOf
            [ EnemyDefeated #after Anyone ByAny (not_ IsSwarm <> EnemyAt YourLocation)
            , InvestigatorDefeated #after ByAny (InvestigatorAt YourLocation)
            , AssetDefeated #after ByAny (AssetWithTrait Ally <> AssetAt YourLocation)
            ]
      , -- "{reaction} When you initiate a skill test, spend X offerings, where X
        -- is the difficulty of the test: Do not reveal chaos tokens for this
        -- test."
        withI18nTooltip "spectralConduit.conceal"
          $ restricted a 2 ControlsThis
          $ triggered
            (InitiatedSkillTest #when You AnySkillType AnySkillTestValue AnySkillTest)
            (dynamicAssetUseCost a Token.Offering SkillTestDifficultyCalculation)
      ]

instance RunMessage SpectralConduit where
  runMessage msg a@(SpectralConduit attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeTokens (attrs.ability 1) attrs Token.Offering 1
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      withSkillTest \sid ->
        skillTestModifier sid (attrs.ability 2) iid DoNotDrawChaosTokensForSkillChecks
      pure a
    _ -> SpectralConduit <$> liftRunMessage msg attrs

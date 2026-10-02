module Arkham.Homebrew.DarkMatter.Assets.GrandUnifiedTheory (grandUnifiedTheory) where

import Arkham.Ability
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Homebrew.DarkMatter.CardDefs.Assets qualified as Cards
import Arkham.Homebrew.DarkMatter.Helpers (getMemories)
import Arkham.Matcher
import Arkham.Modifier

newtype GrandUnifiedTheory = GrandUnifiedTheory AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

grandUnifiedTheory :: AssetCard GrandUnifiedTheory
grandUnifiedTheory = asset GrandUnifiedTheory Cards.grandUnifiedTheory

{- | "[fast] During a skill test, spend 1 secret and exhaust Grand Unified
Theory: You get +1 skill value for each \"Memories\" you have."
-}
instance HasAbilities GrandUnifiedTheory where
  getAbilities (GrandUnifiedTheory a) =
    [ wantsSkillTest (YourSkillTest #any)
        $ controlled a 1 (DuringSkillTest #any)
        $ FastAbility (exhaust a <> assetUseCost a Secret 1)
    ]

instance RunMessage GrandUnifiedTheory where
  runMessage msg a@(GrandUnifiedTheory attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      -- "Memories" is a per-investigator campaign-log count, which no
      -- 'GameCalculation' can read, so the bonus is fixed when the ability
      -- resolves rather than recomputed as the test goes on.
      memories <- getMemories iid
      withSkillTest \sid -> skillTestModifier sid (attrs.ability 1) iid (AnySkillValue memories)
      pure a
    _ -> GrandUnifiedTheory <$> liftRunMessage msg attrs

module Arkham.Asset.Assets.WellPrepared2 (wellPrepared2, WellPrepared2 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card.CardDef
import Arkham.Matcher hiding (AssetCard)
import Arkham.Prelude
import Arkham.Projection

newtype WellPrepared2 = WellPrepared2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wellPrepared2 :: AssetCard WellPrepared2
wellPrepared2 = asset WellPrepared2 Cards.wellPrepared2

instance HasAbilities WellPrepared2 where
  getAbilities (WellPrepared2 a) =
    [ wantsSkillTest (YourSkillTest AnySkillTest)
        $ controlledAbility
          a
          1
          ( exists (not_ (be a) <> AssetControlledBy You <> AssetWithMatchingSkillTestIcon)
              <> DuringSkillTest AnySkillTest
          )
        $ FastAbility (exhaust a)
    ]

instance RunMessage WellPrepared2 where
  runMessage msg a@(WellPrepared2 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withSkillTest \sid -> do
        matchingIcons <- getSkillTestMatchingSkillIcons

        assetIds <-
          select
            $ not_ (AssetWithId $ toId attrs)
            <> assetControlledBy iid
            <> AssetWithMatchingSkillTestIcon
        assetIdsWithIconCount <- for assetIds $ \aid -> do
          x <- fieldMap AssetCard (length . filter (`member` matchingIcons) . cdSkills . toCardDef) aid
          pure (aid, x)
        player <- getPlayer iid
        choices <- for assetIdsWithIconCount $ \(aid, x) -> do
          enabled <- skillTestModifier sid (toSource attrs) (InvestigatorTarget iid) (AnySkillValue x)
          pure $ targetLabel aid [enabled]
        push $ chooseOne player choices
      pure a
    _ -> WellPrepared2 <$> runMessage msg attrs

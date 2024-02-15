module Arkham.Asset.Cards.WellPrepared2 (
  wellPrepared2,
  WellPrepared2 (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card.CardDef
import Arkham.Matcher hiding (AssetCard)
import Arkham.Projection

newtype WellPrepared2 = WellPrepared2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wellPrepared2 :: AssetCard WellPrepared2
wellPrepared2 = asset WellPrepared2 Cards.wellPrepared2

instance HasAbilities WellPrepared2 where
  getAbilities (WellPrepared2 a) =
    [ controlledAbility
        a
        1
        ( exists $ NotAsset (AssetWithId $ toId a) <> AssetControlledBy You <> AssetWithMatchingSkillTestIcon
        )
        $ FastAbility
        $ ExhaustCost
        $ toTarget a
    ]

instance RunMessage WellPrepared2 where
  runMessage msg a@(WellPrepared2 attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      matchingIcons <- getSkillTestMatchingSkillIcons

      assetIds <-
        select
          $ NotAsset (AssetWithId $ toId attrs)
          <> AssetControlledBy (InvestigatorWithId iid)
          <> AssetWithMatchingSkillTestIcon
      assetIdsWithIconCount <- for assetIds $ \aid -> do
        x <-
          fieldMap
            AssetCard
            ( length
                . filter (`member` matchingIcons)
                . cdSkills
                . toCardDef
            )
            aid
        pure (aid, x)
      player <- getPlayer iid
      push
        $ chooseOne
          player
          [ targetLabel
            aid
            [ skillTestModifier (toSource attrs) (InvestigatorTarget iid) (AnySkillValue x)
            ]
          | (aid, x) <- assetIdsWithIconCount
          ]
      pure a
    _ -> WellPrepared2 <$> runMessage msg attrs

module Arkham.Asset.Cards.WellPrepared2
  ( wellPrepared2
  , WellPrepared2(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card.CardDef
import Arkham.Cost
import Arkham.Criteria
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Matcher hiding ( AssetCard )
import Arkham.Projection
import Arkham.SkillTest
import Arkham.SkillType
import Arkham.Target

newtype WellPrepared2 = WellPrepared2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wellPrepared2 :: AssetCard WellPrepared2
wellPrepared2 = asset WellPrepared2 Cards.wellPrepared2

instance HasAbilities WellPrepared2 where
  getAbilities (WellPrepared2 a) =
    [ restrictedAbility
          a
          1
          (ControlsThis <> AssetExists
            (NotAsset (AssetWithId $ toId a)
            <> AssetControlledBy You
            <> AssetWithMatchingSkillTestIcon
            )
          )
        $ FastAbility
        $ ExhaustCost
        $ toTarget a
    ]

instance RunMessage WellPrepared2 where
  runMessage msg a@(WellPrepared2 attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      mSkillTest <- getSkillTest
      case mSkillTest of
        Nothing -> error "can only have been triggered during a skill test"
        Just skillTest -> do
          let skillType = skillTestSkillType skillTest
          assetIds <-
            selectList
            $ NotAsset (AssetWithId $ toId attrs)
            <> AssetControlledBy (InvestigatorWithId iid)
            <> AssetWithMatchingSkillTestIcon
          assetIdsWithIconCount <- for assetIds $ \aid -> do
            x <- fieldMap
              AssetCard
              (length
              . filter (`elem` [SkillIcon skillType, WildIcon])
              . cdSkills
              . toCardDef
              )
              aid
            pure (aid, x)
          push $ chooseOne
            iid
            [ targetLabel
                aid
                [ skillTestModifier (toSource attrs) (InvestigatorTarget iid)
                    $ SkillModifier skillType x
                ]
            | (aid, x) <- assetIdsWithIconCount
            ]
          pure a
    _ -> WellPrepared2 <$> runMessage msg attrs

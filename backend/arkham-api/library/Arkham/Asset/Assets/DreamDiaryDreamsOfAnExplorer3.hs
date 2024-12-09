module Arkham.Asset.Assets.DreamDiaryDreamsOfAnExplorer3 (
  dreamDiaryDreamsOfAnExplorer3,
  DreamDiaryDreamsOfAnExplorer3 (..),
)
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Investigator
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection
import Arkham.Skill.Cards qualified as Skills

newtype DreamDiaryDreamsOfAnExplorer3 = DreamDiaryDreamsOfAnExplorer3 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dreamDiaryDreamsOfAnExplorer3 :: AssetCard DreamDiaryDreamsOfAnExplorer3
dreamDiaryDreamsOfAnExplorer3 =
  asset DreamDiaryDreamsOfAnExplorer3 Cards.dreamDiaryDreamsOfAnExplorer3

instance HasModifiersFor DreamDiaryDreamsOfAnExplorer3 where
  getModifiersFor (DreamDiaryDreamsOfAnExplorer3 a) = case a.controller of
    Nothing -> pure mempty
    Just iid ->
      field InvestigatorLocation iid >>= \case
        Nothing -> pure mempty
        Just lid -> do
          valid <- fieldMap LocationShroud (maybe False (>= 4)) lid
          if valid
            then do
              essences <- findAllCards (`cardMatch` (CardOwnedBy iid <> cardIs Skills.essenceOfTheDream))
              modifyEach a essences [AddSkillIcons [#wild, #wild]]
            else pure mempty

instance HasAbilities DreamDiaryDreamsOfAnExplorer3 where
  getAbilities (DreamDiaryDreamsOfAnExplorer3 a) =
    [ controlledAbility
        a
        1
        (exists $ You <> InvestigatorWithBondedCard (cardIs Skills.essenceOfTheDream))
        $ freeReaction
        $ TurnBegins #when You
    ]

instance RunMessage DreamDiaryDreamsOfAnExplorer3 where
  runMessage msg a@(DreamDiaryDreamsOfAnExplorer3 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      essenceOfTheDream <-
        fromJustNote "must be" . listToMaybe <$> searchBonded iid Skills.essenceOfTheDream
      push $ addToHand iid essenceOfTheDream
      pure a
    _ -> DreamDiaryDreamsOfAnExplorer3 <$> runMessage msg attrs

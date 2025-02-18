module Arkham.Asset.Assets.DreamDiaryDreamsOfAChild3 (dreamDiaryDreamsOfAChild3) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Investigator
import Arkham.Helpers.Modifiers
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Projection
import Arkham.Skill.Cards qualified as Skills

newtype DreamDiaryDreamsOfAChild3 = DreamDiaryDreamsOfAChild3 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dreamDiaryDreamsOfAChild3 :: AssetCard DreamDiaryDreamsOfAChild3
dreamDiaryDreamsOfAChild3 =
  asset DreamDiaryDreamsOfAChild3 Cards.dreamDiaryDreamsOfAChild3

instance HasModifiersFor DreamDiaryDreamsOfAChild3 where
  getModifiersFor (DreamDiaryDreamsOfAChild3 a) = for_ a.controller \iid -> do
    valid <- fieldMap InvestigatorHand ((>= 8) . length) iid
    when valid do
      essences <- findAllCards (`cardMatch` (CardOwnedBy iid <> cardIs Skills.essenceOfTheDream))
      modifyEach a essences [AddSkillIcons [#wild, #wild]]

instance HasAbilities DreamDiaryDreamsOfAChild3 where
  getAbilities (DreamDiaryDreamsOfAChild3 a) =
    [ controlled
        a
        1
        (exists $ You <> InvestigatorWithBondedCard (cardIs Skills.essenceOfTheDream))
        $ freeReaction
        $ TurnBegins #when You
    ]

instance RunMessage DreamDiaryDreamsOfAChild3 where
  runMessage msg a@(DreamDiaryDreamsOfAChild3 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      essenceOfTheDream <-
        fromJustNote "must be" . listToMaybe <$> searchBonded iid Skills.essenceOfTheDream
      push $ addToHand iid essenceOfTheDream
      pure a
    _ -> DreamDiaryDreamsOfAChild3 <$> runMessage msg attrs

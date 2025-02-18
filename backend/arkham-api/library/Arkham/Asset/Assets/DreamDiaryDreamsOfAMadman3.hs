module Arkham.Asset.Assets.DreamDiaryDreamsOfAMadman3 (dreamDiaryDreamsOfAMadman3) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Investigator
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Skill.Cards qualified as Skills

newtype DreamDiaryDreamsOfAMadman3 = DreamDiaryDreamsOfAMadman3 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dreamDiaryDreamsOfAMadman3 :: AssetCard DreamDiaryDreamsOfAMadman3
dreamDiaryDreamsOfAMadman3 = asset DreamDiaryDreamsOfAMadman3 Cards.dreamDiaryDreamsOfAMadman3

instance HasModifiersFor DreamDiaryDreamsOfAMadman3 where
  getModifiersFor (DreamDiaryDreamsOfAMadman3 a) = for_ a.controller \iid -> do
    engaged <- iid <=~> InvestigatorEngagedWith AnyEnemy
    when engaged do
      essences <- findAllCards (`cardMatch` (CardOwnedBy iid <> cardIs Skills.essenceOfTheDream))
      modifyEach a essences [AddSkillIcons [#wild, #wild]]

instance HasAbilities DreamDiaryDreamsOfAMadman3 where
  getAbilities (DreamDiaryDreamsOfAMadman3 a) =
    [ controlled
        a
        1
        (exists $ You <> InvestigatorWithBondedCard (cardIs Skills.essenceOfTheDream))
        $ freeReaction
        $ TurnBegins #when You
    ]

instance RunMessage DreamDiaryDreamsOfAMadman3 where
  runMessage msg a@(DreamDiaryDreamsOfAMadman3 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      essenceOfTheDream <-
        fromJustNote "must be" . listToMaybe <$> searchBonded iid Skills.essenceOfTheDream
      push $ addToHand iid essenceOfTheDream
      pure a
    _ -> DreamDiaryDreamsOfAMadman3 <$> runMessage msg attrs

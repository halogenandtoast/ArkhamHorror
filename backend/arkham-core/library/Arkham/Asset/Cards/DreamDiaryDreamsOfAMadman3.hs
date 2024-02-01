module Arkham.Asset.Cards.DreamDiaryDreamsOfAMadman3 (
  dreamDiaryDreamsOfAMadman3,
  DreamDiaryDreamsOfAMadman3 (..),
)
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Investigator
import Arkham.Matcher
import Arkham.Skill.Cards qualified as Skills

newtype DreamDiaryDreamsOfAMadman3 = DreamDiaryDreamsOfAMadman3 AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

dreamDiaryDreamsOfAMadman3 :: AssetCard DreamDiaryDreamsOfAMadman3
dreamDiaryDreamsOfAMadman3 =
  asset DreamDiaryDreamsOfAMadman3 Cards.dreamDiaryDreamsOfAMadman3

instance HasModifiersFor DreamDiaryDreamsOfAMadman3 where
  getModifiersFor (CardIdTarget cid) (DreamDiaryDreamsOfAMadman3 a) = do
    card <- getCard cid
    mMods <- runMaybeT $ do
      guard $ card `cardMatch` cardIs Skills.essenceOfTheDream
      guard $ toCardOwner card == a.controller
      controller <- hoistMaybe a.controller
      guardM $ lift $ controller <=~> InvestigatorEngagedWith AnyEnemy
      pure $ AddSkillIcons [#wild, #wild]
    pure $ toModifiers a $ toList mMods
  getModifiersFor _ _ = pure []

instance HasAbilities DreamDiaryDreamsOfAMadman3 where
  getAbilities (DreamDiaryDreamsOfAMadman3 a) =
    [ controlledAbility
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

module Arkham.Asset.Cards.DreamDiaryDreamsOfAnExplorer3 (
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
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

dreamDiaryDreamsOfAnExplorer3 :: AssetCard DreamDiaryDreamsOfAnExplorer3
dreamDiaryDreamsOfAnExplorer3 =
  asset DreamDiaryDreamsOfAnExplorer3 Cards.dreamDiaryDreamsOfAnExplorer3

instance HasModifiersFor DreamDiaryDreamsOfAnExplorer3 where
  getModifiersFor (CardIdTarget cid) (DreamDiaryDreamsOfAnExplorer3 a) = do
    card <- getCard cid
    mMods <- runMaybeT $ do
      guard $ card `cardMatch` cardIs Skills.essenceOfTheDream
      guard $ toCardOwner card == a.controller
      controller <- hoistMaybe a.controller
      location <- MaybeT $ field InvestigatorLocation controller
      guardM $ lift $ fieldMap LocationShroud (>= 4) location
      pure $ AddSkillIcons [#wild, #wild]
    pure $ toModifiers a $ toList mMods
  getModifiersFor _ _ = pure []

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

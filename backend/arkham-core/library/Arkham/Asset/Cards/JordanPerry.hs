module Arkham.Asset.Cards.JordanPerry (
  jordanPerry,
  JordanPerry (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card
import Arkham.Matcher
import Arkham.SkillType
import Arkham.Story.Cards qualified as Story
import Arkham.Timing qualified as Timing

newtype JordanPerry = JordanPerry AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

jordanPerry :: AssetCard JordanPerry
jordanPerry = asset JordanPerry Cards.jordanPerry

instance HasAbilities JordanPerry where
  getAbilities (JordanPerry a) =
    [ restrictedAbility
        a
        1
        ( OnSameLocation
            <> InvestigatorExists
              (You <> InvestigatorWithResources (AtLeast $ Static 10))
        )
        $ ActionAbility Nothing
        $ ActionCost 1
    , mkAbility a 2 $
        ForcedAbility $
          LastClueRemovedFromAsset Timing.When $
            AssetWithId $
              toId a
    ]

instance RunMessage JordanPerry where
  runMessage msg a@(JordanPerry attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      push $ beginSkillTest iid source attrs SkillIntellect 2
      pure a
    PassedSkillTest iid _ source SkillTestInitiatorTarget {} _ _ | isSource attrs source -> do
      modifiers <- getModifiers iid
      when
        (assetClues attrs > 0 && CannotTakeControlOfClues `notElem` modifiers)
        (pushAll [RemoveClues (toTarget attrs) 1, GainClues iid 1])
      pure a
    UseCardAbility iid source 2 _ _ | isSource attrs source -> do
      langneauPerdu <- genCard Story.langneauPerdu
      push $ ReadStory iid langneauPerdu
      pure a
    _ -> JordanPerry <$> runMessage msg attrs

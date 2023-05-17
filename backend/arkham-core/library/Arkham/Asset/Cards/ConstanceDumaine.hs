module Arkham.Asset.Cards.ConstanceDumaine (
  constanceDumaine,
  ConstanceDumaine (..),
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

newtype ConstanceDumaine = ConstanceDumaine AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

constanceDumaine :: AssetCard ConstanceDumaine
constanceDumaine = asset ConstanceDumaine Cards.constanceDumaine

instance HasAbilities ConstanceDumaine where
  getAbilities (ConstanceDumaine a) =
    [ restrictedAbility a 1 OnSameLocation $
        ActionAbility Nothing $
          ActionCost
            1
    , mkAbility a 2 $
        ForcedAbility $
          LastClueRemovedFromAsset Timing.When $
            AssetWithId $
              toId a
    ]

instance RunMessage ConstanceDumaine where
  runMessage msg a@(ConstanceDumaine attrs) = case msg of
    UseCardAbility iid source 1 _ _
      | isSource attrs source ->
          a
            <$ push
              (beginSkillTest iid source (toTarget attrs) SkillIntellect 3)
    PassedSkillTest iid _ source SkillTestInitiatorTarget {} _ _
      | isSource attrs source -> do
          modifiers <- getModifiers (InvestigatorTarget iid)
          when
            (assetClues attrs > 0 && CannotTakeControlOfClues `notElem` modifiers)
            (pushAll [RemoveClues (toTarget attrs) 1, GainClues iid 1])
          pure a
    UseCardAbility iid source 2 _ _ | isSource attrs source -> do
      engramsOath <- genCard Story.engramsOath
      push $ ReadStory iid engramsOath
      pure a
    _ -> ConstanceDumaine <$> runMessage msg attrs

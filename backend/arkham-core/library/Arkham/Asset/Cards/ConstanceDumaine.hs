module Arkham.Asset.Cards.ConstanceDumaine (constanceDumaine, ConstanceDumaine (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Story.Cards qualified as Story

newtype ConstanceDumaine = ConstanceDumaine AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

constanceDumaine :: AssetCard ConstanceDumaine
constanceDumaine = asset ConstanceDumaine Cards.constanceDumaine

instance HasAbilities ConstanceDumaine where
  getAbilities (ConstanceDumaine a) =
    [ restrictedAbility a 1 OnSameLocation parleyAction_
    , groupLimit PerGame $ mkAbility a 2 $ forced $ LastClueRemovedFromAsset #when $ AssetWithId $ toId a
    ]

instance RunMessage ConstanceDumaine where
  runMessage msg a@(ConstanceDumaine attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ beginSkillTest iid (attrs.ability 1) attrs #intellect 3
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      modifiers <- getModifiers (InvestigatorTarget iid)
      when (assetClues attrs > 0 && CannotTakeControlOfClues `notElem` modifiers) do
        pushAll [RemoveClues (attrs.ability 1) (toTarget attrs) 1, GainClues iid (attrs.ability 1) 1]
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      engramsOath <- genCard Story.engramsOath
      push $ ReadStory iid engramsOath ResolveIt (Just $ toTarget attrs)
      pure a
    _ -> ConstanceDumaine <$> runMessage msg attrs

module Arkham.Asset.Cards.SebastienMoreau (sebastienMoreau, SebastienMoreau (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Story.Cards qualified as Story

newtype SebastienMoreau = SebastienMoreau AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sebastienMoreau :: AssetCard SebastienMoreau
sebastienMoreau = asset SebastienMoreau Cards.sebastienMoreau

instance HasAbilities SebastienMoreau where
  getAbilities (SebastienMoreau a) =
    [ restrictedAbility a 1 OnSameLocation parleyAction_
    , mkAbility a 2 $ forced $ LastClueRemovedFromAsset #when $ AssetWithId $ toId a
    ]

instance RunMessage SebastienMoreau where
  runMessage msg a@(SebastienMoreau attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ parley iid (attrs.ability 1) attrs #willpower (Fixed 3)
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      modifiers <- getModifiers iid
      when (assetClues attrs > 0 && CannotTakeControlOfClues `notElem` modifiers)
        $ pushAll
          [ RemoveClues (attrs.ability 1) (toTarget attrs) 1
          , GainClues iid (attrs.ability 1) 1
          ]
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      theFirstShow <- genCard Story.theFirstShow
      push $ ReadStory iid theFirstShow ResolveIt (Just $ toTarget attrs)
      pure a
    _ -> SebastienMoreau <$> runMessage msg attrs

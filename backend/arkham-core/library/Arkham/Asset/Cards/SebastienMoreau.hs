module Arkham.Asset.Cards.SebastienMoreau (
  sebastienMoreau,
  SebastienMoreau (..),
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

newtype SebastienMoreau = SebastienMoreau AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sebastienMoreau :: AssetCard SebastienMoreau
sebastienMoreau = asset SebastienMoreau Cards.sebastienMoreau

instance HasAbilities SebastienMoreau where
  getAbilities (SebastienMoreau a) =
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

instance RunMessage SebastienMoreau where
  runMessage msg a@(SebastienMoreau attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      push $ beginSkillTest iid source attrs SkillWillpower 3
      pure a
    PassedSkillTest iid _ source SkillTestInitiatorTarget {} _ _ | isSource attrs source -> do
      modifiers <- getModifiers iid
      when
        (assetClues attrs > 0 && CannotTakeControlOfClues `notElem` modifiers)
        (pushAll [RemoveClues (toTarget attrs) 1, GainClues iid 1])
      pure a
    UseCardAbility iid source 2 _ _ | isSource attrs source -> do
      theFirstShow <- genCard Story.theFirstShow
      push $ ReadStory iid theFirstShow ResolveIt (Just $ toTarget attrs)
      pure a
    _ -> SebastienMoreau <$> runMessage msg attrs

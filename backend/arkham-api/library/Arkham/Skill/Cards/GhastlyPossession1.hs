module Arkham.Skill.Cards.GhastlyPossession1 (ghastlyPossession1) where

import Arkham.Asset.Types (Field (..))
import Arkham.Criteria
import Arkham.Helpers.SkillTest (withSkillTest, withSkillTestSource)
import Arkham.Helpers.Use
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message qualified
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.Projection
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted
import Data.Aeson (Result (..))

newtype GhastlyPossession1 = GhastlyPossession1 SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ghastlyPossession1 :: SkillCard GhastlyPossession1
ghastlyPossession1 = skill GhastlyPossession1 Cards.ghastlyPossession1

instance RunMessage GhastlyPossession1 where
  runMessage msg s@(GhastlyPossession1 attrs) = runQueueT $ case msg of
    InvestigatorCommittedSkill iid sid | sid == toId attrs -> do
      withSkillTestSource \source -> do
        for_ source.asset \aid -> do
          hasDoom <- fieldMap AssetDoom (> 0) aid
          mAddAmount <- runMaybeT do
            guardM $ lift $ aid <=~> not_ AssetWithoutUses
            (uType, n) <-
              MaybeT $ fmap (listToMaybe . mapToList) . toStartingUses =<< field AssetStartingUses aid
            current <- lift $ findWithDefault 0 uType <$> field AssetUses aid
            let half = n `div` 2
            pure $ min (n - current) half

          withSkillTest \stId -> do
            chooseOneM iid $ cardI18n $ scope "ghastlyPossession1" do
              labeled' "placeDoomGainIcons" do
                placeDoom attrs (toTarget aid) 1
                skillTestModifier stId attrs attrs.cardId $ AddSkillIcons [#wild, #wild]
              when (hasDoom || isJust mAddAmount) do
                labeled' "removeDoomOrReplenish"
                  $ doStep 1 msg
      GhastlyPossession1 <$> liftRunMessage msg attrs
    DoStep 1 (InvestigatorCommittedSkill _iid sid) | sid == toId attrs -> do
      pure . GhastlyPossession1 $ attrs & setMeta True
    PassedSkillTest _iid _ _ (isTarget attrs -> True) _ _ -> do
      let
        active =
          case fromJSON attrs.meta of
            Success x -> x
            _ -> False
      when active do
        withSkillTestSource \source -> do
          for_ source.asset \aid -> do
            skillTestCardOptionEdit
              attrs
              ( \opt ->
                  opt
                    { Arkham.Message.criteria =
                        Just (exists $ AssetWithId aid <> oneOf [AssetWithAnyDoom, AssetNotAtUsesX])
                    }
              )
              (do_ msg)
      pure s
    Do (PassedSkillTest iid _ _ (isTarget attrs -> True) _ _) -> do
      withSkillTestSource \source -> do
        for_ source.asset \aid -> do
          hasDoom <- fieldMap AssetDoom (> 0) aid
          mAddAmount <- runMaybeT do
            guardM $ lift $ aid <=~> not_ AssetWithoutUses
            (uType, n) <-
              MaybeT $ fmap (listToMaybe . mapToList) . toStartingUses =<< field AssetStartingUses aid
            current <- lift $ findWithDefault 0 uType <$> field AssetUses aid
            let half = n `div` 2
            pure (uType, min (n - current) half)

          when (hasDoom || isJust mAddAmount) do
            chooseOneM iid $ cardI18n $ scope "ghastlyPossession1" do
              when hasDoom do
                labeled' "removeDoomFromAsset" do
                  removeDoom attrs (toTarget aid) 1
              for_ mAddAmount \(uType, n) ->
                labeled' "replenishUses" do
                  placeTokens attrs (toTarget aid) uType n
      pure s
    _ -> GhastlyPossession1 <$> liftRunMessage msg attrs

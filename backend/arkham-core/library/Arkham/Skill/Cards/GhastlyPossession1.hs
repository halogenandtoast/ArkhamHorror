module Arkham.Skill.Cards.GhastlyPossession1 (ghastlyPossession1, GhastlyPossession1 (..)) where

import Arkham.Asset.Types (Field (..))
import Arkham.Helpers.SkillTest (getSkillTestSource)
import Arkham.Helpers.Use
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.Projection
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted
import Arkham.Skill.Types (setMeta)
import Data.Aeson (Result (..))

newtype GhastlyPossession1 = GhastlyPossession1 SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ghastlyPossession1 :: SkillCard GhastlyPossession1
ghastlyPossession1 = skill GhastlyPossession1 Cards.ghastlyPossession1

instance RunMessage GhastlyPossession1 where
  runMessage msg s@(GhastlyPossession1 attrs) = runQueueT $ case msg of
    InvestigatorCommittedSkill iid sid | sid == toId attrs -> do
      getSkillTestSource >>= traverse_ \source -> do
        for_ source.asset \aid -> do
          hasDoom <- fieldMap AssetDoom (> 0) aid
          mAddAmount <- runMaybeT do
            guardM $ lift $ aid <=~> not_ AssetWithoutUses
            (uType, n) <-
              MaybeT $ fmap (listToMaybe . mapToList) . toStartingUses =<< field AssetStartingUses aid
            current <- lift $ findWithDefault 0 uType <$> field AssetUses aid
            let half = n `div` 2
            pure $ min (n - current) half

          chooseOneM iid do
            labeled "Place 1 doom on that asset. Ghastly Posession gains {wild}{wild}" do
              placeDoom attrs (toTarget aid) 1
              skillTestModifier attrs attrs.cardId $ AddSkillIcons [#wild, #wild]
            when (hasDoom || isJust mAddAmount) do
              labeled
                "If this test is successful, either remove 1 doom from that asset, or replenish half of its uses (rounded down)"
                $ doStep 1 msg
      pure s
    DoStep 1 (InvestigatorCommittedSkill _iid sid) | sid == toId attrs -> do
      pure . GhastlyPossession1 $ attrs & setMeta True
    PassedSkillTest iid _ _ (isTarget attrs -> True) _ _ -> do
      let
        active =
          case fromJSON attrs.meta of
            Success x -> x
            _ -> False
      when active do
        getSkillTestSource >>= traverse_ \source -> do
          for_ source.asset \aid -> do
            hasDoom <- fieldMap AssetDoom (> 0) aid
            mAddAmount <- runMaybeT do
              guardM $ lift $ aid <=~> not_ AssetWithoutUses
              (uType, n) <-
                MaybeT $ fmap (listToMaybe . mapToList) . toStartingUses =<< field AssetStartingUses aid
              current <- lift $ findWithDefault 0 uType <$> field AssetUses aid
              let half = n `div` 2
              pure $ (uType, min (n - current) half)

            when (hasDoom || isJust mAddAmount) do
              chooseOneM iid do
                when hasDoom do
                  labeled "Remove 1 doom from that asset" do
                    removeDoom attrs (toTarget aid) 1
                for_ mAddAmount \(uType, n) ->
                  labeled "Replenish half of its uses (rounded down)" do
                    placeTokens attrs (toTarget aid) uType n
      pure s
    _ -> GhastlyPossession1 <$> liftRunMessage msg attrs

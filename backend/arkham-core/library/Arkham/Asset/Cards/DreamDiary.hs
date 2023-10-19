module Arkham.Asset.Cards.DreamDiary (
  dreamDiary,
  DreamDiary (..),
)
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.CampaignLogKey
import Arkham.Helpers.Investigator
import Arkham.Matcher
import Arkham.Skill.Cards qualified as Skills

newtype DreamDiary = DreamDiary AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dreamDiary :: AssetCard DreamDiary
dreamDiary = asset DreamDiary Cards.dreamDiary

instance HasAbilities DreamDiary where
  getAbilities (DreamDiary a) =
    [ controlledAbility
        a
        1
        (exists $ You <> InvestigatorWithBondedCard (cardIs Skills.essenceOfTheDream))
        actionAbility
    , controlledAbility a 2 (NotYetRecorded YouHaveInterpretedTheDreams)
        $ freeReaction
          ( SkillTestResult
              #after
              You
              (SkillTestWithSkill $ SkillControlledBy You <> skillIs Skills.essenceOfTheDream)
              $ SuccessResult (atLeast 3)
          )
    ]

instance RunMessage DreamDiary where
  runMessage msg a@(DreamDiary attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      essenceOfTheDream <-
        fromJustNote "must be" . listToMaybe <$> searchBonded iid Skills.essenceOfTheDream
      push $ addToHand iid essenceOfTheDream
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      push $ Record YouHaveInterpretedTheDreams
      pure a
    _ -> DreamDiary <$> runMessage msg attrs

module Arkham.Asset.Cards.MedicalTexts (
  MedicalTexts (..),
  medicalTexts,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Helpers.Investigator
import Arkham.Matcher

newtype MedicalTexts = MedicalTexts AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

medicalTexts :: AssetCard MedicalTexts
medicalTexts = asset MedicalTexts Cards.medicalTexts

instance HasAbilities MedicalTexts where
  getAbilities (MedicalTexts a) =
    [restrictedAbility a 1 ControlsThis $ ActionAbility Nothing $ ActionCost 1]

instance RunMessage MedicalTexts where
  runMessage msg a@(MedicalTexts attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      let controllerId = getController attrs
      investigators <- selectList $ colocatedWith controllerId
      push
        $ chooseOne iid
        $ [ targetLabel iid' [beginSkillTest iid (toAbilitySource attrs 1) iid' #intellect 2]
          | iid' <- investigators
          ]
      pure a
    PassedThisSkillTest _ (isAbilitySource attrs 1 -> True) -> do
      mtarget <- getSkillTestTarget
      case mtarget of
        Just target@(InvestigatorTarget iid) ->
          pushWhenM (canHaveDamageHealed attrs iid)
            $ HealDamage target (toAbilitySource attrs 1) 1
        _ -> error "invalid target"
      pure a
    FailedThisSkillTest _ (isAbilitySource attrs 1 -> True) -> do
      mtarget <- getSkillTestTarget
      case mtarget of
        Just (InvestigatorTarget iid) -> push $ assignDamage iid (toAbilitySource attrs 1) 1
        _ -> error "invalid target"
      pure a
    _ -> MedicalTexts <$> runMessage msg attrs

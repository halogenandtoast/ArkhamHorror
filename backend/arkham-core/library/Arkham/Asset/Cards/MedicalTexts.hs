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
  getAbilities (MedicalTexts a) = [restrictedAbility a 1 ControlsThis #action]

instance RunMessage MedicalTexts where
  runMessage msg a@(MedicalTexts attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let controllerId = getController attrs
      investigators <- select $ affectsOthers $ colocatedWith controllerId
      player <- getPlayer iid
      push
        $ chooseOne player
        $ targetLabels investigators
        $ \iid' -> only $ beginSkillTest iid (attrs.ability 1) iid' #intellect 2
      pure a
    PassedThisSkillTest who (isAbilitySource attrs 1 -> True) -> do
      mtarget <- getSkillTestTarget
      case mtarget of
        Just target@(InvestigatorTarget iid) -> do
          whenM (withoutModifier who CannotAffectOtherPlayersWithPlayerEffectsExceptDamage)
            $ pushWhenM (canHaveDamageHealed attrs iid)
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

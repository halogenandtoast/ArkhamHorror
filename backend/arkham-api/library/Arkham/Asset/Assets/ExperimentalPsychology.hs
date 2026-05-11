module Arkham.Asset.Assets.ExperimentalPsychology (experimentalPsychology) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Effect.Builder
import Arkham.Helpers.Window (healedInvestigator)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier

newtype ExperimentalPsychology = ExperimentalPsychology AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

experimentalPsychology :: AssetCard ExperimentalPsychology
experimentalPsychology = asset ExperimentalPsychology Cards.experimentalPsychology

instance HasAbilities ExperimentalPsychology where
  getAbilities (ExperimentalPsychology a) =
    [ controlled
        a
        1
        ( oneOf
            [ exists $ HealableInvestigator (a.ability 1) #horror $ affectsColocatedMatch You
            , exists $ HealableAsset (a.ability 1) #horror (#ally <> AssetAt YourLocation)
            ]
        )
        actionAbility
    , controlled_ a 2
        $ triggered
          (InvestigatorHealed #after #horror (affectsOthers Anyone) (SourceOwnedBy You))
          (exhaust a)
    ]

instance RunMessage ExperimentalPsychology where
  runMessage msg a@(ExperimentalPsychology attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #intellect (Fixed 2)
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      investigators <- select $ HealableInvestigator (attrs.ability 1) #horror $ affectsColocated iid
      assets <- select $ HealableAsset (attrs.ability 1) #horror (#ally <> assetAtLocationWith iid)
      chooseOneM iid do
        targets investigators $ healHorrorOn (attrs.ability 1) 1
        targets assets $ healHorrorOn (attrs.ability 1) 1
      pure a
    UseCardAbility _ (isSource attrs -> True) 2 (healedInvestigator -> iid) _ -> do
      effectWithSource (attrs.ability 2) iid do
        apply $ AnySkillValue 2
        during $ #nextSkillTest iid
        removeOn #round
      pure a
    _ -> ExperimentalPsychology <$> liftRunMessage msg attrs

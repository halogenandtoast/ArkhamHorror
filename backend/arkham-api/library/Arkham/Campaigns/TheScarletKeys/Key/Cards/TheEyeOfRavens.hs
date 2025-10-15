module Arkham.Campaigns.TheScarletKeys.Key.Cards.TheEyeOfRavens (theEyeOfRavens) where

import Arkham.Ability
import Arkham.Campaigns.TheScarletKeys.Key.Cards qualified as Cards
import Arkham.Campaigns.TheScarletKeys.Key.Import.Lifted
import Arkham.Helpers.SkillTest (withSkillTest, withSkillTestInvestigator)
import Arkham.Matcher hiding (key)
import Arkham.Modifier

newtype TheEyeOfRavens = TheEyeOfRavens ScarletKeyAttrs
  deriving anyclass (IsScarletKey, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theEyeOfRavens :: ScarletKeyCard TheEyeOfRavens
theEyeOfRavens = key TheEyeOfRavens Cards.theEyeOfRavens

instance HasAbilities TheEyeOfRavens where
  getAbilities (TheEyeOfRavens a) = case a.bearer of
    InvestigatorTarget iid ->
      case a.stability of
        Stable ->
          [ restricted
              a
              1
              ( youExist (InvestigatorWithId iid)
                  <> DuringSkillTest (SkillTestAtYourLocation <> SkillTestOfInvestigator (affectsOthers Anyone))
              )
              $ FastAbility Free
          ]
        Unstable -> [restricted a 1 (youExist (InvestigatorWithId iid)) $ FastAbility Free]
    _ -> []

instance RunMessage TheEyeOfRavens where
  runMessage msg k@(TheEyeOfRavens attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> liftRunMessage (CampaignSpecific "shift[09519]" Null) k
    CampaignSpecific "shift[09519]" _ -> do
      shiftKey attrs do
        when attrs.stable do
          withSkillTest \sid -> do
            withSkillTestInvestigator \iid -> do
              skillTestModifier sid attrs iid (BaseSkill 6)
          withInvestigatorBearer attrs (`flipOver` attrs)
        when attrs.unstable $ batched \_ -> do
          withInvestigatorBearer attrs \iid -> do
            drawEncounterCardAndThen iid attrs $ flipOver iid attrs
      pure k
    _ -> TheEyeOfRavens <$> liftRunMessage msg attrs

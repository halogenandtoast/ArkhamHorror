module Arkham.Treachery.Cards.HospitalDebts (hospitalDebts) where

import Arkham.Ability hiding (you)
import Arkham.Effect.Builder
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Script
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted hiding (moveTokens)

newtype HospitalDebts = HospitalDebts TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, Targetable)

hospitalDebts :: TreacheryCard HospitalDebts
hospitalDebts = treachery HospitalDebts Cards.hospitalDebts

instance HasAbilities HospitalDebts where
  getAbilities (HospitalDebts a) =
    wantsSkillTest
      (maybe (NotSkillTest AnySkillTest) (SkillTestOfInvestigator . InvestigatorWithId) a.owner)
      ( limitedAbility (PlayerLimit PerRound 2)
          $ restricted a 1 (OnSameLocation <> youExist InvestigatorWithAnyResources)
          $ FastAbility Free
      )
      : [ restricted a 2 (ResourcesOnThis $ lessThan 6) $ forcedOnElimination iid
        | iid <- toList a.inThreatAreaOf
        ]

instance RunMessage HospitalDebts where
  runMessage = script do
    revelation placeInYourThreatArea
    onAbility 1 $ moveTokens you this #resource 1
    onAbility 2 $ effect you do
      during #resolution
      apply $ XPModifier "Hospital Debts" (-2)

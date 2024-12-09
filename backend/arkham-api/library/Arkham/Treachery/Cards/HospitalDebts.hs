module Arkham.Treachery.Cards.HospitalDebts (hospitalDebts) where

import Arkham.Ability
import Arkham.Classes
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Prelude
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype HospitalDebts = HospitalDebts TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

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
  runMessage msg t@(HospitalDebts attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      pushAll [SpendResources iid 1, PlaceResources (attrs.ability 1) (toTarget attrs) 1]
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      resolutionModifier (attrs.ability 2) iid (XPModifier "Hospital Debts" (-2))
      pure t
    _ -> HospitalDebts <$> liftRunMessage msg attrs

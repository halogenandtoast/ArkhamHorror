module Arkham.Treachery.Cards.HospitalDebts (
  HospitalDebts (..),
  hospitalDebts,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Helpers
import Arkham.Treachery.Runner

newtype HospitalDebts = HospitalDebts TreacheryAttrs
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, Generic, ToJSON, FromJSON, Entity)

hospitalDebts :: TreacheryCard HospitalDebts
hospitalDebts = treachery HospitalDebts Cards.hospitalDebts

instance HasModifiersFor HospitalDebts where
  getModifiersFor (InvestigatorTarget iid) (HospitalDebts attrs) = do
    pure $ toModifiers attrs $ do
      guard $ treacheryOnInvestigator iid attrs
      guard $ attrs.resources < 6
      pure $ XPModifier (-2)
  getModifiersFor _ _ = pure []

instance HasAbilities HospitalDebts where
  getAbilities (HospitalDebts a) =
    limitedAbility
      (PlayerLimit PerRound 2)
      ( restrictedAbility
          a
          1
          (OnSameLocation <> InvestigatorExists (You <> InvestigatorWithResources (atLeast 1)))
          $ FastAbility Free
      )
      : [ restrictedAbility a 2 (ResourcesOnThis $ lessThan 6) $ forcedOnElimination iid
        | iid <- toList (treacheryOwner a)
        ]

instance RunMessage HospitalDebts where
  runMessage msg t@(HospitalDebts attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      push $ AttachTreachery (toId attrs) (toTarget iid)
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      pushAll [SpendResources iid 1, PlaceResources (toAbilitySource attrs 1) (toTarget attrs) 1]
      pure t
    _ -> HospitalDebts <$> runMessage msg attrs

module Arkham.Treachery.Cards.HospitalDebts (
  HospitalDebts (..),
  hospitalDebts,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.GameValue
import Arkham.Matcher
import Arkham.Message hiding (InvestigatorEliminated)
import Arkham.Modifier
import Arkham.Timing qualified as Timing
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
    let resources' = treacheryResources attrs
    pure $ toModifiers attrs [XPModifier (-2) | treacheryOnInvestigator iid attrs && resources' < 6]
  getModifiersFor _ _ = pure []

instance HasAbilities HospitalDebts where
  getAbilities (HospitalDebts a) =
    limitedAbility
      (PlayerLimit PerRound 2)
      ( restrictedAbility
          a
          1
          (OnSameLocation <> InvestigatorExists (You <> InvestigatorWithResources (AtLeast $ Static 1)))
          $ FastAbility Free
      )
      : [ restrictedAbility a 2 (ResourcesOnThis $ LessThan $ Static 6)
          $ ForcedAbility
          $ OrWindowMatcher [GameEnds Timing.When, InvestigatorEliminated Timing.When (InvestigatorWithId iid)]
        | iid <- maybeToList (treacheryOwner a)
        ]

instance RunMessage HospitalDebts where
  runMessage msg t@(HospitalDebts attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      push $ AttachTreachery (toId attrs) (InvestigatorTarget iid)
      pure t
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      pushAll [SpendResources iid 1, PlaceResources (toAbilitySource attrs 1) (toTarget attrs) 1]
      pure t
    _ -> HospitalDebts <$> runMessage msg attrs

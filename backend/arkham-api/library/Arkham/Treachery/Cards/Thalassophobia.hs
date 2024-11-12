module Arkham.Treachery.Cards.Thalassophobia (thalassophobia, Thalassophobia (..)) where

import Arkham.Matcher
import Arkham.Modifier
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Thalassophobia = Thalassophobia TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

thalassophobia :: TreacheryCard Thalassophobia
thalassophobia = treachery Thalassophobia Cards.thalassophobia

instance RunMessage Thalassophobia where
  runMessage msg t@(Thalassophobia attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> do
      fullyFlooded <- select $ InvestigatorAt FullyFloodedLocation
      partiallyFlooded <- select $ InvestigatorAt PartiallyFloodedLocation
      if null fullyFlooded && null partiallyFlooded
        then gainSurge attrs
        else do
          for_ partiallyFlooded \iid -> assignHorror iid attrs 1
          for_ fullyFlooded \iid ->
            temporaryModifier iid attrs (CannotCancelHorrorFrom $ toSource attrs)
              $ directHorror iid attrs 1
      pure t
    _ -> Thalassophobia <$> liftRunMessage msg attrs

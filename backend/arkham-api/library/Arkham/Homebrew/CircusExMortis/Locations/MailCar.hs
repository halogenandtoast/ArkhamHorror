module Arkham.Homebrew.CircusExMortis.Locations.MailCar (mailCar) where

import Arkham.Ability
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype MailCar = MailCar LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mailCar :: LocationCard MailCar
mailCar = location MailCar Cards.mailCar 4 (Static 2)

instance HasAbilities MailCar where
  getAbilities (MailCar a) =
    extendRevealed1 a
      $ playerLimit PerRound
      $ restricted a 1 (Here <> DuringPhase #upkeep <> thisExists a LocationWithoutClues)
      $ freeReaction (DrewCardsFromOwnDeck #after You)

instance RunMessage MailCar where
  runMessage msg l@(MailCar attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      drawCards iid (attrs.ability 1) 1
      pure l
    _ -> MailCar <$> liftRunMessage msg attrs

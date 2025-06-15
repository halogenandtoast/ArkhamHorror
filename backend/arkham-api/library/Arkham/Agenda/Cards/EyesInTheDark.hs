module Arkham.Agenda.Cards.EyesInTheDark (eyesInTheDark) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Helpers.Investigator
import Arkham.Helpers.Location
import Arkham.Matcher hiding (InvestigatorDefeated)

newtype EyesInTheDark = EyesInTheDark AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

eyesInTheDark :: AgendaCard EyesInTheDark
eyesInTheDark = agenda (2, A) EyesInTheDark Cards.eyesInTheDark (StaticWithPerPlayer 12 1)

instance HasAbilities EyesInTheDark where
  getAbilities (EyesInTheDark a) =
    [restricted a 1 (exists $ YourLocation <> LocationWithoutClues) actionAbility]

instance RunMessage EyesInTheDark where
  runMessage msg a@(EyesInTheDark attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      locationSymbols <- toConnections =<< getJustLocation iid
      push
        $ Explore
          iid
          (toSource attrs)
          (CardWithOneOf $ map CardWithPrintedLocationSymbol locationSymbols)
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      eachInvestigator \iid -> do
        sufferPhysicalTrauma iid 1
        investigatorDefeated attrs iid
      pure a
    _ -> EyesInTheDark <$> liftRunMessage msg attrs

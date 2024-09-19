module Arkham.Location.Cards.IdolChamber (idolChamber, IdolChamber (..)) where

import Arkham.Ability
import Arkham.Campaigns.TheInnsmouthConspiracy.Memory
import Arkham.Key
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers (connectsToAdjacent)
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.ThePitOfDespair.Helpers

newtype IdolChamber = IdolChamber LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

idolChamber :: LocationCard IdolChamber
idolChamber = locationWith IdolChamber Cards.idolChamber 4 (PerPlayer 1) connectsToAdjacent

instance HasAbilities IdolChamber where
  getAbilities (IdolChamber a) =
    extendRevealed
      a
      [ restricted a 1 (KeyIsSetAside BlueKey) $ forced $ RevealLocation #after Anyone (be a)
      , groupLimit PerGame
          $ restricted a 2 (Here <> thisIs a LocationWithoutClues <> youExist (InvestigatorWithKey PurpleKey))
          $ FastAbility Free
      ]

instance RunMessage IdolChamber where
  runMessage msg l@(IdolChamber attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeKey attrs BlueKey
      pure l
    UseThisAbility _iid (isSource attrs -> True) 2 -> do
      flashback Flashback4
      recoverMemory AnEncounterWithASecretCult
      removeChaosToken #elderthing
      pure l
    _ -> IdolChamber <$> liftRunMessage msg attrs

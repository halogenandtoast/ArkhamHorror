module Arkham.Homebrew.CircusExMortis.Locations.CircusEncampment (circusEncampment) where

import Arkham.Ability
import Arkham.Homebrew.CircusExMortis.Helpers (getSealedMoonTokens, releaseMoonToken)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype CircusEncampment = CircusEncampment LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

circusEncampment :: LocationCard CircusEncampment
circusEncampment =
  locationWith CircusEncampment Cards.circusEncampment 4 (Static 0) connectsToAdjacent

instance HasModifiersFor CircusEncampment where
  getModifiersFor (CircusEncampment a) = do
    -- "Investigators cannot enter Circus Encampment while there are fewer than 5
    -- locations connected to it."
    connected <- selectCount $ connectedTo (be a)
    modifySelect a Anyone [CannotEnter a.id | connected < 5]

instance HasAbilities CircusEncampment where
  getAbilities (CircusEncampment a) =
    extendRevealed1 a
      $ restricted a 1 Here
      $ freeReaction
      $ TurnEnds #when You

instance RunMessage CircusEncampment where
  runMessage msg l@(CircusEncampment attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      -- "Release any amount of moon tokens sealed on your investigator card."
      moons <- getSealedMoonTokens iid
      chooseSomeM iid "Done releasing moon tokens" do
        for_ moons \token ->
          targeting (ChaosTokenTarget token) $ releaseMoonToken token
      pure l
    _ -> CircusEncampment <$> liftRunMessage msg attrs

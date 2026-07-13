module Arkham.Location.Cards.CircusEncampmentCircusExMortis (circusEncampmentCircusExMortis) where

import Arkham.Ability
import Arkham.Campaigns.CircusExMortis.Helpers (getSealedMoonTokens, releaseMoonToken)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype CircusEncampmentCircusExMortis = CircusEncampmentCircusExMortis LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

circusEncampmentCircusExMortis :: LocationCard CircusEncampmentCircusExMortis
circusEncampmentCircusExMortis =
  locationWith CircusEncampmentCircusExMortis Cards.circusEncampmentCircusExMortis 4 (Static 0) connectsToAdjacent

instance HasModifiersFor CircusEncampmentCircusExMortis where
  getModifiersFor (CircusEncampmentCircusExMortis a) = do
    -- "Investigators cannot enter Circus Encampment while there are fewer than 5
    -- locations connected to it."
    connected <- selectCount $ connectedTo (be a)
    modifySelect a Anyone [CannotEnter a.id | connected < 5]

instance HasAbilities CircusEncampmentCircusExMortis where
  getAbilities (CircusEncampmentCircusExMortis a) =
    extendRevealed1 a
      $ restricted a 1 Here
      $ freeReaction
      $ TurnEnds #when You

instance RunMessage CircusEncampmentCircusExMortis where
  runMessage msg l@(CircusEncampmentCircusExMortis attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      -- "Release any amount of moon tokens sealed on your investigator card."
      moons <- getSealedMoonTokens iid
      chooseSomeM iid "Done releasing moon tokens" do
        for_ moons \token ->
          targeting (ChaosTokenTarget token) $ releaseMoonToken token
      pure l
    _ -> CircusEncampmentCircusExMortis <$> liftRunMessage msg attrs

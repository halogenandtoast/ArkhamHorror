module Arkham.Location.Cards.ControlStation (controlStation) where

import Arkham.Card.CardDef
import Arkham.Direction
import Arkham.Helpers.Act
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Grid
import Arkham.Location.Import.Lifted
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection

newtype ControlStation = ControlStation LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

controlStation :: LocationCard ControlStation
controlStation = locationWith ControlStation Cards.controlStation 1 (PerPlayer 2) connectsToAdjacent

instance HasModifiersFor ControlStation where
  getModifiersFor (ControlStation attrs) = do
    n <- getCurrentActStep
    when (n == 2) do
      for_ attrs.position \pos -> do
        for_ [North, East, South, West] \dir -> do
          selectEach (LocationInPosition $ updatePosition pos dir) \loc -> do
            def <- field LocationCardDef loc
            for_ (lookup "rails" (cdMeta def)) \rails -> do
              when (oppositeDirection dir `elem` toResultDefault [] rails) do
                modifySelf attrs [ConnectedToWhen (be attrs) (LocationWithId loc)]

instance HasAbilities ControlStation where
  getAbilities (ControlStation a) = extendRevealed a []

instance RunMessage ControlStation where
  runMessage msg (ControlStation attrs) = runQueueT $ case msg of
    ScenarioSpecific "theCaveIn" _ -> do
      pure $ ControlStation $ attrs & connectsToL .~ mempty
    _ -> ControlStation <$> liftRunMessage msg attrs

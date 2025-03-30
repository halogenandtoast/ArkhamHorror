module Arkham.Location.Cards.LabyrinthineChamber (labyrinthineChamber) where

import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Helpers.SkillTest (isSkillTestAt)
import Arkham.Investigator.Types (Field (..))
import Arkham.Key
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype LabyrinthineChamber = LabyrinthineChamber LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

labyrinthineChamber :: LocationCard LabyrinthineChamber
labyrinthineChamber = locationWith LabyrinthineChamber Cards.labyrinthineChamber 3 (PerPlayer 2) connectsToAdjacent

instance HasModifiersFor LabyrinthineChamber where
  getModifiersFor (LabyrinthineChamber attrs) = do
    whenM (isSkillTestAt attrs) do
      let hereKeys = mapMaybe (preview _TokenKey) (toList $ locationKeys attrs)
      iKeys <-
        concatMap (mapMaybe (preview _TokenKey) . toList)
          <$> selectField InvestigatorKeys (investigator_ $ at_ $ be attrs)
      let faces = nub $ map (.face) (hereKeys <> iKeys)
      modifySelect attrs (mapOneOf ChaosTokenFaceIs faces) [DoubleModifiersOnChaosTokens]

instance RunMessage LabyrinthineChamber where
  runMessage msg (LabyrinthineChamber attrs) =
    LabyrinthineChamber <$> runMessage msg attrs

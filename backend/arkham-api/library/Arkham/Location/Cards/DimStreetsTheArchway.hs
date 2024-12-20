module Arkham.Location.Cards.DimStreetsTheArchway (dimStreetsTheArchway) where

import Arkham.Ability
import Arkham.Game.Helpers
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Location.Types (revealedL)
import Arkham.Matcher hiding (NonAttackDamageEffect)
import Arkham.Scenarios.DimCarcosa.Helpers
import Arkham.Story.Cards qualified as Story

newtype DimStreetsTheArchway = DimStreetsTheArchway LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dimStreetsTheArchway :: LocationCard DimStreetsTheArchway
dimStreetsTheArchway =
  locationWith
    DimStreetsTheArchway
    Cards.dimStreetsTheArchway
    2
    (PerPlayer 1)
    ((canBeFlippedL .~ True) . (revealedL .~ True))

instance HasAbilities DimStreetsTheArchway where
  getAbilities (DimStreetsTheArchway a) =
    extend1 a $ mkAbility a 1 $ forced $ DiscoveringLastClue #after You (be a)

instance RunMessage DimStreetsTheArchway where
  runMessage msg l@(DimStreetsTheArchway attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      loseActions iid (attrs.ability 1) 1
      pure l
    Flip iid _ (isTarget attrs -> True) -> do
      readStory iid (toId attrs) Story.theArchway
      pure . DimStreetsTheArchway $ attrs & canBeFlippedL .~ False
    _ -> DimStreetsTheArchway <$> liftRunMessage msg attrs

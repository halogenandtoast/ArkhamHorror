module Arkham.Location.Cards.DimStreetsTheKingsParade (dimStreetsTheKingsParade) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Location.Types (revealedL)
import Arkham.Matcher hiding (NonAttackDamageEffect)
import Arkham.Scenarios.DimCarcosa.Helpers
import Arkham.Story.Cards qualified as Story

newtype DimStreetsTheKingsParade = DimStreetsTheKingsParade LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dimStreetsTheKingsParade :: LocationCard DimStreetsTheKingsParade
dimStreetsTheKingsParade =
  locationWith DimStreetsTheKingsParade Cards.dimStreetsTheKingsParade 2 (PerPlayer 1)
    $ (canBeFlippedL .~ True)
    . (revealedL .~ True)

instance HasAbilities DimStreetsTheKingsParade where
  getAbilities (DimStreetsTheKingsParade a) =
    extendRevealed1 a
      $ mkAbility a 1
      $ forced
      $ DiscoveringLastClue #after You (be a)

instance RunMessage DimStreetsTheKingsParade where
  runMessage msg l@(DimStreetsTheKingsParade attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      loseActions iid (attrs.ability 1) 1
      pure l
    Flip iid _ (isTarget attrs -> True) -> do
      readStory iid (toId attrs) Story.theKingsParade
      pure . DimStreetsTheKingsParade $ attrs & canBeFlippedL .~ False
    _ -> DimStreetsTheKingsParade <$> liftRunMessage msg attrs

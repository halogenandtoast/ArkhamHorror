module Arkham.Location.Cards.DimStreetsMappingTheStreets (dimStreetsMappingTheStreets) where

import Arkham.Ability
import Arkham.Game.Helpers
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Location.Types (revealedL)
import Arkham.Matcher hiding (NonAttackDamageEffect)
import Arkham.Scenarios.DimCarcosa.Helpers
import Arkham.Story.Cards qualified as Story

newtype DimStreetsMappingTheStreets = DimStreetsMappingTheStreets LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dimStreetsMappingTheStreets :: LocationCard DimStreetsMappingTheStreets
dimStreetsMappingTheStreets =
  locationWith DimStreetsMappingTheStreets Cards.dimStreetsMappingTheStreets 2 (PerPlayer 1)
    $ (canBeFlippedL .~ True)
    . (revealedL .~ True)

instance HasAbilities DimStreetsMappingTheStreets where
  getAbilities (DimStreetsMappingTheStreets a) =
    extend1 a $ mkAbility a 1 $ forced $ DiscoveringLastClue #after You (be a)

instance RunMessage DimStreetsMappingTheStreets where
  runMessage msg l@(DimStreetsMappingTheStreets attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      loseActions iid (attrs.ability 1) 1
      pure l
    Flip iid _ (isTarget attrs -> True) -> do
      readStory iid (toId attrs) Story.mappingTheStreets
      pure . DimStreetsMappingTheStreets $ attrs & canBeFlippedL .~ False
    _ -> DimStreetsMappingTheStreets <$> liftRunMessage msg attrs

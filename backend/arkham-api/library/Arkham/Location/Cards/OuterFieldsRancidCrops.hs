module Arkham.Location.Cards.OuterFieldsRancidCrops (outerFieldsRancidCrops) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.TheLongestNight.Helpers
import Arkham.Token

newtype OuterFieldsRancidCrops = OuterFieldsRancidCrops LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

outerFieldsRancidCrops :: LocationCard OuterFieldsRancidCrops
outerFieldsRancidCrops = symbolLabel $ locationWith OuterFieldsRancidCrops Cards.outerFieldsRancidCrops 0 (Static 0) connectsToAdjacent

instance HasAbilities OuterFieldsRancidCrops where
  getAbilities (OuterFieldsRancidCrops a) =
    extendRevealed
      a
      [ mkAbility a 1 $ freeReaction $ DiscoveringLastClue #after You (be a)
      , playerLimit PerTurn
          $ restricted a 2 (Here <> DuringTurn You <> exists (LocationWithHorror (atLeast 1)))
          $ FastAbility Free
      ]

instance RunMessage OuterFieldsRancidCrops where
  runMessage msg l@(OuterFieldsRancidCrops attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeDecoy attrs attrs.id
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      decoyLocations <- select $ LocationWithHorror (atLeast 1)
      chooseTargetM iid decoyLocations \fromLid -> do
        destinations <- select $ LocationWithoutModifier CannotHaveDecoys <> not_ (LocationWithId fromLid)
        chooseTargetM iid destinations \toLid ->
          moveTokens (attrs.ability 2) fromLid toLid Horror 1
      pure l
    _ -> OuterFieldsRancidCrops <$> liftRunMessage msg attrs

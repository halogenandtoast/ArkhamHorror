module Arkham.Location.Cards.SubmergedPassageway (submergedPassageway) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype SubmergedPassageway = SubmergedPassageway LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

submergedPassageway :: LocationCard SubmergedPassageway
submergedPassageway = locationWith SubmergedPassageway Cards.submergedPassageway 5 (PerPlayer 1) connectsToAdjacent

instance HasAbilities SubmergedPassageway where
  getAbilities (SubmergedPassageway a) =
    extendRevealed1 a
      $ restricted
        a
        1
        ( Here
            <> thisExists a LocationWithAnyClues
            <> exists
              (not_ (be a) <> UnrevealedLocation <> oneOf [LocationInRowOf (be a), LocationInColumnOf (be a)])
        )
        actionAbility

instance RunMessage SubmergedPassageway where
  runMessage msg l@(SubmergedPassageway attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      ls <-
        select
          $ not_ (be attrs)
          <> UnrevealedLocation
          <> oneOf [LocationInRowOf (be attrs), LocationInColumnOf (be attrs)]
      chooseTargetM iid ls \lid ->
        moveTokens (attrs.ability 1) attrs lid #clue 1
      pure l
    _ -> SubmergedPassageway <$> liftRunMessage msg attrs

module Arkham.Location.Cards.LostCampsite (lostCampsite) where

import Arkham.Ability
import Arkham.Card
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype LostCampsite = LostCampsite LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lostCampsite :: LocationCard LostCampsite
lostCampsite = location LostCampsite Cards.lostCampsite 4 (Static 1)

instance HasAbilities LostCampsite where
  getAbilities (LostCampsite a) =
    extendRevealed1 a
      $ restricted
        a
        1
        (Here <> thisExists a (LocationWithCardsUnderneath $ LengthIs $ atLeast 1))
        actionAbility

instance RunMessage LostCampsite where
  runMessage msg l@(LostCampsite attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      -- The stack stays hidden until drawn from. Turning a card over means clearing
      -- `facedown`, which is a different field from a card's flipped side, so
      -- flipping it here would leave the draw showing the encounter back.
      for_ (listToMaybe attrs.underneath) $ setFacedown False >=> drawCard iid
      pure l
    _ -> LostCampsite <$> liftRunMessage msg attrs

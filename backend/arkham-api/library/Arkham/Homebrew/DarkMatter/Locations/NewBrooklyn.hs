module Arkham.Homebrew.DarkMatter.Locations.NewBrooklyn (newBrooklyn) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype NewBrooklyn = NewBrooklyn LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

newBrooklyn :: LocationCard NewBrooklyn
newBrooklyn = location NewBrooklyn Cards.newBrooklyn 2 (Static 1)

-- | "[reaction] At the end of your turn: Heal 1 damage." / "[action]: Resign."
instance HasAbilities NewBrooklyn where
  getAbilities (NewBrooklyn a) =
    extendRevealed
      a
      [ restricted a 1 (Here <> youExist InvestigatorWithAnyDamage) $ freeReaction $ TurnEnds #when You
      , locationResignAction a
      ]

instance RunMessage NewBrooklyn where
  runMessage msg l@(NewBrooklyn attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      healDamage iid (attrs.ability 1) 1
      pure l
    _ -> NewBrooklyn <$> liftRunMessage msg attrs

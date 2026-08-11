module Arkham.Homebrew.DarkMatter.Locations.MessHall (messHall) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher hiding (DuringTurn)

newtype MessHall = MessHall LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

messHall :: LocationCard MessHall
messHall = location MessHall Cards.messHall 1 (PerPlayer 2)

{- | "[reaction] When you gain any amount of resources during your turn at the
Mess Hall: Gain 1 additional resource. (Limit once per round.)"
-}
instance HasAbilities MessHall where
  getAbilities (MessHall a) =
    extendRevealed1 a
      $ playerLimit PerRound
      $ restricted a 1 (Here <> DuringTurn You)
      $ freeReaction
      $ GainsResources #when You AnySource (atLeast 1)

instance RunMessage MessHall where
  runMessage msg l@(MessHall attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      gainResources iid (attrs.ability 1) 1
      pure l
    _ -> MessHall <$> liftRunMessage msg attrs

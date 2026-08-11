module Arkham.Homebrew.DarkMatter.Locations.CrewQuarters (crewQuarters) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype CrewQuarters = CrewQuarters LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

crewQuarters :: LocationCard CrewQuarters
crewQuarters = location CrewQuarters Cards.crewQuarters 3 (PerPlayer 2)

{- | "[reaction] After you discover the last clue from this location: Each
investigator at this location gains 2 resources. (Group limit once per game.)"
-}
instance HasAbilities CrewQuarters where
  getAbilities (CrewQuarters a) =
    extendRevealed1 a
      $ groupLimit PerGame
      $ restricted a 1 Here
      $ freeReaction
      $ DiscoveringLastClue #after You (be a)

instance RunMessage CrewQuarters where
  runMessage msg l@(CrewQuarters attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      here <- select $ investigatorAt attrs.id
      for_ here \iid -> gainResources iid (attrs.ability 1) 2
      pure l
    _ -> CrewQuarters <$> liftRunMessage msg attrs

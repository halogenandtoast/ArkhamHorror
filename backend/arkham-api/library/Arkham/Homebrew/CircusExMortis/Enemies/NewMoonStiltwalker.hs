module Arkham.Homebrew.CircusExMortis.Enemies.NewMoonStiltwalker (newMoonStiltwalker) where

import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Location (getLocationOf)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf, modifySelfMaybe)
import Arkham.Homebrew.CircusExMortis.CardDefs.Enemies qualified as Cards
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher

newtype NewMoonStiltwalker = NewMoonStiltwalker EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

newMoonStiltwalker :: EnemyCard NewMoonStiltwalker
newMoonStiltwalker =
  enemy NewMoonStiltwalker Cards.newMoonStiltwalker
    & setSpawnAt (FirstLocation [LocationWithoutInvestigators, Anywhere])

instance HasModifiersFor NewMoonStiltwalker where
  getModifiersFor (NewMoonStiltwalker a) = do
    modifySelf a [AddKeyword Keyword.Hunter, AddKeyword Keyword.Alert]
    allLocations <- select Anywhere
    modifySelfMaybe a do
      loc <- MaybeT $ getLocationOf a
      pure [HunterConnectedTo x | x <- allLocations, x /= loc]

instance RunMessage NewMoonStiltwalker where
  runMessage msg (NewMoonStiltwalker attrs) =
    NewMoonStiltwalker <$> runMessage msg attrs

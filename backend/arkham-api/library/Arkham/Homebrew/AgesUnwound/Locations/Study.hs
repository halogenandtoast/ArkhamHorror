module Arkham.Homebrew.AgesUnwound.Locations.Study (study) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect, modifySelf)
import Arkham.Homebrew.AgesUnwound.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.AgesUnwound.Scenarios.TheMyriadGentleman.Helpers
import Arkham.Keyword qualified as Keyword
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype Study = Study LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

study :: LocationCard Study
study = symbolLabel $ location Study Cards.study 4 (PerPlayer 2)

instance HasModifiersFor Study where
  getModifiersFor (Study a) = do
    -- Unrevealed: "The door to this room is locked. You may not enter the Study."
    whenUnrevealed a $ modifySelf a [Blocked]
    -- "While at least one investigator is at Study, each copy of The Myriad
    -- Gentleman gains 'Patrol (Study).'"
    anyone <- selectAny $ investigatorAt a.id
    when anyone
      $ modifySelect
        a
        (enemyIs Enemies.theMyriadGentleman_042)
        [AddKeyword $ Keyword.Patrol (LocationWithId a.id)]

{- | "Forced - At the end of the round: Each investigator at Study spawns a copy
of The Myriad Gentleman at Entrance Hall."
-}
instance HasAbilities Study where
  getAbilities (Study a) =
    extendRevealed1 a
      $ restricted a 1 (exists $ investigatorAt a.id)
      $ forced
      $ RoundEnds #when

instance RunMessage Study where
  runMessage msg l@(Study attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      entranceHall <- selectJust $ locationIs Cards.entranceHall
      iids <- select $ investigatorAt attrs.id
      for_ iids \iid -> spawnMyriadCopiesAt iid 1 entranceHall
      pure l
    _ -> Study <$> liftRunMessage msg attrs

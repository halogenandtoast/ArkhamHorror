module Arkham.Homebrew.AgesUnwound.Locations.MasterBedroom (masterBedroom) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Homebrew.AgesUnwound.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.AgesUnwound.Scenarios.TheMyriadGentleman.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype MasterBedroom = MasterBedroom LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

masterBedroom :: LocationCard MasterBedroom
masterBedroom = symbolLabel $ location MasterBedroom Cards.masterBedroom 4 (PerPlayer 1)

instance HasAbilities MasterBedroom where
  getAbilities (MasterBedroom a) =
    extendRevealed
      a
      [ -- "Forced - After you fail a skill test while at Master Bedroom: Spawn 1
        -- copy of The Myriad Gentleman engaged with you."
        restricted a 1 Here $ forced $ SkillTestResult #after You AnySkillTest #failure
      , -- "[reaction] After you successfully investigate Master Bedroom:
        -- Automatically evade a copy of The Myriad Gentlemen at Master Bedroom."
        restricted a 2 (exists $ ReadyEnemy <> copiesAt a)
          $ freeReaction
          $ SuccessfulInvestigation #after You (be a)
      ]

copiesAt :: LocationAttrs -> EnemyMatcher
copiesAt a = enemyIs Enemies.theMyriadGentleman_042 <> EnemyAt (be a)

instance RunMessage MasterBedroom where
  runMessage msg l@(MasterBedroom attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      spawnMyriadCopiesEngagedWith iid 1
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      copies <- select $ ReadyEnemy <> copiesAt attrs
      chooseTargetM iid copies $ automaticallyEvadeEnemy iid
      pure l
    _ -> MasterBedroom <$> liftRunMessage msg attrs

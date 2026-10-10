module Arkham.Homebrew.TheMasqueOfTheRedDeath.Locations.OrangeChamber (orangeChamber) where

import Arkham.ChaosToken (pattern PositiveModifier)
import Arkham.ChaosToken.Types (ChaosTokenValue (ChaosTokenValue))
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Homebrew.TheMasqueOfTheRedDeath.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.TheMasqueOfTheRedDeath.Helpers (
  chamberToll,
  describedSkullEffect,
  tollDoomMayAdvanceAgenda,
 )
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype OrangeChamber = OrangeChamber LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

orangeChamber :: LocationCard OrangeChamber
orangeChamber =
  locationWith OrangeChamber Cards.orangeChamber 5 (PerPlayer 1)
    $ costToEnterUnrevealedL
    .~ chamberToll

instance HasModifiersFor OrangeChamber where
  -- "Each ready non-Elite enemy at Orange Chamber gets +1 fight and +1 damage."
  getModifiersFor (OrangeChamber a) =
    whenRevealed a
      $ modifySelect a (enemyAt a <> ReadyEnemy <> NonEliteEnemy) [EnemyFight 1, DamageDealt 1]

instance HasAbilities OrangeChamber where
  -- "[skull]: +2." The only chamber whose token effect helps you.
  getAbilities (OrangeChamber a) = extendRevealed1 a $ describedSkullEffect 2 "" a 1

instance RunMessage OrangeChamber where
  runMessage msg l@(OrangeChamber attrs) = runQueueT do
    tollDoomMayAdvanceAgenda attrs msg
    case msg of
      UseThisAbility _ (isSource attrs -> True) 1 -> do
        withSkillTest \sid ->
          skillTestModifier sid (attrs.ability 1) sid
            $ AddChaosTokenValue (ChaosTokenValue #skull (PositiveModifier 2))
        pure l
      _ -> OrangeChamber <$> liftRunMessage msg attrs

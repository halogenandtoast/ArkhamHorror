module Arkham.Homebrew.TheMasqueOfTheRedDeath.Locations.PurpleChamber (purpleChamber) where

import Arkham.ChaosToken (pattern NegativeModifier)
import Arkham.ChaosToken.Types (ChaosToken, ChaosTokenValue (ChaosTokenValue))
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Helpers.SkillTest (getSkillTestRevealedChaosTokens, withSkillTest)
import Arkham.Homebrew.TheMasqueOfTheRedDeath.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.TheMasqueOfTheRedDeath.Helpers (
  chamberToll,
  describedSkullEffect,
  tollDoomMayAdvanceAgenda,
 )
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype PurpleChamber = PurpleChamber LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

purpleChamber :: LocationCard PurpleChamber
purpleChamber =
  locationWith PurpleChamber Cards.purpleChamber 4 (PerPlayer 1)
    $ costToEnterUnrevealedL
    .~ chamberToll

-- | "non-[elder_sign] symbol tokens" -- every symbol face but [elder_sign].
nonElderSignSymbol :: ChaosToken -> Bool
nonElderSignSymbol t = t.face.isSymbol && t.face /= #eldersign

instance HasModifiersFor PurpleChamber where
  -- "Each ready non-Elite enemy at Purple Chamber gets +1 evade and +1 horror."
  getModifiersFor (PurpleChamber a) =
    whenRevealed a
      $ modifySelect a (enemyAt a <> ReadyEnemy <> NonEliteEnemy) [EnemyEvade 1, HorrorDealt 1]

instance HasAbilities PurpleChamber where
  -- "[skull]: -1. Reveal another token. If you reveal 2 or more non-[elder_sign]
  -- symbol tokens during this test, this test automatically succeeds."
  getAbilities (PurpleChamber a) =
    extendRevealed1 a
      $ describedSkullEffect
        (-1)
        "Reveal another token. If you reveal 2 or more non-{elderSign} symbol tokens during this test, this test automatically succeeds."
        a
        1

instance RunMessage PurpleChamber where
  runMessage msg l@(PurpleChamber attrs) = runQueueT do
    tollDoomMayAdvanceAgenda attrs msg
    case msg of
      UseThisAbility iid (isSource attrs -> True) 1 -> do
        withSkillTest \sid ->
          skillTestModifier sid (attrs.ability 1) sid
            $ AddChaosTokenValue (ChaosTokenValue #skull (NegativeModifier 1))
        drawAnotherChaosToken iid
        doStep 1 msg
        pure l
      {- The extra token has to be counted too, so the tally waits for it to resolve.
      'SkillTestAutomaticallySucceeds' is only read at 'TriggerSkillTest', before any
      token is drawn, so a mid-test automatic success is 'PassSkillTest'. -}
      DoStep 1 (UseThisAbility _ (isSource attrs -> True) 1) -> do
        symbols <- count nonElderSignSymbol <$> getSkillTestRevealedChaosTokens
        when (symbols >= 2) $ push PassSkillTest
        pure l
      _ -> PurpleChamber <$> liftRunMessage msg attrs

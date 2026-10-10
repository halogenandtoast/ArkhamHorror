module Arkham.Homebrew.AgesUnwound.Acts.HellishHound (hellishHound) where

import Arkham.Ability
import Arkham.Act.Import.Lifted
import Arkham.Helpers.Investigator (getJustLocation)
import Arkham.Homebrew.AgesUnwound.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.AgesUnwound.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.AgesUnwound.Helpers (isAtOrPastFor)
import Arkham.Homebrew.AgesUnwound.Key
import Arkham.Homebrew.AgesUnwound.Scenarios.AWorldTornDownAgain.Helpers (advanceToActThreeD)
import Arkham.Matcher
import Arkham.Message.Lifted.Log (getRecordCount)

newtype HellishHound = HellishHound ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | Act 2c, reached when /the investigators used the school's front door/ a year
ago --- which is also the branch on which Scenario III recorded what happened to the
Hound of Unmaking.
-}
hellishHound :: ActCard HellishHound
hellishHound = act (2, C) HellishHound Cards.hellishHound Nothing

{- | "Objective - When /the investigators repelled the Hound of Unmaking,/ advance. /
Objective - When /the investigators put down the Hound of Unmaking,/ advance. /
Objective - When /the investigators fell to the Myriad,/ advance to Act 3d."

Polled after the doom threshold check, like every past-deck act; see
'Arkham.Homebrew.AgesUnwound.Acts.WhatCameBefore'.
-}
instance HasAbilities HellishHound where
  getAbilities (HellishHound a) =
    [mkAbility a 1 $ SilentForcedAbility $ MythosStep AfterCheckDoomThreshold]

instance RunMessage HellishHound where
  runMessage msg a@(HellishHound attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      conditionMet <-
        orM
          [ isAtOrPastFor TheInvestigatorsRepelledTheHoundOfUnmaking
          , isAtOrPastFor TheInvestigatorsPutDownTheHoundOfUnmaking
          , isAtOrPastFor TheInvestigatorsFellToTheMyriad
          ]
      when conditionMet $ advancedWithOther attrs
      pure a
    AdvanceAct (isSide D attrs -> True) _ _ -> do
      {- The back re-reads the log rather than remembering which objective fired:
      every one of these entries is written by Scenario III and cannot change
      during Scenario VI, and they are read in printed order so a replayed
      Scenario III that recorded two of them resolves the first. -}
      repelled <- isAtOrPastFor TheInvestigatorsRepelledTheHoundOfUnmaking
      putDown <- isAtOrPastFor TheInvestigatorsPutDownTheHoundOfUnmaking

      if repelled
        then do
          {- "Oh.: Spawn the set-aside Hound of Unmaking at the Rear Corridors. (If
          Rear Corridors are not in play, spawn it at the lead investigator's
          location instead.) If an amount of damage is recorded for it in your
          Campaign Log, it enters play with that much damage on it.
          Advance to Act 3c - The First Circle." -}
          lead <- getLead
          rearCorridors <- selectOne $ locationIs Locations.rearCorridors
          lid <- maybe (getJustLocation lead) pure rearCorridors
          hound <- createSetAsideEnemy Enemies.houndOfUnmaking lid
          {- Scenario III's act 2b records zero as zero when an undamaged Hound is
          repelled, so this is a plain count read with no "is it recorded" flag. -}
          damage <- getRecordCount DamageOnTheHoundOfUnmaking
          when (damage > 0) $ placeTokens attrs hound #damage damage
          advanceToAct attrs Cards.theFirstCircle C
        else
          if putDown
            then
              -- "You hear a faint wail in the distance, as a terrible beast is
              -- slain. Advance to Act 3c - The First Circle."
              advanceToAct attrs Cards.theFirstCircle C
            else advanceToActThreeD attrs
      pure a
    _ -> HellishHound <$> liftRunMessage msg attrs

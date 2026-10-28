module Arkham.Homebrew.AgesUnwound.Acts.HellishHound (hellishHound) where

import Arkham.Act.Import.Lifted
import Arkham.Helpers.Investigator (getJustLocation)
import Arkham.Homebrew.AgesUnwound.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.AgesUnwound.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.AgesUnwound.Helpers (isAtOrPastFor)
import Arkham.Homebrew.AgesUnwound.Key
import Arkham.Homebrew.AgesUnwound.Scenarios.AWorldTornDownAgain.Helpers (advanceToActThreeD)
import Arkham.Homebrew.AgesUnwound.Scenarios.AWorldTornDownAgain.Helpers qualified as Scenario
import Arkham.Matcher
import Arkham.Message.Lifted.Log (getRecordCount)

newtype HellishHound = HellishHound ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hellishHound :: ActCard HellishHound
hellishHound = act (2, C) HellishHound Cards.hellishHound Nothing

instance HasModifiersFor HellishHound where
  getModifiersFor (HellishHound a) = do
    Scenario.pastObjectiveModifier a TheInvestigatorsRepelledTheHoundOfUnmaking
    Scenario.pastObjectiveModifier a TheInvestigatorsPutDownTheHoundOfUnmaking
    Scenario.pastObjectiveModifier a TheInvestigatorsFellToTheMyriad

instance HasAbilities HellishHound where
  getAbilities (HellishHound a) =
    [ Scenario.pastObjective a 1 TheInvestigatorsRepelledTheHoundOfUnmaking
    , Scenario.pastObjective a 2 TheInvestigatorsPutDownTheHoundOfUnmaking
    , Scenario.pastObjective a 3 TheInvestigatorsFellToTheMyriad
    ]

instance RunMessage HellishHound where
  runMessage msg a@(HellishHound attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 3 -> do
      advanceToActThreeD attrs
      pure a
    UseThisAbility _ (isSource attrs -> True) abilityIndex | abilityIndex `elem` [1, 2] -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide D attrs -> True) _ _ -> do
      repelled <- isAtOrPastFor TheInvestigatorsRepelledTheHoundOfUnmaking
      putDown <- isAtOrPastFor TheInvestigatorsPutDownTheHoundOfUnmaking

      if repelled
        then do
          lead <- getLead
          rearCorridors <- selectOne $ locationIs Locations.rearCorridors
          lid <- maybe (getJustLocation lead) pure rearCorridors
          hound <- createSetAsideEnemy Enemies.houndOfUnmaking lid
          damage <- getRecordCount DamageOnTheHoundOfUnmaking
          when (damage > 0) $ placeTokens attrs hound #damage damage
          advanceToAct attrs Cards.theFirstCircle C
        else
          if putDown
            then advanceToAct attrs Cards.theFirstCircle C
            else advanceToActThreeD attrs
      pure a
    _ -> HellishHound <$> liftRunMessage msg attrs

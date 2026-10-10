module Arkham.Homebrew.AgesUnwound.Locations.Classroom_228 (classroom_228) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Helpers.SkillTest (getSkillTestInvestigator, isInvestigating)
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.AgesUnwound.Helpers (campaignI18n, getStandardActions)
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Trait (Trait (Criminal))

newtype Classroom_228 = Classroom_228 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | /Guarded Secrets/. One of three distinct Classrooms, all with symbol T.
classroom_228 :: LocationCard Classroom_228
classroom_228 = setLabel "classroom2" $ location Classroom_228 Cards.classroom_228 7 (PerPlayer 1)

{- | "While you are investigating Classroom, it gets -2 shroud for each standard
action you have remaining."
-}
instance HasModifiersFor Classroom_228 where
  getModifiersFor (Classroom_228 a) = whenRevealed a $ maybeModifySelf a do
    iid <- MaybeT getSkillTestInvestigator
    liftGuardM $ isInvestigating iid a
    n <- lift $ getStandardActions iid
    guard (n > 0)
    pure [ShroudModifier (negate (2 * n))]

{- | "Haunted - Search the encounter deck and discard pile for a [[Criminal]]
enemy and draw it."
-}
instance HasAbilities Classroom_228 where
  getAbilities (Classroom_228 a) =
    extendRevealed1 a $ campaignI18n $ hauntedI "classroomGuardedSecrets.haunted" a 1

instance RunMessage Classroom_228 where
  runMessage msg l@(Classroom_228 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      findAndDrawEncounterCard iid (#enemy <> CardWithTrait Criminal)
      pure l
    _ -> Classroom_228 <$> liftRunMessage msg attrs

module Arkham.Homebrew.DarkMatter.Treacheries.MiGoExperiments (miGoExperiments) where

import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Homebrew.DarkMatter.CardDefs.Treacheries qualified as Cards
import Arkham.Message (pattern BeginSkillTest)
import Arkham.SkillTest.Base
import Arkham.SkillTest.Type
import Arkham.Treachery.Import.Lifted

newtype MiGoExperiments = MiGoExperiments TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

miGoExperiments :: TreacheryCard MiGoExperiments
miGoExperiments = treachery MiGoExperiments Cards.miGoExperiments

{- | "Revelation - Test [willpower] + [willpower] (3). Reveal and resolve an
additional chaos token for this skill test. For each point you fail by, take 1
horror."
-}
instance RunMessage MiGoExperiments where
  runMessage msg t@(MiGoExperiments attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      let skills = [#willpower, #willpower]
      push
        $ BeginSkillTest
        $ buildSkillTest
          sid
          iid
          attrs
          iid
          (AndSkillTest skills)
          (AndSkillBaseValue skills)
          (SkillTestDifficulty $ Fixed 3)
      skillTestModifier sid attrs sid RevealAnotherChaosToken
      pure t
    FailedThisSkillTestBy iid (isSource attrs -> True) n -> do
      assignHorror iid attrs n
      pure t
    _ -> MiGoExperiments <$> liftRunMessage msg attrs

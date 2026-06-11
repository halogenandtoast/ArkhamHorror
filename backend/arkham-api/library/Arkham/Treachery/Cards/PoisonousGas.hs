module Arkham.Treachery.Cards.PoisonousGas (poisonousGas) where

import Arkham.Helpers.Location (getLocationOf)
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.Scenarios.TheLabyrinthsOfLunacy.Helpers
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype PoisonousGas = PoisonousGas TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

poisonousGas :: TreacheryCard PoisonousGas
poisonousGas = treachery PoisonousGas Cards.poisonousGas

-- When this skill test begins, an investigator at your location may place 1
-- of his or her clues on your location to reduce the difficulty of this test
-- by 2.
instance RunMessage PoisonousGas where
  runMessage msg t@(PoisonousGas attrs) = runQueueT $ scenarioI18n $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      getLocationOf iid >>= \case
        Nothing -> revelationSkillTest sid iid attrs #agility (Fixed 4)
        Just lid -> do
          helpers <- select $ InvestigatorAt (LocationWithId lid) <> InvestigatorWithClues (atLeast 1)
          chooseOrRunOneM iid $ scope "poisonousGas" do
            unscoped $ labeled' "skip" nothing
            targets helpers \helper -> do
              moveTokens attrs helper lid #clue 1
              skillTestModifier sid attrs sid (Difficulty (-2))
          revelationSkillTest sid iid attrs #agility (Fixed 4)
      pure t
    FailedThisSkillTestBy iid (isSource attrs -> True) n -> do
      loseActions iid attrs (if n >= 2 then 2 else 1)
      pure t
    _ -> PoisonousGas <$> liftRunMessage msg attrs

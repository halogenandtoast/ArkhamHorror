module Arkham.Location.Cards.ChamberOfSorrows (chamberOfSorrows) where

import Arkham.Ability
import Arkham.Helpers.Location (getLocationOf)
import Arkham.I18n
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.TheLabyrinthsOfLunacy.Helpers

newtype ChamberOfSorrows = ChamberOfSorrows LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

chamberOfSorrows :: LocationCard ChamberOfSorrows
chamberOfSorrows = location ChamberOfSorrows Cards.chamberOfSorrows 2 (PerPlayer 1)

eitherChamber :: LocationMatcher
eitherChamber = mapOneOf locationIs [Cards.chamberOfSorrows, Cards.chamberOfRain]

-- Any investigator in either Chamber of Sorrows or Chamber of Rain may
-- perform this action.
instance HasAbilities ChamberOfSorrows where
  getAbilities (ChamberOfSorrows a) =
    extendRevealed1 a
      $ skillTestAbility
      $ restricted a 1 (youExist $ InvestigatorAt eitherChamber) actionAbility

instance RunMessage ChamberOfSorrows where
  runMessage msg l@(ChamberOfSorrows attrs) = runQueueT $ scenarioI18n $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #willpower (Fixed 2)
      pure l
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      onlyInvestigator <- selectNone $ NotInvestigator (InvestigatorWithId iid)
      investigators <-
        select $ not_ (InvestigatorWithId iid) <> at_ eitherChamber <> InvestigatorWithClues (atLeast 1)
      locations <-
        if onlyInvestigator
          then
            getLocationOf iid >>= maybe (pure []) (select . (<> LocationWithClues (atLeast 1)) . LocationWithId)
          else pure []
      chooseOneM iid $ scope "chamberOfSorrows" do
        questionLabeled' "takeClue"
        when (notNull investigators || notNull locations) do
          unscoped $ labeled' "skip" nothing
        targets investigators \iid' -> moveTokens (attrs.ability 1) iid' iid #clue 1
        targets locations \lid -> moveTokens (attrs.ability 1) lid iid #clue 1
      pure l
    _ -> ChamberOfSorrows <$> liftRunMessage msg attrs

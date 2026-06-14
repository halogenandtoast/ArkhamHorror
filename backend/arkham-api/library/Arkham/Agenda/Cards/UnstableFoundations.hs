module Arkham.Agenda.Cards.UnstableFoundations (unstableFoundations) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import {-# SOURCE #-} Arkham.GameEnv (getPhase)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelectMapM)
import Arkham.Location.Types (Field (LocationPosition))
import Arkham.Matcher
import Arkham.Phase
import Arkham.Projection
import Arkham.Trait (Trait (Lift))

newtype UnstableFoundations = UnstableFoundations AgendaAttrs
  deriving anyclass (IsAgenda, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unstableFoundations :: AgendaCard UnstableFoundations
unstableFoundations = agenda (2, A) UnstableFoundations Cards.unstableFoundations (Static 8)

instance HasModifiersFor UnstableFoundations where
  getModifiersFor (UnstableFoundations a) = do
    -- During the enemy phase, each location is considered connected to the
    -- location above and below it (the rows directly above/below its grid row).
    phase <- getPhase
    when (phase == EnemyPhase) do
      modifySelectMapM a Anywhere \loc -> do
        mpos <- field LocationPosition loc
        pure case mpos of
          Nothing -> []
          Just pos ->
            [ ConnectedToWhen
                (LocationWithId loc)
                (mapOneOf LocationInRow [pos.row - 1, pos.row + 1])
            ]

instance RunMessage UnstableFoundations where
  runMessage msg a@(UnstableFoundations attrs) = runQueueT $ case msg of
    -- [Forced] After the Great Lift moves. There is no "after a location moves"
    -- window, so we hook the engine's LocationMoved message directly (the slide
    -- pushes this). The Great Lift is the only location with the Lift trait.
    LocationMoved lid -> do
      isLift <- lid <=~> LocationWithTrait Lift
      when isLift do
        investigators <- select $ InvestigatorAt $ LocationWithId lid
        for_ investigators \iid -> do
          sid <- getRandom
          beginSkillTest sid iid attrs iid #agility (Fixed 2)
      pure a
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      assignDamage iid attrs 1
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      eachInvestigator \iid -> do
        sufferMentalTrauma iid 1
        investigatorDefeated attrs iid
      noResolution
      pure a
    _ -> UnstableFoundations <$> liftRunMessage msg attrs

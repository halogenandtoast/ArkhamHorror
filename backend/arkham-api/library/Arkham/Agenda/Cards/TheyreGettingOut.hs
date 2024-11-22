module Arkham.Agenda.Cards.TheyreGettingOut (TheyreGettingOut (..), theyreGettingOut) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Helpers.Act
import Arkham.Helpers.Query (getLead)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Resolution

newtype TheyreGettingOut = TheyreGettingOut AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theyreGettingOut :: AgendaCard TheyreGettingOut
theyreGettingOut = agenda (3, A) TheyreGettingOut Cards.theyreGettingOut (Static 10)

instance HasAbilities TheyreGettingOut where
  getAbilities (TheyreGettingOut x) =
    [ restricted
        x
        1
        (exists (UnengagedEnemy <> #ghoul <> not_ (at_ "Parlor")) <> exists (location_ "Parlor"))
        $ forced (PhaseEnds #when #enemy)
    , restricted x 2 (exists $ enemy_ $ at_ (oneOf ["Parlor", "Hallway"]) <> #ghoul)
        $ forced (RoundEnds #when)
    ]

instance RunMessage TheyreGettingOut where
  runMessage msg a@(TheyreGettingOut attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      actSequence <- getCurrentActStep
      push $ if actSequence `elem` [1, 2] then R3 else ScenarioResolution NoResolution
      pure a
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      enemiesToMove <- select $ UnengagedEnemy <> #ghoul <> not_ (at_ "Parlor")

      when (notNull enemiesToMove) do
        lead <- getLead
        chooseOneAtATimeM lead do
          targets enemiesToMove \enemy -> moveTowardsMatching (attrs.ability 1) enemy "Parlor"
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      ghoulCount <- selectCount $ enemy_ $ #ghoul <> at_ (oneOf ["Parlor", "Hallway"])
      placeDoomOnAgenda ghoulCount
      pure a
    _ -> TheyreGettingOut <$> liftRunMessage msg attrs

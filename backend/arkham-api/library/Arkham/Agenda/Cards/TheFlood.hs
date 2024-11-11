module Arkham.Agenda.Cards.TheFlood (TheFlood (..), theFlood) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Campaigns.TheInnsmouthConspiracy.Helpers
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Matcher

newtype TheFlood = TheFlood AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theFlood :: AgendaCard TheFlood
theFlood = agenda (3, A) TheFlood Cards.theFlood (Static 10)

instance HasAbilities TheFlood where
  getAbilities (TheFlood a) =
    [ restrictedAbility
        a
        1
        ( exists
            $ IncludeOmnipotent
            $ mapOneOf enemyIs [Enemies.dagonDeepInSlumberIntoTheMaelstrom, Enemies.hydraDeepInSlumber]
        )
        $ SilentForcedAbility AnyWindow
    , needsAir a 2
    ]

instance RunMessage TheFlood where
  runMessage msg a@(TheFlood attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      push R8
      pure a
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      selectForMaybeM (enemyIs Enemies.dagonDeepInSlumberIntoTheMaelstrom) $ flipOver iid
      selectForMaybeM (enemyIs Enemies.hydraDeepInSlumber) $ flipOver iid
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      struggleForAir attrs iid
      pure a
    _ -> TheFlood <$> liftRunMessage msg attrs

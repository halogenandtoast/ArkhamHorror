module Arkham.Agenda.Cards.TheDrownedCity.TheDrownedQuarter.TheSunkenRuins (theSunkenRuins) where

import Arkham.Ability
import Arkham.Agenda.CardDefs.TheDrownedCity.TheDrownedQuarter qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Campaigns.TheDrownedCity.Helpers
import Arkham.Card
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Matcher

newtype TheSunkenRuins = TheSunkenRuins AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theSunkenRuins :: AgendaCard TheSunkenRuins
theSunkenRuins = agenda (1, A) TheSunkenRuins Cards.theSunkenRuins (Static 7)

instance HasAbilities TheSunkenRuins where
  getAbilities (TheSunkenRuins a) =
    [ restricted a 1 (youExist $ at_ FullyFloodedLocation)
        $ forced
        $ TurnBegins #when You
    ]

instance RunMessage TheSunkenRuins where
  runMessage msg a@(TheSunkenRuins attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      struggleForAir attrs iid
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      -- The Seafloor Leviathan is the back of this agenda, so generate the enemy
      -- side directly rather than fetching (a fetch resolves to the in-play
      -- agenda side). It spawns at the Barrier Core via its own instruction.
      card <- genCard Enemies.seafloorLeviathan
      createEnemy_ card ()
      advanceAgendaDeck attrs
      pure a
    _ -> TheSunkenRuins <$> liftRunMessage msg attrs

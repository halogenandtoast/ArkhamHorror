module Arkham.Agenda.Cards.TheSunkenRuins (theSunkenRuins) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Campaigns.TheDrownedCity.Helpers
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
    _ -> TheSunkenRuins <$> liftRunMessage msg attrs

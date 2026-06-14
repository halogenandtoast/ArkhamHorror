module Arkham.Agenda.Cards.TheComingStorm (theComingStorm) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Helpers.Query (getLead)
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection

newtype TheComingStorm = TheComingStorm AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theComingStorm :: AgendaCard TheComingStorm
theComingStorm = agenda (1, A) TheComingStorm Cards.theComingStorm (Static 14)

instance HasAbilities TheComingStorm where
  getAbilities (TheComingStorm a) =
    [ mkAbility a 1 $ ActionAbility #resign Nothing (ActionCost 1)
    , mkAbility a 2 $ forced $ PlacedDoomCounter #when AnySource (targetIs a)
    ]

instance RunMessage TheComingStorm where
  runMessage msg a@(TheComingStorm attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      resign iid
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      lead <- getLead
      field InvestigatorLocation lead >>= traverse_ (push . IncreaseFloodLevel)
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      eachInvestigator resign
      pure a
    _ -> TheComingStorm <$> liftRunMessage msg attrs

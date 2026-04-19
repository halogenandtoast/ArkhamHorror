module Arkham.Agenda.Cards.DarkClouds (darkClouds) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Campaigns.TheFeastOfHemlockVale.Key
import Arkham.Card
import Arkham.Helpers.Log (getHasRecord)
import Arkham.Helpers.Window (cardDrawn)
import Arkham.Matcher

newtype DarkClouds = DarkClouds AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

darkClouds :: AgendaCard DarkClouds
darkClouds = agenda (1, A) DarkClouds Cards.darkClouds (Static 6)

instance HasAbilities DarkClouds where
  getAbilities (DarkClouds attrs) =
    [mkAbility attrs 1 $ forced $ DrawCard #when You (basic WeaknessCard) AnyDeck]

instance RunMessage DarkClouds where
  runMessage msg a@(DarkClouds attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      bertieWasRescued <- getHasRecord BertieWasRescued
      push $ if bertieWasRescued then R1 else R2
      pure a
    UseCardAbility iid (isSource attrs -> True) 1 (cardDrawn -> weakness) _ -> do
      cancelCardEffects attrs weakness
      insertAfterMatching [DiscardCard iid (toSource attrs) (toCardId weakness)] \case
        Do (InvestigatorDrewPlayerCardFrom iid' c _) | iid == iid' && c.id == weakness.id -> True
        _ -> False
      pure a
    _ -> DarkClouds <$> liftRunMessage msg attrs

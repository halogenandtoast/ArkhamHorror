module Arkham.Agenda.Cards.SilenceSpeaks (silenceSpeaks) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Campaigns.TheScarletKeys.Helpers
import Arkham.Capability
import Arkham.Card
import Arkham.Helpers.GameValue
import Arkham.Helpers.Query (getLead)
import Arkham.Investigator.Projection ()
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.DancingMad.Helpers

newtype SilenceSpeaks = SilenceSpeaks AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

silenceSpeaks :: AgendaCard SilenceSpeaks
silenceSpeaks = agenda (1, A) SilenceSpeaks Cards.silenceSpeaks (Static 6)

instance HasAbilities SilenceSpeaks where
  getAbilities (SilenceSpeaks a) =
    [restricted a 1 (ExtendedCardCount (AtLeast $ PerPlayer 5) HollowedCard) $ forced AnyWindow]

instance RunMessage SilenceSpeaks where
  runMessage msg a@(SilenceSpeaks attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      n <- perPlayer 3
      cards <- select HollowedCard
      focusCards cards $ chooseNM iid n $ targets cards removeHollow
      placeDoomOnAgendaAndCheckAdvance 1
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      lead <- getLead
      investigators <- select $ investigator_ can.manipulate.deck
      leadChooseOneM $ scenarioI18n do
        labeled' "silenceSpeaks.concealed" $ findAndDrawEncounterCard lead CardWithConcealed
        labeledValidate' (notNull investigators) "silenceSpeaks.hollowed" do
          chooseOneAtATimeM lead $ targets investigators (`forInvestigator` msg)
      advanceAgendaDeck attrs
      pure a
    ForInvestigator iid (AdvanceAgenda (isSide B attrs -> True)) -> do
      iid.topOfDeck >>= traverse_ \c -> do
        revealCard c
        if cardMatch c NonWeakness
          then hollow iid (toCard c)
          else drawCard iid c
      pure a
    _ -> SilenceSpeaks <$> liftRunMessage msg attrs

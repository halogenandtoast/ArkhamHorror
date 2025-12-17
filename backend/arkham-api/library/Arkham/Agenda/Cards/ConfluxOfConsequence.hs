module Arkham.Agenda.Cards.ConfluxOfConsequence (confluxOfConsequence) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Acts
import Arkham.Act.Types (Field (..))
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Campaigns.TheScarletKeys.Helpers
import Arkham.Campaigns.TheScarletKeys.Key.Matcher
import Arkham.Helpers.Act
import Arkham.Helpers.GameValue
import Arkham.Helpers.Phases (runEnemyPhase, withPhase)
import Arkham.Helpers.Query (getLead)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection

newtype ConfluxOfConsequence = ConfluxOfConsequence AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

confluxOfConsequence :: AgendaCard ConfluxOfConsequence
confluxOfConsequence = agenda (1, A) ConfluxOfConsequence Cards.confluxOfConsequence (Static 3)

instance HasAbilities ConfluxOfConsequence where
  getAbilities (ConfluxOfConsequence a) =
    [ restricted a 1 (ActExists $ ActOneOf $ map ActWithStep [2, 3, 4])
        $ forced
        $ AgendaAdvances #when (AgendaWithId a.id)
    ]

instance RunMessage ConfluxOfConsequence where
  runMessage msg a@(ConfluxOfConsequence attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      pure a
    AdvanceAgenda (isSide A attrs -> True) -> do
      n <- getCurrentActStep
      if n == 1
        then ConfluxOfConsequence <$> liftRunMessage msg attrs
        else do
          advanceAgendaDeck attrs
          pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      card <- field ActCard =<< getCurrentAct
      lead <- getLead
      if
        | card.cardCode == Acts.secretsAndLiesV1.cardCode -> do
            skeys <- select $ ScarletKeyWithEnemyBearer AnyEnemy
            chooseOneAtATimeM lead $ targets skeys shift
            advanceAgendaDeck attrs
            withPhase #enemy $ runEnemyPhase Noop
        | card.cardCode == Acts.secretsAndLiesV2.cardCode -> do
            advanceAgendaDeck attrs
            placeDoomOnAgenda 4
            advanceCurrentAct attrs
        | otherwise -> do
            n <- perPlayer 1
            skeys <- select StableScarletKey
            chooseNM lead n $ targets skeys $ flipOverBy lead attrs
            advanceAgendaDeck attrs
            withPhase #enemy $ runEnemyPhase Noop
      pure a
    _ -> ConfluxOfConsequence <$> liftRunMessage msg attrs

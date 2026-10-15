module Arkham.Homebrew.ThePhantomOfTheOpera.Agendas.SinisterForces (sinisterForces) where

import Arkham.Agenda.Import.Lifted
import Arkham.Campaigns.TheScarletKeys.Concealed.Helpers
import Arkham.Campaigns.TheScarletKeys.Concealed.Matcher
import Arkham.Helpers.Query (getLead)
import Arkham.Homebrew.ThePhantomOfTheOpera.CardDefs.Agendas qualified as Cards
import Arkham.Homebrew.ThePhantomOfTheOpera.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.ThePhantomOfTheOpera.Import
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Placement

newtype SinisterForces = SinisterForces AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sinisterForces :: AgendaCard SinisterForces
sinisterForces = agenda (1, A) SinisterForces Cards.sinisterForces (Static 4)

instance RunMessage SinisterForces where
  runMessage msg a@(SinisterForces attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      concealed <- shuffle =<< selectMap (.id) ConcealedCardAny
      locations <- select Anywhere
      when (notNull locations) $ distributeEvenlyBetween concealed locations
      doStep 1 msg
      pure a
    DoStep 1 msg'@(AdvanceAgenda (isSide B attrs -> True)) -> do
      lead <- getLead
      concealed <- select ConcealedCardAny
      chooseOneM lead $ scenarioI18n $ scope "sinisterForces" do
        questionLabeled "forceYourHand"
        targets concealed \c -> do
          revealConcealed lead attrs c.id
          forTarget_ c (DoStep 2 msg')
      doStep 3 msg'
      pure a
    ForTarget (ConcealedCardTarget cid) (DoStep 2 (AdvanceAgenda (isSide B attrs -> True))) -> do
      selectOne (ConcealedCardWithId cid) >>= traverse_ \c ->
        if c.isDecoy
          then selectForMaybeM (enemyIs Enemies.thePhantom) \phantom ->
            eachInvestigator $ initiateEnemyAttack phantom attrs
          else case c.placement of
            AtLocation lid -> shuffleConcealedAt lid
            _ -> pure ()
      pure a
    DoStep 3 (AdvanceAgenda (isSide B attrs -> True)) -> do
      attrs' <- liftRunMessage (RevertAgenda attrs.id) attrs
      pure $ SinisterForces $ attrs' & doomL .~ 0
    _ -> SinisterForces <$> liftRunMessage msg attrs

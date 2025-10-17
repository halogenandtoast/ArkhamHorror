module Arkham.Agenda.Cards.WhereIsShe (whereIsShe) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Query (getLead, getPlayerCount)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype WhereIsShe = WhereIsShe AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

whereIsShe :: AgendaCard WhereIsShe
whereIsShe = agenda (1, A) WhereIsShe Cards.whereIsShe (Static 7)

instance HasAbilities WhereIsShe where
  getAbilities (WhereIsShe a) =
    [ mkAbility a 1 $ forced $ GameBegins #when
    , mkAbility a 2 $ forced $ CampaignEvent #after (Just You) "exposed[decoy]"
    ]

instance RunMessage WhereIsShe where
  runMessage msg a@(WhereIsShe attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeDoomOnAgenda =<< getPlayerCount
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      laChicaRoja <- selectJust $ enemyIs Enemies.laChicaRojaTheGirlInTheCarmineCoat
      initiateEnemyAttack laChicaRoja (attrs.ability 2) iid
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      advanceAgendaDeck attrs
      pure a
    ResetAgendaDeckToStage 1 -> do
      lead <- getLead
      chooseOneM lead $ abilityLabeled lead (noLimit $ mkAbility attrs 1 $ forced AnyWindow) nothing
      pure $ WhereIsShe $ attrs & doomL .~ 0
    _ -> WhereIsShe <$> liftRunMessage msg attrs

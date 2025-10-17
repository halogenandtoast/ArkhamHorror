module Arkham.Agenda.Cards.WhereIsShe (whereIsShe) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Campaigns.TheScarletKeys.Concealed.Helpers
import Arkham.Campaigns.TheScarletKeys.Concealed.Kind
import Arkham.Campaigns.TheScarletKeys.Concealed.Types (Field (..))
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Types (Field (..))
import Arkham.Helpers.Location
import Arkham.Helpers.Query (getLead, getPlayerCount)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Placement
import Arkham.Projection
import Arkham.Token qualified as Token

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
    AdvanceAgenda (isSide A attrs -> True) -> do
      turnOverAllConcealed attrs
      WhereIsShe <$> liftRunMessage msg attrs
    AdvanceAgenda (isSide B attrs -> True) -> do
      lead <- getLead
      -- step 2 will show the card without UI interaction
      selectEach IsDecoy removeFromGame
      mLaChicaRojaMiniCard <- selectOne $ ConcealedCardIs LaChicaRoja
      laChicaRoja <- selectJust $ enemyIs Enemies.laChicaRojaTheGirlInTheCarmineCoat
      targetCount <- fieldMap EnemyTokens (Token.countTokens Token.Target) laChicaRoja
      mloc <- case mLaChicaRojaMiniCard of
        Nothing -> getLocationOf laChicaRoja
        Just miniCard -> fieldMap ConcealedCardPlacement (preview _AtLocation) miniCard.id

      when (isNothing mloc) $ error "invalid placement for La Chica Roja's mini card or La Chica Roja"
      for_ mloc \loc -> do
        hasTarget <- matches loc (LocationWithToken Token.Target)
        if hasTarget
          then moveTokens attrs loc laChicaRoja Token.Target 1
          else do
            locations <- select $ NearestLocationToLocation loc (LocationWithToken Token.Target)
            leadChooseOneM $ targets locations \targetLoc -> do
              moveTokens attrs targetLoc laChicaRoja Token.Target 1
      if targetCount + 1 >= 3
        then push R3
        else do
          for_ mLaChicaRojaMiniCard removeFromGame
          resolveConcealed lead laChicaRoja
          push $ ResetActDeckToStage 1
          push $ ResetAgendaDeckToStage 1
          selectEach Anywhere (placeCluesUpToClueValue attrs)
      pure a
    ResetAgendaDeckToStage 1 -> do
      lead <- getLead
      chooseOneM lead $ abilityLabeled lead (noLimit $ mkAbility attrs 1 $ forced AnyWindow) nothing
      attrs' <- liftRunMessage (RevertAgenda attrs.id) attrs
      pure $ WhereIsShe $ attrs' & doomL .~ 0
    _ -> WhereIsShe <$> liftRunMessage msg attrs

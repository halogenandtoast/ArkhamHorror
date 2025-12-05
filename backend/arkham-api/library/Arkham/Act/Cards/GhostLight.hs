module Arkham.Act.Cards.GhostLight (ghostLight) where

import Arkham.Ability hiding (Haunted)
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Agenda.Sequence qualified as Agenda
import Arkham.Campaigns.TheScarletKeys.Concealed.Kind
import Arkham.Campaigns.TheScarletKeys.Concealed.Types
import Arkham.Card
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Types (Field (EnemyCardsUnderneath))
import Arkham.Helpers.Location (getLocationOf)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Matcher
import Arkham.Message.Lifted.Move
import Arkham.Placement
import Arkham.Projection
import Arkham.Trait (Trait (Geist, Haunted))

newtype GhostLight = GhostLight ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ghostLight :: ActCard GhostLight
ghostLight = act (2, A) GhostLight Cards.ghostLight Nothing

instance HasModifiersFor GhostLight where
  getModifiersFor (GhostLight a) = do
    modifySelect
      a
      (EnemyWithTrait Geist <> NonWeaknessEnemy <> EnemyAt (LocationWithTrait Haunted))
      [CannotMove, CannotBeMoved]

instance HasAbilities GhostLight where
  getAbilities = actAbilities1 \a ->
    restricted a 1 (not_ $ exists $ InPlayEnemy $ EnemyWithTrait Geist <> NonWeaknessEnemy)
      $ forced
      $ RoundEnds #when

instance RunMessage GhostLight where
  runMessage msg a@(GhostLight attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      tzuSanNiang <- selectJust $ enemyIs Enemies.tzuSanNiangTheLadyWithTheRedParasol
      under <- field EnemyCardsUnderneath tzuSanNiang
      lead <- getLead
      selectEach ConcealedCardAny removeFromGame
      if any (`cardMatch` card_ #enemy) under
        then do
          healAllDamage attrs tzuSanNiang
          flipOver lead tzuSanNiang
          enemyEngageInvestigator tzuSanNiang lead
        else do
          mTzuSanNiangMiniCard <- selectOne $ ConcealedCardIs TzuSanNiang
          mloc <- case mTzuSanNiangMiniCard of
            Nothing -> getLocationOf tzuSanNiang
            Just miniCard -> fieldMap ConcealedCardPlacement (preview _AtLocation) miniCard.id
          for_ mloc $ enemyMoveTo attrs tzuSanNiang
          flipOver lead tzuSanNiang
      do_ $ AdvanceToAgenda 1 Agendas.fearTheReaper Agenda.A (toSource attrs)
      advanceActDeck attrs
      pure a
    _ -> GhostLight <$> liftRunMessage msg attrs

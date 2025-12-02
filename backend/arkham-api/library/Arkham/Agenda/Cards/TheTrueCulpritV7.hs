module Arkham.Agenda.Cards.TheTrueCulpritV7 (theTrueCulpritV7) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Asset.Cards qualified as Cards
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Name
import Arkham.Trait (Trait (Cultist, Guest, Innocent))

newtype TheTrueCulpritV7 = TheTrueCulpritV7 AgendaAttrs
  deriving anyclass IsAgenda
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theTrueCulpritV7 :: AgendaCard TheTrueCulpritV7
theTrueCulpritV7 = agenda (3, A) TheTrueCulpritV7 Cards.theTrueCulpritV7 (Static 8)

instance HasModifiersFor TheTrueCulpritV7 where
  getModifiersFor (TheTrueCulpritV7 attrs) = do
    modifySelect attrs (EnemyWithTrait Guest) [LoseVictory, RemoveTrait Innocent, AddTrait Cultist]

instance HasAbilities TheTrueCulpritV7 where
  getAbilities (TheTrueCulpritV7 attrs) =
    guard (onSide A attrs)
      *> ( [ notSkillTestAbility
               $ controlled
                 (proxied (assetIs asset) attrs)
                 1
                 (exists (enemyIs Cards.dimensionalShambler <> EnemyAt YourLocation <> CanEvadeEnemy (toSource attrs)))
                 (evadeAction $ AssetClueCost (toTitle asset) (assetIs asset) $ Static 1)
           | asset <-
               [ Cards.alienDevice
               , Cards.managersKey
               , Cards.sinisterSolution
               , Cards.timeWornLocket
               , Cards.tomeOfRituals
               ]
           ]
             <> [restricted attrs 2 (notExists (InPlayEnemy #cultist)) $ Objective $ forced AnyWindow]
         )

instance RunMessage TheTrueCulpritV7 where
  runMessage msg a@(TheTrueCulpritV7 attrs) = runQueueT $ case msg of
    UseThisAbility iid (ProxySource _ (isSource attrs -> True)) 1 -> do
      dimensionalShambler <- selectJust $ enemyIs Cards.dimensionalShambler
      automaticallyEvadeEnemy iid dimensionalShambler
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advanceAgenda attrs
      pure a
    AdvanceAgendaBy (isSide B attrs -> True) means -> do
      push $ if means == AgendaAdvancedWithDoom then R2 else R1
      pure a
    _ -> TheTrueCulpritV7 <$> liftRunMessage msg attrs

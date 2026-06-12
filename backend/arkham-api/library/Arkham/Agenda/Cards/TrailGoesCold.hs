module Arkham.Agenda.Cards.TrailGoesCold (trailGoesCold) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted hiding (EnemyEvaded)
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Helpers.Window (getEnemy)
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher
import Arkham.Scenarios.RedTideRising.Helpers
import Arkham.Trait (Trait (Hideout, Suspect))
import Arkham.Window qualified as Window

newtype TrailGoesCold = TrailGoesCold AgendaAttrs
  deriving anyclass IsAgenda
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

trailGoesCold :: AgendaCard TrailGoesCold
trailGoesCold = agenda (2, A) TrailGoesCold Cards.trailGoesCold (Static 9)

instance HasModifiersFor TrailGoesCold where
  getModifiersFor (TrailGoesCold a) = do
    modifySelect a (enemyIs Enemies.angryMob) [CannotBeDefeated]
    modifySelect a (EnemyWithTrait Suspect) [RemoveKeyword Keyword.Aloof]

instance HasAbilities TrailGoesCold where
  getAbilities (TrailGoesCold a) =
    guard (onSide A a)
      *> [ mkAbility a 1 $ freeReaction $ EnemyEvaded #after (You <> wendyAdams) (enemyIs Enemies.angryMob)
         , mkAbility a 2 $ forced $ EnemySpawns #after Anywhere (EnemyWithTrait Suspect)
         , mkAbility a 3 $ forced $ LocationEntersPlay #after (LocationWithTrait Hideout)
         ]

instance RunMessage TrailGoesCold where
  runMessage msg a@(TrailGoesCold attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      selectOne (enemyIs Enemies.angryMob) >>= traverse_ \mob ->
        withLocationOf mob \lid -> discoverAt NotInvestigate iid (attrs.ability 1) 1 lid
      pure a
    UseCardAbility _ (isSource attrs -> True) 2 (getEnemy -> enemy) _ -> do
      placeClues (attrs.ability 2) enemy =<< perPlayer 1
      pure a
    UseCardAbility _ (isSource attrs -> True) 3 ws _ -> do
      let mlid = listToMaybe [lid | (Window.windowType -> Window.LocationEntersPlay lid) <- ws]
      for_ mlid \lid -> placeClues (attrs.ability 3) lid =<< perPlayer 1
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      push R2
      pure a
    _ -> TrailGoesCold <$> liftRunMessage msg attrs

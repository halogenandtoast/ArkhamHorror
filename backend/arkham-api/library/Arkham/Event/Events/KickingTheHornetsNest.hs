module Arkham.Event.Events.KickingTheHornetsNest (kickingTheHornetsNest) where

import Arkham.Card
import Arkham.Enemy.Types (Field (EnemyHealth))
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), getTotalSearchTargets)
import Arkham.I18n
import Arkham.Matcher
import Arkham.Projection
import Arkham.Spawn
import Arkham.Strategy

newtype KickingTheHornetsNest = KickingTheHornetsNest EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

kickingTheHornetsNest :: EventCard KickingTheHornetsNest
kickingTheHornetsNest = event KickingTheHornetsNest Cards.kickingTheHornetsNest

instance RunMessage KickingTheHornetsNest where
  runMessage msg e@(KickingTheHornetsNest attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      search iid attrs EncounterDeckTarget [fromTopOfDeck 9] #any (defer attrs IsDraw)
      pure e
    SearchFound iid (isTarget attrs -> True) _ cards -> do
      let enemyCards = filterCards (IsEncounterCard <> #enemy) cards
      n <- getTotalSearchTargets iid enemyCards 1
      focusCards cards do
        when (null enemyCards) $ withI18n $ prompt_ iid "noEnemiesFound"
        chooseNM iid n do
          targets enemyCards \card -> do
            searchModifier attrs card (ForceSpawn (SpawnEngagedWith $ InvestigatorWithId iid))
            createEnemy_ card iid
            handleTarget iid attrs card.id
      pure e
    HandleTargetChoice iid (isSource attrs -> True) (CardIdTarget cid) -> do
      discoverAtYourLocation NotInvestigate iid attrs 1
      selectOne (EnemyWithCardId cid)
        >>= traverse_ (field EnemyHealth >=> traverse_ (gainResourcesIfCan iid attrs))
      pure e
    _ -> KickingTheHornetsNest <$> liftRunMessage msg attrs

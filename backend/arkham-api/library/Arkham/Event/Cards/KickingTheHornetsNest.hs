module Arkham.Event.Cards.KickingTheHornetsNest (kickingTheHornetsNest, KickingTheHornetsNest (..)) where

import Arkham.Card
import Arkham.Enemy.Types (Field (EnemyHealth))
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Modifiers qualified as Msg
import Arkham.Matcher
import Arkham.Projection
import Arkham.Spawn
import Arkham.Strategy
import Arkham.Zone

newtype KickingTheHornetsNest = KickingTheHornetsNest EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

kickingTheHornetsNest :: EventCard KickingTheHornetsNest
kickingTheHornetsNest = event KickingTheHornetsNest Cards.kickingTheHornetsNest

instance RunMessage KickingTheHornetsNest where
  runMessage msg e@(KickingTheHornetsNest attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      search iid attrs EncounterDeckTarget [(FromTopOfDeck 9, ShuffleBackIn)] #any (defer attrs IsDraw)
      pure e
    SearchFound iid (isTarget attrs -> True) _ cards -> do
      additionalTargets <- getAdditionalSearchTargets iid
      let enemyCards = filter (`cardMatch` EnemyType) $ onlyEncounterCards cards
      chooseN iid (min (length enemyCards) (1 + additionalTargets))
        $ [ targetLabel
            card
            [ Msg.searchModifier attrs card (ForceSpawn (SpawnEngagedWith $ InvestigatorWithId iid))
            , InvestigatorDrewEncounterCard iid card
            , HandleTargetChoice iid (toSource attrs) (CardIdTarget $ toCardId card)
            ]
          | card <- enemyCards
          ]
      pure e
    HandleTargetChoice iid (isSource attrs -> True) (CardIdTarget cid) -> do
      discoverAtYourLocation NotInvestigate iid attrs 1
      selectOne (EnemyWithCardId cid) >>= traverse_ \eid -> do
        field EnemyHealth eid >>= traverse_ \n -> gainResourcesIfCan iid attrs n
      pure e
    _ -> KickingTheHornetsNest <$> liftRunMessage msg attrs

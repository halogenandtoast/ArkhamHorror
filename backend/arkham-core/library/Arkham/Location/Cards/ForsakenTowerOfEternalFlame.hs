module Arkham.Location.Cards.ForsakenTowerOfEternalFlame (
  forsakenTowerOfEternalFlame,
  ForsakenTowerOfEternalFlame (..),
)
where

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Attack
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Treacheries

newtype ForsakenTowerOfEternalFlame = ForsakenTowerOfEternalFlame LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

forsakenTowerOfEternalFlame :: LocationCard ForsakenTowerOfEternalFlame
forsakenTowerOfEternalFlame =
  location
    ForsakenTowerOfEternalFlame
    Cards.forsakenTowerOfEternalFlame
    2
    (PerPlayer 1)

instance HasAbilities ForsakenTowerOfEternalFlame where
  getAbilities (ForsakenTowerOfEternalFlame attrs) =
    let
      whisperingChaos = case attrs.label of
        "northTower" -> Treacheries.whisperingChaosNorth
        "southTower" -> Treacheries.whisperingChaosSouth
        "eastTower" -> Treacheries.whisperingChaosEast
        "westTower" -> Treacheries.whisperingChaosWest
        _ -> error "Invalid Label"
      restriction =
        exists (TreacheryInHandOf You <> treacheryIs whisperingChaos)
          <> exists (EnemyInHandOf You <> EnemyWithTitle "Nyarlathotep")
     in
      extendRevealed attrs [restrictedAbility attrs 1 restriction fightAction_]

instance RunMessage ForsakenTowerOfEternalFlame where
  runMessage msg l@(ForsakenTowerOfEternalFlame attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      nyarlathoteps <- select $ EnemyInHandOf $ InvestigatorWithId iid
      chooseOne
        iid
        [ targetLabel
          nyarlathotep
          [HandleTargetChoice iid (attrs.ability 1) (toTarget nyarlathotep)]
        | nyarlathotep <- nyarlathoteps
        ]
      pure l
    HandleTargetChoice iid (isAbilitySource attrs 1 -> True) (EnemyTarget nyarlathotep) -> do
      push $ FightEnemy iid nyarlathotep (attrs.ability 1) (Just $ toTarget attrs) #combat False
      pure l
    Successful (Action.Fight, EnemyTarget eid) _iid _ (isTarget attrs -> True) _ -> do
      push $ AddToVictory (toTarget eid)
      pure l
    Failed (Action.Fight, EnemyTarget eid) iid _ (isTarget attrs -> True) _ -> do
      pushAll
        [ InitiateEnemyAttack $ enemyAttack eid (attrs.ability 1) iid
        , ShuffleBackIntoEncounterDeck (toTarget eid)
        ]
      pure l
    _ -> ForsakenTowerOfEternalFlame <$> lift (runMessage msg attrs)

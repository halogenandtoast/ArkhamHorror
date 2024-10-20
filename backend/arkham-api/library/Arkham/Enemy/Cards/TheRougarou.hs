module Arkham.Enemy.Cards.TheRougarou (TheRougarou (..), theRougarou) where

import Arkham.Ability
import Arkham.DamageEffect
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.GameValue (getPlayerCountValue)
import Arkham.Helpers.Query (getLead)
import Arkham.Matcher
import Arkham.Message qualified as Msg

newtype Meta = Meta {damagePerPhase :: Int}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype TheRougarou = TheRougarou (EnemyAttrs `With` Meta)
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theRougarou :: EnemyCard TheRougarou
theRougarou = enemy (TheRougarou . (`with` Meta 0)) Cards.theRougarou (3, PerPlayer 5, 3) (2, 2)

instance HasAbilities TheRougarou where
  getAbilities (TheRougarou (a `With` meta)) = do
    let actions' = getAbilities a
    let firstAbility =
          noLimit
            $ restricted a 1 (ValueIs (damagePerPhase meta) (AtLeast $ PerPlayer 1))
            $ forced
            $ EnemyDealtDamage #after AnyDamageEffect (be a) AnySource
    if any (`abilityIs` #engage) actions'
      then do
        let
          engageAction =
            restricted a 102 (OnSameLocation <> not_ (thisEnemy $ EnemyIsEngagedWith You))
              $ ActionAbility [#engage] (GroupClueCost (ByPlayerCount 1 1 2 2) Anywhere <> ActionCost 1)
        firstAbility : filter (not . (`abilityIs` #engage)) actions' <> [engageAction]
      else firstAbility : actions'

instance RunMessage TheRougarou where
  runMessage msg (TheRougarou (attrs `With` metadata)) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      damageThreshold <- getPlayerCountValue (PerPlayer 1)
      lead <- getLead
      select (FarthestLocationFromAll Anywhere) >>= \case
        [] -> error "can't happen"
        [x] -> push $ MoveUntil x (toTarget attrs)
        xs -> chooseOne lead [targetLabel x [MoveUntil x (toTarget attrs)] | x <- xs]
      TheRougarou
        . (`with` Meta (damagePerPhase metadata `mod` damageThreshold))
        <$> liftRunMessage msg attrs
    EndPhase -> TheRougarou . (`with` Meta 0) <$> liftRunMessage msg attrs
    Msg.EnemyDamage eid (damageAssignmentAmount -> n) | eid == attrs.id -> do
      TheRougarou . (`with` Meta (damagePerPhase metadata + n)) <$> liftRunMessage msg attrs
    _ -> TheRougarou . (`with` metadata) <$> liftRunMessage msg attrs

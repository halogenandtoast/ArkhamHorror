module Arkham.Types.Enemy.Cards.Narogath where


import Arkham.Types.Action
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Helpers
import Arkham.Types.Enemy.Runner
import Arkham.Types.Trait
import qualified Arkham.Types.Trait as Trait

newtype Narogath = Narogath EnemyAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

narogath :: EnemyId -> Narogath
narogath uuid =
  Narogath
    $ baseAttrs uuid "50026b"
    $ (healthDamageL .~ 1)
    . (sanityDamageL .~ 2)
    . (fightL .~ 3)
    . (healthL .~ Static 4)
    . (evadeL .~ 3)
    . (preyL .~ NearestToEnemyWithTrait Trait.Cultist)
    . (uniqueL .~ True)

instance (HasSet InvestigatorId env LocationId, HasSet ConnectedLocationId env LocationId) => HasModifiersFor env Narogath where
  getModifiersFor _ (InvestigatorTarget iid) (Narogath a@EnemyAttrs {..})
    | spawned a = do
      connectedLocationIds <- map unConnectedLocationId
        <$> getSetList enemyLocation
      iids <- concat <$> for (enemyLocation : connectedLocationIds) getSetList
      pure $ toModifiers
        a
        [ CannotTakeAction (EnemyAction Parley [Cultist])
        | not enemyExhausted && iid `elem` iids
        ]
  getModifiersFor _ _ _ = pure []

instance ActionRunner env => HasActions env Narogath where
  getActions i window (Narogath attrs) = getActions i window attrs

instance (EnemyRunner env) => RunMessage env Narogath where
  runMessage msg (Narogath attrs@EnemyAttrs {..}) = case msg of
    EnemySpawnEngagedWithPrey eid | eid == enemyId -> do
      playerCount <- unPlayerCount <$> getCount ()
      Narogath
        <$> runMessage msg (attrs & healthL %~ fmap (+ (3 * playerCount)))
    _ -> Narogath <$> runMessage msg attrs

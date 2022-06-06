module Arkham.Treachery.Cards.ToweringBeasts
  ( toweringBeasts
  , ToweringBeasts(..)
  ) where

import Arkham.Prelude

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Card.CardCode
import Arkham.Classes
import Arkham.Game.Helpers
import Arkham.Id
import Arkham.Matcher
import Arkham.Message
import Arkham.Modifier
import Arkham.Target
import Arkham.Treachery.Attrs
import Arkham.Treachery.Runner

newtype ToweringBeasts = ToweringBeasts TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

toweringBeasts :: TreacheryCard ToweringBeasts
toweringBeasts = treachery ToweringBeasts Cards.toweringBeasts

instance HasModifiersFor ToweringBeasts where
  getModifiersFor _ (EnemyTarget eid) (ToweringBeasts attrs)
    | treacheryOnEnemy eid attrs = pure
    $ toModifiers attrs [EnemyFight 1, HealthModifier 1]
  getModifiersFor _ _ _ = pure []

instance TreacheryRunner env => RunMessage ToweringBeasts where
  runMessage msg t@(ToweringBeasts attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      broodOfYogSothoth <- getSetList @EnemyId (CardCode "02255")
      case broodOfYogSothoth of
        [] -> pure t
        xs -> do
          locationId <- getId @LocationId iid
          broodWithLocationIds <- for xs $ \x -> (x, ) <$> selectJust (LocationWithEnemy $ EnemyWithId x)
          t <$ push
            (chooseOne
              iid
              [ TargetLabel
                  (EnemyTarget eid)
                  ([AttachTreachery (toId attrs) (EnemyTarget eid)]
                  <> [ InvestigatorAssignDamage iid source DamageAny 1 0
                     | lid == locationId
                     ]
                  )
              | (eid, lid) <- broodWithLocationIds
              ]
            )
    _ -> ToweringBeasts <$> runMessage msg attrs

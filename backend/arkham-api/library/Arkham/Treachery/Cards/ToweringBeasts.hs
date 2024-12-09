module Arkham.Treachery.Cards.ToweringBeasts (
  toweringBeasts,
  ToweringBeasts (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Game.Helpers
import Arkham.Helpers.Investigator
import Arkham.Matcher
import Arkham.Scenarios.UndimensionedAndUnseen.Helpers
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype ToweringBeasts = ToweringBeasts TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

toweringBeasts :: TreacheryCard ToweringBeasts
toweringBeasts = treachery ToweringBeasts Cards.toweringBeasts

instance HasModifiersFor ToweringBeasts where
  getModifiersFor (ToweringBeasts attrs) = case attrs.placement of
    AttachedToEnemy eid -> modified_ attrs eid [EnemyFight 1, HealthModifier 1]
    _ -> pure mempty

instance RunMessage ToweringBeasts where
  runMessage msg t@(ToweringBeasts attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      broodOfYogSothoth <- getBroodOfYogSothoth
      unless (null broodOfYogSothoth) $ do
        locationId <- getJustLocation iid
        broodWithLocationIds <- for broodOfYogSothoth
          $ \x -> (x,) <$> selectJust (LocationWithEnemy $ EnemyWithId x)
        player <- getPlayer iid
        push
          $ chooseOne
            player
            [ targetLabel eid $ [attachTreachery attrs eid] <> [assignDamage iid source 1 | lid == locationId]
            | (eid, lid) <- broodWithLocationIds
            ]
      pure t
    _ -> ToweringBeasts <$> runMessage msg attrs

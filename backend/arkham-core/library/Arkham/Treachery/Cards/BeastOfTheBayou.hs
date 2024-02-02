module Arkham.Treachery.Cards.BeastOfTheBayou where

import Arkham.Prelude

import Arkham.Attack
import Arkham.Classes
import Arkham.Matcher
import Arkham.Scenarios.CurseOfTheRougarou.Helpers
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype BeastOfTheBayou = BeastOfTheBayou TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

beastOfTheBayou :: TreacheryCard BeastOfTheBayou
beastOfTheBayou = treachery BeastOfTheBayou Cards.beastOfTheBayou

instance RunMessage BeastOfTheBayou where
  runMessage msg t@(BeastOfTheBayou attrs) = case msg of
    Revelation _iid source | isSource attrs source -> do
      mrougarou <- getTheRougarou
      t <$ case mrougarou of
        Nothing -> pushAll [PlaceDoomOnAgenda]
        Just eid -> do
          locationId <- selectJust $ locationWithEnemy eid
          connectedLocationIds <- selectList $ accessibleFrom locationId
          investigatorIds <-
            selectList
              $ InvestigatorAt
              $ LocationMatchAny
              $ map
                LocationWithId
                (locationId : connectedLocationIds)
          case investigatorIds of
            [] -> pushAll [PlaceDoomOnAgenda]
            xs -> pushAll $ map (EnemyAttack . enemyAttack eid attrs) xs
    FailedSkillTest iid _ (isSource attrs -> True) SkillTestInitiatorTarget {} _ n ->
      do
        push $ InvestigatorAssignDamage iid (toSource attrs) DamageAny n 0
        pure t
    _ -> BeastOfTheBayou <$> runMessage msg attrs

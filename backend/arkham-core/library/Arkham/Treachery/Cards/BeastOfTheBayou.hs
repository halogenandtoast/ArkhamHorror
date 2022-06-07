module Arkham.Treachery.Cards.BeastOfTheBayou where

import Arkham.Prelude

import Arkham.Attack
import Arkham.Classes
import Arkham.Matcher
import Arkham.Message
import Arkham.Scenarios.CurseOfTheRougarou.Helpers
import Arkham.Source
import Arkham.Target
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype BeastOfTheBayou = BeastOfTheBayou TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

beastOfTheBayou :: TreacheryCard BeastOfTheBayou
beastOfTheBayou = treachery BeastOfTheBayou Cards.beastOfTheBayou

instance RunMessage BeastOfTheBayou where
  runMessage msg t@(BeastOfTheBayou attrs@TreacheryAttrs {..}) = case msg of
    Revelation _iid source | isSource attrs source -> do
      mrougarou <- getTheRougarou
      t <$ case mrougarou of
        Nothing -> pushAll [PlaceDoomOnAgenda]
        Just eid -> do
          locationId <- selectJust $ LocationWithEnemy $ EnemyWithId eid
          connectedLocationIds <- selectList $ AccessibleFrom $ LocationWithId
            locationId
          investigatorIds <-
            selectList $ InvestigatorAt $ LocationMatchAny $ map
              LocationWithId
              (locationId : connectedLocationIds)
          case investigatorIds of
            [] -> pushAll [PlaceDoomOnAgenda]
            xs -> pushAll
              [ EnemyAttack iid' eid DamageAny RegularAttack | iid' <- xs ]
    FailedSkillTest iid _ (TreacherySource tid) SkillTestInitiatorTarget{} _ n
      | tid == treacheryId
      -> t
        <$ push
             (InvestigatorAssignDamage
               iid
               (TreacherySource treacheryId)
               DamageAny
               n
               0
             )
    _ -> BeastOfTheBayou <$> runMessage msg attrs

module Arkham.Treachery.Cards.BeastOfTheBayou where

import Arkham.Prelude

import Arkham.Attack
import Arkham.Scenarios.CurseOfTheRougarou.Helpers
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Classes
import Arkham.Id
import Arkham.Matcher
import Arkham.Message
import Arkham.Source
import Arkham.Target
import Arkham.Treachery.Attrs
import Arkham.Treachery.Runner

newtype BeastOfTheBayou = BeastOfTheBayou TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

beastOfTheBayou :: TreacheryCard BeastOfTheBayou
beastOfTheBayou = treachery BeastOfTheBayou Cards.beastOfTheBayou

instance TreacheryRunner env => RunMessage env BeastOfTheBayou where
  runMessage msg t@(BeastOfTheBayou attrs@TreacheryAttrs {..}) = case msg of
    Revelation _iid source | isSource attrs source -> do
      mrougarou <- getTheRougarou
      t <$ case mrougarou of
        Nothing -> pushAll [PlaceDoomOnAgenda]
        Just eid -> do
          locationId <- selectJust $ LocationWithEnemy $ EnemyWithId eid
          connectedLocationIds <- map unConnectedLocationId
            <$> getSetList locationId
          investigatorIds <- concat <$> traverse
            (getSetList @InvestigatorId)
            (locationId : connectedLocationIds)
          case investigatorIds of
            [] -> pushAll [PlaceDoomOnAgenda]
            xs -> pushAll [ EnemyAttack iid' eid DamageAny RegularAttack | iid' <- xs ]
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

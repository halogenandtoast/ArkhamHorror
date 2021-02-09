module Arkham.Types.Treachery.Cards.BeastOfTheBayou where


import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype BeastOfTheBayou = BeastOfTheBayou TreacheryAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

beastOfTheBayou :: TreacheryId -> a -> BeastOfTheBayou
beastOfTheBayou uuid _ = BeastOfTheBayou $ baseAttrs uuid "81035"

instance HasModifiersFor env BeastOfTheBayou where
  getModifiersFor = noModifiersFor

instance HasActions env BeastOfTheBayou where
  getActions i window (BeastOfTheBayou attrs) = getActions i window attrs

instance TreacheryRunner env => RunMessage env BeastOfTheBayou where
  runMessage msg t@(BeastOfTheBayou attrs@TreacheryAttrs {..}) = case msg of
    Revelation _iid source | isSource attrs source -> do
      mrougarou <- fmap unStoryEnemyId <$> getId (CardCode "81028")
      t <$ case mrougarou of
        Nothing ->
          unshiftMessages [PlaceDoomOnAgenda, Discard (toTarget attrs)]
        Just eid -> do
          locationId <- getId @LocationId eid
          connectedLocationIds <- map unConnectedLocationId
            <$> getSetList locationId
          investigatorIds <- concat <$> traverse
            (getSetList @InvestigatorId)
            (locationId : connectedLocationIds)
          case investigatorIds of
            [] -> unshiftMessages [PlaceDoomOnAgenda, Discard (toTarget attrs)]
            xs -> unshiftMessages
              ([ EnemyAttack iid' eid | iid' <- xs ]
              <> [Discard (toTarget attrs)]
              )
    FailedSkillTest iid _ (TreacherySource tid) SkillTestInitiatorTarget{} _ n
      | tid == treacheryId -> t <$ unshiftMessage
        (InvestigatorAssignDamage
          iid
          (TreacherySource treacheryId)
          DamageAny
          n
          0
        )
    _ -> BeastOfTheBayou <$> runMessage msg attrs

{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Treachery.Cards.BeastOfTheBayou where

import Arkham.Import

import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype BeastOfTheBayou = BeastOfTheBayou Attrs
  deriving newtype (Show, ToJSON, FromJSON)

beastOfTheBayou :: TreacheryId -> a -> BeastOfTheBayou
beastOfTheBayou uuid _ = BeastOfTheBayou $ baseAttrs uuid "81035"

instance HasModifiersFor env BeastOfTheBayou where
  getModifiersFor = noModifiersFor

instance HasActions env BeastOfTheBayou where
  getActions i window (BeastOfTheBayou attrs) = getActions i window attrs

instance TreacheryRunner env => RunMessage env BeastOfTheBayou where
  runMessage msg t@(BeastOfTheBayou attrs@Attrs {..}) = case msg of
    Revelation _iid source | isSource attrs source -> do
      mrougarou <- asks (fmap unStoryEnemyId <$> getId (CardCode "81028"))
      case mrougarou of
        Nothing ->
          unshiftMessages [PlaceDoomOnAgenda, Discard (toTarget attrs)]
        Just eid -> do
          locationId <- asks $ getId @LocationId eid
          connectedLocationIds <-
            asks $ map unConnectedLocationId . setToList . getSet locationId
          investigatorIds <- concat <$> traverse
            (asks . (setToList .) . getSet @InvestigatorId)
            (locationId : connectedLocationIds)
          case investigatorIds of
            [] -> unshiftMessages [PlaceDoomOnAgenda, Discard (toTarget attrs)]
            xs -> unshiftMessages
              ([ EnemyAttack iid' eid | iid' <- xs ]
              <> [Discard (toTarget attrs)]
              )
      BeastOfTheBayou <$> runMessage msg (attrs & resolved .~ True)
    FailedSkillTest iid _ (TreacherySource tid) SkillTestInitiatorTarget n
      | tid == treacheryId -> t <$ unshiftMessage
        (InvestigatorAssignDamage iid (TreacherySource treacheryId) n 0)
    _ -> BeastOfTheBayou <$> runMessage msg attrs

{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Treachery.Cards.SearchingForIzzie where

import Arkham.Import

import qualified Arkham.Types.Action as Action
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype SearchingForIzzie = SearchingForIzzie Attrs
  deriving newtype (Show, ToJSON, FromJSON)

searchingForIzzie :: TreacheryId -> Maybe InvestigatorId -> SearchingForIzzie
searchingForIzzie uuid iid = SearchingForIzzie $ weaknessAttrs uuid iid "02011"

instance HasModifiersFor env SearchingForIzzie where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env SearchingForIzzie where
  getActions iid NonFast (SearchingForIzzie attrs@Attrs {..}) = do
    investigatorLocationId <- getId @LocationId iid
    pure
      [ ActivateCardAbilityAction
          iid
          (mkAbility (TreacherySource treacheryId) 1 (ActionAbility 2 Nothing))
      | treacheryOnLocation investigatorLocationId attrs
      ]
  getActions _ _ _ = pure []

instance TreacheryRunner env => RunMessage env SearchingForIzzie where
  runMessage msg t@(SearchingForIzzie attrs@Attrs {..}) = case msg of
    Revelation iid source | isSource attrs source -> do
      farthestLocations <- map unFarthestLocationId <$> getSetList iid
      t <$ case farthestLocations of
        [lid] ->
          unshiftMessage (AttachTreachery treacheryId (LocationTarget lid))
        lids -> unshiftMessage
          (Ask
            iid
            (ChooseOne
              [ AttachTreachery treacheryId (LocationTarget lid) | lid <- lids ]
            )
          )
    UseCardAbility iid (TreacherySource tid) _ 1 | tid == treacheryId ->
      withTreacheryLocation attrs $ \attachedLocationId -> do
        shroud <- unShroud <$> getCount attachedLocationId
        t <$ unshiftMessage
          (BeginSkillTest
            iid
            (TreacherySource treacheryId)
            (InvestigatorTarget iid)
            (Just Action.Investigate)
            SkillIntellect
            shroud
          )
    PassedSkillTest _ _ source _ _ | isSource attrs source ->
      t <$ unshiftMessage (Discard $ toTarget attrs)
    EndOfGame ->
      let investigator = fromJustNote "missing investigator" treacheryOwner
      in t <$ unshiftMessage (SufferTrauma investigator 0 1)
    _ -> SearchingForIzzie <$> runMessage msg attrs

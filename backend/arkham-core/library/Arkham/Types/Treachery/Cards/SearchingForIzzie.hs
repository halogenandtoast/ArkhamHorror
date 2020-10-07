{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Treachery.Cards.SearchingForIzzie where

import Arkham.Json
import Arkham.Types.Ability
import qualified Arkham.Types.Action as Action
import Arkham.Types.Classes
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner
import Arkham.Types.TreacheryId
import Arkham.Types.Window
import ClassyPrelude
import qualified Data.HashSet as HashSet
import Lens.Micro
import Safe (fromJustNote)

newtype SearchingForIzzie = SearchingForIzzie Attrs
  deriving newtype (Show, ToJSON, FromJSON)

searchingForIzzie :: TreacheryId -> Maybe InvestigatorId -> SearchingForIzzie
searchingForIzzie uuid iid = SearchingForIzzie $ weaknessAttrs uuid iid "02011"

instance (ActionRunner env investigator) => HasActions env investigator SearchingForIzzie where
  getActions i NonFast (SearchingForIzzie Attrs {..}) = pure
    [ ActivateCardAbilityAction
        (getId () i)
        (mkAbility (TreacherySource treacheryId) 1 (ActionAbility 2 Nothing))
    | treacheryAttachedLocation == Just (locationOf i)
    ]
  getActions _ _ _ = pure []

instance (TreacheryRunner env) => RunMessage env SearchingForIzzie where
  runMessage msg t@(SearchingForIzzie attrs@Attrs {..}) = case msg of
    Revelation iid tid | tid == treacheryId -> do
      farthestLocations <- setToList . HashSet.map unFarthestLocationId <$> asks
        (getSet iid)
      case farthestLocations of
        [lid] -> unshiftMessage (AttachTreachery tid (LocationTarget lid))
        lids -> unshiftMessage
          (Ask
            iid
            (ChooseOne
              [ AttachTreachery tid (LocationTarget lid) | lid <- lids ]
            )
          )
      SearchingForIzzie <$> runMessage msg attrs
    AttachTreachery tid (LocationTarget lid) | tid == treacheryId ->
      pure $ SearchingForIzzie $ attrs & attachedLocation ?~ lid
    UseCardAbility iid _ (TreacherySource tid) _ 1 | tid == treacheryId -> do
      let
        attachedLocationId =
          fromJustNote "has to be set" treacheryAttachedLocation
      shroud <- unShroud <$> asks (getCount attachedLocationId)
      t <$ unshiftMessage
        (BeginSkillTest
          iid
          (TreacherySource treacheryId)
          (InvestigatorTarget iid)
          (Just Action.Investigate)
          SkillIntellect
          shroud
          [Discard (TreacheryTarget treacheryId)]
          mempty
          mempty
          mempty
        )
    EndOfGame ->
      let investigator = fromJustNote "missing investigator" treacheryOwner
      in t <$ unshiftMessage (SufferTrauma investigator 0 1)
    _ -> SearchingForIzzie <$> runMessage msg attrs

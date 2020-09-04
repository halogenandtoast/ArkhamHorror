{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Treachery.Cards.LockedDoor where

import Arkham.Json
import Arkham.Types.Card.CardCode
import Arkham.Types.Ability
import Arkham.Types.Window
import Arkham.Types.LocationId
import Arkham.Types.Classes
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Query
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner
import Arkham.Types.TreacheryId
import Arkham.Types.SkillType
import qualified Data.HashSet as HashSet
import ClassyPrelude
import Lens.Micro
import Safe (fromJustNote)

newtype LockedDoor = LockedDoor Attrs
  deriving newtype (Show, ToJSON, FromJSON)

lockedDoor :: TreacheryId -> a -> LockedDoor
lockedDoor uuid _ = LockedDoor $ baseAttrs uuid "01174"

instance (IsInvestigator investigator) => HasActions env investigator LockedDoor where
  getActions i NonFast (LockedDoor Attrs {..}) = pure
    [ ActivateCardAbilityAction
        (getId () i)
        (mkAbility (TreacherySource treacheryId) 1 (ActionAbility 1 Nothing))
    | Just (locationOf i) == treacheryAttachedLocation && hasActionsRemaining i
    ]
  getActions _ _ _ = pure []

instance (TreacheryRunner env) => RunMessage env LockedDoor where
  runMessage msg t@(LockedDoor attrs@Attrs {..}) = case msg of
    Revelation iid tid | tid == treacheryId -> do
      exemptLocations <- asks
        (getSet @LocationId (TreacheryCardCode $ CardCode "01174"))
      targetLocations <-
        HashSet.toList . (`difference` exemptLocations) <$> asks
          (getSet @LocationId ())
      locationsWithClueCounts <- for targetLocations
        $ \lid -> (lid, ) . unClueCount <$> asks (getCount lid)
      let
        sortedLocationsWithClueCounts =
          sortOn (Down . snd) locationsWithClueCounts
      case sortedLocationsWithClueCounts of
        [] -> pure t -- Revelation whiffed
        ((_, c) : _) ->
          let (matches, _) = span ((== c) . snd) sortedLocationsWithClueCounts
          in
            do
              case matches of
                [(x, _)] -> unshiftMessages [AttachTreacheryToLocation tid x]
                xs -> unshiftMessage
                  (Ask iid $ ChooseOne
                    [ AttachTreacheryToLocation tid x | (x, _) <- xs ]
                  )
              LockedDoor <$> runMessage msg attrs
    AttachTreacheryToLocation tid lid | tid == treacheryId -> do
      unshiftMessage
        (AddModifier
          (LocationTarget lid)
          (TreacherySource tid)
          CannotInvestigate
        )
      pure . LockedDoor $ attrs & attachedLocation ?~ lid
    UseCardAbility iid _ (TreacherySource tid) 1 | tid == treacheryId -> do
      t <$ unshiftMessage
        (Ask iid $ ChooseOne
          [ BeginSkillTest
            iid
            (TreacherySource treacheryId)
            Nothing
            SkillCombat
            4
            [Discard (TreacheryTarget treacheryId)]
            mempty
            mempty
            mempty
          , BeginSkillTest
            iid
            (TreacherySource treacheryId)
            Nothing
            SkillAgility
            4
            [Discard (TreacheryTarget treacheryId)]
            mempty
            mempty
            mempty
          ]
        )
    Discard (TreacheryTarget tid) | tid == treacheryId -> do
      unshiftMessages
        [ RemoveAllModifiersOnTargetFrom
            (LocationTarget $ fromJustNote
              "had to have been attached"
              treacheryAttachedLocation
            )
            (TreacherySource treacheryId)
        ]
      LockedDoor <$> runMessage msg attrs
    _ -> LockedDoor <$> runMessage msg attrs

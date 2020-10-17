{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Treachery.Cards.LockedDoor where

import Arkham.Import

import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Helpers
import Arkham.Types.Treachery.Runner

newtype LockedDoor = LockedDoor Attrs
  deriving newtype (Show, ToJSON, FromJSON)

lockedDoor :: TreacheryId -> a -> LockedDoor
lockedDoor uuid _ = LockedDoor $ baseAttrs uuid "01174"

instance ActionRunner env => HasActions env LockedDoor where
  getActions iid NonFast (LockedDoor Attrs {..}) = do
    investigatorLocationId <- asks $ getId @LocationId iid
    hasActionsRemaining <- getHasActionsRemaining
      iid
      Nothing
      (setToList treacheryTraits)
    pure
      [ ActivateCardAbilityAction
          iid
          (mkAbility (TreacherySource treacheryId) 1 (ActionAbility 1 Nothing))
      | Just investigatorLocationId
        == treacheryAttachedLocation
        && hasActionsRemaining
      ]
  getActions _ _ _ = pure []

instance (TreacheryRunner env) => RunMessage env LockedDoor where
  runMessage msg t@(LockedDoor attrs@Attrs {..}) = case msg of
    Revelation iid tid | tid == treacheryId -> do
      exemptLocations <- asks
        (getSet @LocationId (TreacheryCardCode $ CardCode "01174"))
      targetLocations <- setToList . (`difference` exemptLocations) <$> asks
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
                [(x, _)] ->
                  unshiftMessages [AttachTreachery tid (LocationTarget x)]
                xs -> unshiftMessage
                  (Ask iid $ ChooseOne
                    [ AttachTreachery tid (LocationTarget x) | (x, _) <- xs ]
                  )
              LockedDoor <$> runMessage msg attrs
    AttachTreachery tid (LocationTarget lid) | tid == treacheryId -> do
      unshiftMessage
        (AddModifiers
          (LocationTarget lid)
          (TreacherySource tid)
          [CannotInvestigate]
        )
      pure . LockedDoor $ attrs & attachedLocation ?~ lid
    UseCardAbility iid (TreacherySource tid) _ 1 | tid == treacheryId -> do
      t <$ unshiftMessage
        (Ask iid $ ChooseOne
          [ BeginSkillTest
            iid
            (TreacherySource treacheryId)
            (TreacheryTarget treacheryId)
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
            (TreacheryTarget treacheryId)
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

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

instance HasModifiersFor env LockedDoor where
  getModifiersFor _ (LocationTarget lid) (LockedDoor attrs) =
    pure [ CannotInvestigate | lid `elem` treacheryAttachedLocation attrs ]
  getModifiersFor _ _ _ = pure []

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
    Revelation iid source | isSource attrs source -> do
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
                [(x, _)] -> unshiftMessages
                  [AttachTreachery treacheryId (LocationTarget x)]
                xs -> unshiftMessage
                  (Ask iid
                  $ ChooseOne
                      [ AttachTreachery treacheryId (LocationTarget x)
                      | (x, _) <- xs
                      ]
                  )
              LockedDoor <$> runMessage msg attrs
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
          , BeginSkillTest
            iid
            (TreacherySource treacheryId)
            (TreacheryTarget treacheryId)
            Nothing
            SkillAgility
            4
          ]
        )
    PassedSkillTest _ _ source _ _ | isSource attrs source ->
      t <$ unshiftMessage (Discard $ toTarget attrs)
    _ -> LockedDoor <$> runMessage msg attrs

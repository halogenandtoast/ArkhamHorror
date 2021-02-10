module Arkham.Types.Asset.Cards.StrayCat
  ( StrayCat(..)
  , strayCat
  ) where

import Arkham.Prelude

import Arkham.Types.Ability
import Arkham.Types.AssetId
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.LocationId
import Arkham.Types.Message
import Arkham.Types.Slot
import Arkham.Types.Window
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner
import Arkham.Types.Trait

newtype StrayCat = StrayCat AssetAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

strayCat :: AssetId -> StrayCat
strayCat uuid = StrayCat
  $ (baseAttrs uuid "01076") { assetSlots = [AllySlot], assetHealth = Just 2 }

instance HasModifiersFor env StrayCat where
  getModifiersFor = noModifiersFor

ability :: AssetAttrs -> Ability
ability a = mkAbility (toSource a) 1 (FastAbility (DiscardCost $ toTarget a))

instance HasActions env StrayCat where
  getActions iid FastPlayerWindow (StrayCat a) | ownedBy a iid =
    withBaseActions iid FastPlayerWindow a
      $ pure [ActivateCardAbilityAction iid (ability a)]
  getActions _ _ _ = pure []

instance AssetRunner env => RunMessage env StrayCat where
  runMessage msg a@(StrayCat attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      locationId <- getId @LocationId iid
      locationEnemyIds <- getSetList locationId
      nonEliteEnemyIds <- filterM
        ((notMember Elite <$>) . getSet)
        locationEnemyIds

      a <$ unshiftMessage
        (chooseOne iid [ EnemyEvaded iid enemyId | enemyId <- nonEliteEnemyIds ]
        )
    _ -> StrayCat <$> runMessage msg attrs

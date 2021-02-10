module Arkham.Types.Asset.Cards.PeterClover
  ( peterClover
  , PeterClover(..)
  )
where

import Arkham.Prelude

import Arkham.Types.Ability
import Arkham.Types.AssetId
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.EnemyId
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.Message
import Arkham.Types.Window
import Arkham.Types.Asset.Attrs
import Arkham.Types.Trait

newtype PeterClover = PeterClover AssetAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

peterClover :: AssetId -> PeterClover
peterClover uuid = PeterClover
  $ (baseAttrs uuid "02079") { assetHealth = Just 3, assetSanity = Just 2 }

ability :: AssetAttrs -> Ability
ability attrs =
  mkAbility (toSource attrs) 1 (FastAbility $ ExhaustCost (toTarget attrs))

instance
  ( HasSet EnemyId env ([Trait], LocationId)
  , HasId LocationId env InvestigatorId
  )
  => HasActions env PeterClover where
  getActions iid FastPlayerWindow (PeterClover attrs) = do
    lid <- getId @LocationId iid
    criminals <- getSet @EnemyId ([Criminal], lid)
    pure
      [ ActivateCardAbilityAction iid (ability attrs) | not (null criminals) ]
  getActions iid window (PeterClover attrs) = getActions iid window attrs

instance HasModifiersFor env PeterClover where
  getModifiersFor = noModifiersFor

instance
  ( HasQueue env
  , HasModifiersFor env ()
  , HasSet EnemyId env ([Trait], LocationId)
  , HasId LocationId env InvestigatorId
  )
  => RunMessage env PeterClover where
  runMessage msg a@(PeterClover attrs@AssetAttrs {..}) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      lid <- getId @LocationId iid
      criminals <- getSetList ([Criminal], lid)
      a <$ unshiftMessage
        (chooseOne iid [ EnemyEvaded iid eid | eid <- criminals ])
    BeginEnemy | isNothing assetInvestigator ->
      a <$ unshiftMessage (AssetDamage assetId (toSource attrs) 1 0)
    _ -> PeterClover <$> runMessage msg attrs

{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.PeterClover
  ( peterClover
  , PeterClover(..)
  )
where

import Arkham.Import

import Arkham.Types.Asset.Attrs
import Arkham.Types.Trait

newtype PeterClover = PeterClover Attrs
  deriving newtype (Show, ToJSON, FromJSON)

peterClover :: AssetId -> PeterClover
peterClover uuid = PeterClover
  $ (baseAttrs uuid "02079") { assetHealth = Just 3, assetSanity = Just 2 }

ability :: Attrs -> Ability
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
  runMessage msg a@(PeterClover attrs@Attrs {..}) = case msg of
    UseCardAbility iid source _ 1 | isSource attrs source -> do
      lid <- getId @LocationId iid
      criminals <- getSetList ([Criminal], lid)
      a <$ unshiftMessage
        (chooseOne iid [ EnemyEvaded iid eid | eid <- criminals ])
    BeginEnemy | isNothing assetInvestigator ->
      a <$ unshiftMessage (AssetDamage assetId (toSource attrs) 1 0)
    _ -> PeterClover <$> runMessage msg attrs

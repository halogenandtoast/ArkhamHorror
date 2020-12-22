{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.JazzMulligan
  ( jazzMulligan
  , JazzMulligan(..)
  )
where

import Arkham.Import

import Arkham.Types.Action
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Trait

newtype JazzMulligan = JazzMulligan Attrs
  deriving newtype (Show, ToJSON, FromJSON)

jazzMulligan :: AssetId -> JazzMulligan
jazzMulligan uuid = JazzMulligan $ (baseAttrs uuid "02060")
  { assetHealth = Just 2
  , assetSanity = Just 2
  , assetIsStory = True
  }

ability :: Attrs -> Ability
ability attrs = mkAbility (toSource attrs) 1 (ActionAbility 1 Nothing)

instance HasId LocationId env InvestigatorId => HasActions env JazzMulligan where
  getActions iid NonFast (JazzMulligan attrs) = do
    lid <- getId iid
    case assetLocation attrs of
      Just location -> pure
        [ ActivateCardAbilityAction iid (ability attrs)
        | lid == location && isNothing (assetInvestigator attrs)
        ]
      _ -> pure mempty
  getActions iid window (JazzMulligan attrs) = getActions iid window attrs

instance HasSet Trait env LocationId => HasModifiersFor env JazzMulligan where
  getModifiersFor (InvestigatorSource iid) (LocationTarget lid) (JazzMulligan attrs)
    | ownedBy attrs iid
    = do
      traits <- getSet lid
      pure [ toModifier attrs Blank | Miskatonic `member` traits ]
  getModifiersFor _ _ _ = pure []

instance (HasQueue env, HasModifiersFor env ()) => RunMessage env JazzMulligan where
  runMessage msg a@(JazzMulligan attrs) = case msg of
    UseCardAbility iid source _ 1 | isSource attrs source -> a <$ unshiftMessage
      (BeginSkillTest iid source (toTarget attrs) (Just Parley) SkillIntellect 3
      )
    PassedSkillTest iid _ source SkillTestInitiatorTarget{} _
      | isSource attrs source -> a
      <$ unshiftMessage (TakeControlOfAsset iid (assetId attrs))
    _ -> JazzMulligan <$> runMessage msg attrs

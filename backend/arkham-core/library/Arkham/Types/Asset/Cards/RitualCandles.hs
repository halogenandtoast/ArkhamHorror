{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.RitualCandles
  ( ritualCandles
  , RitualCandles(..)
  )
where

import Arkham.Import

import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers

newtype RitualCandles = RitualCandles Attrs
  deriving newtype (Show, ToJSON, FromJSON)

ritualCandles :: AssetId -> RitualCandles
ritualCandles uuid =
  RitualCandles $ (baseAttrs uuid "02029") { assetSlots = [HandSlot] }

ability :: Window -> Attrs -> Ability
ability window attrs =
  mkAbility (toSource attrs) 1 (ReactionAbility window Free)

instance HasActions env RitualCandles where
  getActions iid window@(WhenRevealToken You token) (RitualCandles x) = pure
    [ ActivateCardAbilityAction iid (ability window x)
    | token `elem` [Skull, Cultist, Tablet, ElderSign]
    ]
  getActions iid window (RitualCandles x) = getActions iid window x

instance HasModifiersFor env RitualCandles where
  getModifiersFor = noModifiersFor

instance (HasQueue env, HasModifiersFor env ()) => RunMessage env RitualCandles where
  runMessage msg a@(RitualCandles attrs) = case msg of
    UseCardAbility iid source _ 1 | isSource attrs source ->
      a <$ unshiftMessages
        [ CreateSkillTestEffect
            (EffectModifiers $ toModifiers attrs [AnySkillValue 1])
            source
            (InvestigatorTarget iid)
        ]
    _ -> RitualCandles <$> runMessage msg attrs

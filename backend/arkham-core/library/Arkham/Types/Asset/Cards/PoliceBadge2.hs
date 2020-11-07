{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.PoliceBadge2 where

import Arkham.Import

import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner

newtype PoliceBadge2 = PoliceBadge2 Attrs
  deriving newtype (Show, ToJSON, FromJSON)

policeBadge2 :: AssetId -> PoliceBadge2
policeBadge2 uuid =
  PoliceBadge2 $ baseAttrs uuid "01027" $ slots .= [AccessorySlot]

instance HasModifiersFor env PoliceBadge2 where
  getModifiersFor _ (InvestigatorTarget iid) (PoliceBadge2 a) =
    pure [ SkillModifier SkillWillpower 1 | ownedBy a iid ]
  getModifiersFor _ _ _ = pure []

instance HasActions env PoliceBadge2 where
  getActions iid (DuringTurn InvestigatorAtYourLocation) (PoliceBadge2 a)
    | ownedBy a iid = pure
      [ ActivateCardAbilityAction
          iid
          (mkAbility (toSource a) 1 (ActionAbility 1 Nothing))
      ]
  getActions _ _ _ = pure []

instance AssetRunner env => RunMessage env PoliceBadge2 where
  runMessage msg a@(PoliceBadge2 attrs) = case msg of
    UseCardAbility _ source _ 1 | isSource attrs source -> do
      activeInvestigatorId <- asks $ unActiveInvestigatorId . getId ()
      a <$ unshiftMessages
        [Discard (toTarget attrs), GainActions activeInvestigatorId source 2]
    _ -> PoliceBadge2 <$> runMessage msg attrs

module Arkham.Types.Asset.Cards.ToothOfEztli
  ( toothOfEztli
  , ToothOfEztli(..)
  )
where

import Arkham.Import

import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner

newtype ToothOfEztli = ToothOfEztli Attrs
  deriving newtype (Show, ToJSON, FromJSON)

toothOfEztli :: AssetId -> ToothOfEztli
toothOfEztli uuid =
  ToothOfEztli $ (baseAttrs uuid "04023") { assetSlots = [AccessorySlot] }

instance HasModifiersFor env ToothOfEztli where
  getModifiersFor (SkillTestSource _ _ (TreacherySource _) _) (InvestigatorTarget iid) (ToothOfEztli a)
    | ownedBy a iid
    = pure $ toModifiers
      a
      [SkillModifier SkillWillpower 1, SkillModifier SkillAgility 1]
  getModifiersFor _ _ _ = pure []

ability :: Attrs -> Ability
ability a =
  mkAbility (toSource a) 1 (ReactionAbility $ ExhaustCost (toTarget a))

instance HasActions env ToothOfEztli where
  getActions iid (AfterPassSkillTest _ (TreacherySource _) You _) (ToothOfEztli a)
    = pure [ ActivateCardAbilityAction iid (ability a) | ownedBy a iid ]
  getActions i window (ToothOfEztli a) = getActions i window a

instance AssetRunner env => RunMessage env ToothOfEztli where
  runMessage msg a@(ToothOfEztli attrs@Attrs {..}) = case msg of
    UseCardAbility iid source _ 1 | isSource attrs source ->
      a <$ unshiftMessage (DrawCards iid 1 False)
    _ -> ToothOfEztli <$> runMessage msg attrs

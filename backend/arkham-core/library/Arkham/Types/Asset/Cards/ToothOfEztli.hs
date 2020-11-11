{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.ToothOfEztli
  ( toothOfEztli
  , ToothOfEztli(..)
  )
where

import Arkham.Import

import Arkham.Types.Asset.Attrs

newtype ToothOfEztli = ToothOfEztli Attrs
  deriving newtype (Show, ToJSON, FromJSON)

toothOfEztli :: AssetId -> ToothOfEztli
toothOfEztli uuid =
  ToothOfEztli $ (baseAttrs uuid "04023") { assetSlots = [AccessorySlot] }

instance HasModifiersFor env ToothOfEztli where
  getModifiersFor (SkillTestSource _ (TreacherySource _) _) (InvestigatorTarget iid) (ToothOfEztli a)
    | ownedBy a iid
    = pure [SkillModifier SkillWillpower 1, SkillModifier SkillAgility 1]
  getModifiersFor _ _ _ = pure []

instance HasActions env ToothOfEztli where
  getActions iid (AfterPassSkillTest source@(TreacherySource _) You n) (ToothOfEztli a)
    | ownedBy a iid
    = do
      let
        ability = mkAbility
          (toSource a)
          1
          (ReactionAbility (AfterPassSkillTest source You n))
      pure [ ActivateCardAbilityAction iid ability | not (assetExhausted a) ]
  getActions i window (ToothOfEztli a) = getActions i window a

instance (HasQueue env, HasModifiers env InvestigatorId) => RunMessage env ToothOfEztli where
  runMessage msg (ToothOfEztli attrs@Attrs {..}) = case msg of
    UseCardAbility iid source _ 1 | isSource attrs source -> do
      unshiftMessage (DrawCards iid 1 False)
      pure . ToothOfEztli $ attrs & exhausted .~ True
    _ -> ToothOfEztli <$> runMessage msg attrs

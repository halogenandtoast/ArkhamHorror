{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.ProfessorWarrenRice where

import Arkham.Import

import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner

newtype ProfessorWarrenRice = ProfessorWarrenRice Attrs
  deriving newtype (Show, ToJSON, FromJSON)

professorWarrenRice :: AssetId -> ProfessorWarrenRice
professorWarrenRice uuid = ProfessorWarrenRice $ (baseAttrs uuid "02061")
  { assetSlots = [AllySlot]
  , assetHealth = Just 2
  , assetSanity = Just 3
  }

instance HasModifiersFor env ProfessorWarrenRice where
  getModifiersFor _ (InvestigatorTarget iid) (ProfessorWarrenRice a) =
    pure [ toModifier a (SkillModifier SkillIntellect 1) | ownedBy a iid ]
  getModifiersFor _ _ _ = pure []

instance ActionRunner env => HasActions env ProfessorWarrenRice where
  getActions iid window@(AfterDiscoveringClues You YourLocation) (ProfessorWarrenRice a@Attrs {..})
    | ownedBy a iid
    = do
      lid <- getId @LocationId iid
      lastClue <- (== 0) . unClueCount <$> getCount lid
      pure
        [ ActivateCardAbilityAction
            iid
            (mkAbility (toSource a) 1 (ReactionAbility window))
        | lastClue && not assetExhausted
        ]
  getActions _ _ _ = pure []

instance AssetRunner env => RunMessage env ProfessorWarrenRice where
  runMessage msg (ProfessorWarrenRice attrs) = case msg of
    UseCardAbility iid source _ 1 | isSource attrs source -> do
      unshiftMessage (DrawCards iid 1 False)
      pure $ ProfessorWarrenRice $ attrs & exhaustedL .~ True
    _ -> ProfessorWarrenRice <$> runMessage msg attrs

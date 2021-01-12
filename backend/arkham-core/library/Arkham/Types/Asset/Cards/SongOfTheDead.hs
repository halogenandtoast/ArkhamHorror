module Arkham.Types.Asset.Cards.SongOfTheDead
  ( songOfTheDead
  , SongOfTheDead(..)
  )
where

import Arkham.Import

import Arkham.Types.Asset.Attrs

newtype SongOfTheDead = SongOfTheDead Attrs
  deriving newtype (Show, ToJSON, FromJSON)

songOfTheDead :: AssetId -> SongOfTheDead
songOfTheDead uuid =
  SongOfTheDead $ baseAttrs uuid "02112" { assetSlots = [ArcaneSlot] }

instance ActionRunner env => HasActions env SongOfTheDead  where
  getActions iid window (SongOfTheDead  a) | ownedBy a iid = do
    fightAvailable <- hasFightActions iid window
    pure
      [ ActivateCardAbilityAction
          iid
          (mkAbility
            (toSource a)
            1
            (ActionAbility
              (Just Action.Fight)
              (Costs [ActionCost 1, UseCost (toId a) Charge 1])
            )
          )
      | fightAvailable
      ]
  getActions _ _ _ = pure []

instance HasModifiersFor env SongOfTheDead where
  getModifiersFor = noModifiersFor

instance (HasQueue env, HasModifiersFor env ()) => RunMessage env SongOfTheDead where
  runMessage msg a@(SongOfTheDead attrs) = case msg of
    InvestigatorPlayAsset _ aid _ _ | aid == assetId attrs ->
      SongOfTheDead <$> runMessage msg (attrs & usesL .~ Uses Charge 5)
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      a <$ unshiftMessages
        [ CreateSkillTestEffect
          (EffectModifiers $ toModifiers attrs [SkillModifer SkillWillpower 1])
          source
          (InvestigatorTarget iid)
        , CreateEffect "02112" Nothing source (InvestigatorTarget iid)
        , ChooseFightEnemy iid source SkillWillpower False
        ]
    _ -> SongOfTheDead <$> runMessage msg attrs

module Arkham.Types.Asset.Cards.GuardDog
  ( GuardDog(..)
  , guardDog
  )
where

import Arkham.Prelude

import Arkham.Types.Ability
import Arkham.Types.AssetId
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Message
import Arkham.Types.Slot
import Arkham.Types.Source
import Arkham.Types.Window
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner

newtype GuardDog = GuardDog AssetAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

guardDog :: AssetId -> GuardDog
guardDog uuid = GuardDog $ (baseAttrs uuid "01021")
  { assetSlots = [AllySlot]
  , assetHealth = Just 3
  , assetSanity = Just 1
  }

instance HasModifiersFor env GuardDog where
  getModifiersFor = noModifiersFor

ability :: Source -> AssetAttrs -> Ability
ability source attrs = base
  { abilityLimit = PlayerLimit PerTestOrAbility 1
  , abilityMetadata = Just (SourceMetadata source)
  }
  where base = mkAbility (toSource attrs) 1 (ReactionAbility Free)

instance HasActions env GuardDog where
  getActions iid (WhenDealtDamage source@(EnemySource _) target) (GuardDog attrs)
    | isTarget attrs target
    = pure
      [ ActivateCardAbilityAction iid (ability source attrs)
      | ownedBy attrs iid
      ]
  getActions i window (GuardDog attrs) = getActions i window attrs

instance (AssetRunner env) => RunMessage env GuardDog where
  runMessage msg a@(GuardDog attrs@AssetAttrs {..}) = case msg of
    UseCardAbility _ source (Just (SourceMetadata (EnemySource eid))) 1 _
      | isSource attrs source -> a <$ unshiftMessage
        (chooseOne
          (getInvestigator attrs)
          [ EnemyDamage eid (getInvestigator attrs) (toSource attrs) 1
          , Continue "Do not use Guard Dog's ability"
          ]
        )
    _ -> GuardDog <$> runMessage msg attrs

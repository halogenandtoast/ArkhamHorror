module Arkham.Types.Asset.Cards.GuardDog
  ( GuardDog(..)
  , guardDog
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Message
import Arkham.Types.Source
import Arkham.Types.Window

newtype GuardDog = GuardDog AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

guardDog :: AssetCard GuardDog
guardDog = ally GuardDog Cards.guardDog (3, 1)

instance HasModifiersFor env GuardDog

ability :: Source -> AssetAttrs -> Ability
ability source attrs = base
  { abilityLimit = PlayerLimit PerTestOrAbility 1
  , abilityMetadata = Just (SourceMetadata source)
  }
  where base = mkAbility (toSource attrs) 1 (ResponseAbility Free)

instance HasAbilities env GuardDog where
  getAbilities iid (WhenDealtDamage source@(EnemySource _) target) (GuardDog attrs)
    | isTarget attrs target
    = pure [ ability source attrs | ownedBy attrs iid ]
  getAbilities i window (GuardDog attrs) = getAbilities i window attrs

instance (AssetRunner env) => RunMessage env GuardDog where
  runMessage msg a@(GuardDog attrs) = case msg of
    UseCardAbility _ source (Just (SourceMetadata (EnemySource eid))) 1 _
      | isSource attrs source -> a <$ push
        (chooseOne
          (getInvestigator attrs)
          [ EnemyDamage eid (getInvestigator attrs) (toSource attrs) 1
          , Continue "Do not use Guard Dog's ability"
          ]
        )
    _ -> GuardDog <$> runMessage msg attrs

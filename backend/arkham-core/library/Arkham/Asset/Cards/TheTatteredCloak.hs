module Arkham.Asset.Cards.TheTatteredCloak
  ( theTatteredCloak
  , TheTatteredCloak(..)
  ) where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Investigator.Types (Field(..))
import Arkham.Projection
import Arkham.SkillType

newtype TheTatteredCloak = TheTatteredCloak AssetAttrs
  deriving anyclass (IsAsset, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theTatteredCloak :: AssetCard TheTatteredCloak
theTatteredCloak =
  assetWith TheTatteredCloak Cards.theTatteredCloak (healthL ?~ 1)

instance HasModifiersFor TheTatteredCloak where
  getModifiersFor (InvestigatorTarget iid) (TheTatteredCloak attrs)
    | controlledBy attrs iid = do
      remainingSanity <- field InvestigatorRemainingSanity iid
      let
        skillModifiers = if remainingSanity <= 3
          then
            [ SkillModifier SkillWillpower 1
            , SkillModifier SkillCombat 1
            , SkillModifier SkillAgility 1
            ]
          else []
      pure $ toModifiers attrs (SanityModifier (-1) : skillModifiers)
  getModifiersFor _ _ = pure []

instance RunMessage TheTatteredCloak where
  runMessage msg (TheTatteredCloak attrs) =
    TheTatteredCloak <$> runMessage msg attrs

module Arkham.Asset.Assets.TheTatteredCloak (
  theTatteredCloak,
  TheTatteredCloak (..),
) where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Investigator.Types (Field (..))
import Arkham.Projection

newtype TheTatteredCloak = TheTatteredCloak AssetAttrs
  deriving anyclass (IsAsset, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theTatteredCloak :: AssetCard TheTatteredCloak
theTatteredCloak =
  assetWith TheTatteredCloak Cards.theTatteredCloak (healthL ?~ 1)

instance HasModifiersFor TheTatteredCloak where
  getModifiersFor (TheTatteredCloak a) = case a.controller of
    Nothing -> pure mempty
    Just iid -> do
      remainingSanity <- field InvestigatorRemainingSanity iid
      let
        skillModifiers =
          if remainingSanity <= 3
            then
              [ SkillModifier #willpower 1
              , SkillModifier #combat 1
              , SkillModifier #agility 1
              ]
            else []
      modified_ a iid (SanityModifier (-1) : skillModifiers)

instance RunMessage TheTatteredCloak where
  runMessage msg (TheTatteredCloak attrs) =
    TheTatteredCloak <$> runMessage msg attrs

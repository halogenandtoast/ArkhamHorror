module Arkham.Types.Asset.Cards.TheTatteredCloak
  ( theTatteredCloak
  , TheTatteredCloak(..)
  ) where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner
import Arkham.Types.Classes
import Arkham.Types.InvestigatorId
import Arkham.Types.Modifier
import Arkham.Types.Query
import Arkham.Types.SkillType
import Arkham.Types.Target

newtype TheTatteredCloak = TheTatteredCloak AssetAttrs
  deriving anyclass (IsAsset, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theTatteredCloak :: AssetCard TheTatteredCloak
theTatteredCloak =
  bodyWith TheTatteredCloak Cards.theTatteredCloak (healthL ?~ 1)

instance HasCount RemainingSanity env InvestigatorId => HasModifiersFor env TheTatteredCloak where
  getModifiersFor _ (InvestigatorTarget iid) (TheTatteredCloak attrs)
    | ownedBy attrs iid = do
      remainingSanity <- unRemainingSanity <$> getCount iid
      let
        skillModifiers = if remainingSanity <= 3
          then
            [ SkillModifier SkillWillpower 1
            , SkillModifier SkillCombat 1
            , SkillModifier SkillAgility 1
            ]
          else []
      pure $ toModifiers attrs (SanityModifier (-1) : skillModifiers)
  getModifiersFor _ _ _ = pure []

instance AssetRunner env => RunMessage env TheTatteredCloak where
  runMessage msg (TheTatteredCloak attrs) =
    TheTatteredCloak <$> runMessage msg attrs

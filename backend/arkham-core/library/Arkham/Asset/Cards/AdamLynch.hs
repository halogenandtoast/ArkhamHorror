module Arkham.Asset.Cards.AdamLynch
  ( adamLynch
  , AdamLynch(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Attrs
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Source
import Arkham.Target
import Arkham.Timing qualified as Timing
import Arkham.Token

newtype AdamLynch = AdamLynch AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

adamLynch :: AssetCard AdamLynch
adamLynch =
  allyWith AdamLynch Cards.adamLynch (1, 1)
    $ (isStoryL .~ True)
    . (slotsL .~ mempty)

instance HasAbilities AdamLynch where
  getAbilities (AdamLynch x) =
    [ mkAbility x 1
      $ ForcedAbility
      $ AssetLeavesPlay Timing.When
      $ AssetWithId
      $ toId x
    ]

instance Query LocationMatcher env => HasModifiersFor env AdamLynch where
  getModifiersFor (InvestigatorSource iid) (LocationTarget lid) (AdamLynch attrs)
    = do
      isSecurityOffice <- elem lid
        <$> select (LocationWithTitle "Security Office")
      pure $ toModifiers
        attrs
        [ ActionCostSetToModifier 1 | isSecurityOffice && ownedBy attrs iid ]
  getModifiersFor _ _ _ = pure []

instance AssetRunner env => RunMessage env AdamLynch where
  runMessage msg a@(AdamLynch attrs) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source ->
      a <$ pushAll [AddToken Tablet, RemoveFromGame $ toTarget attrs]
    _ -> AdamLynch <$> runMessage msg attrs

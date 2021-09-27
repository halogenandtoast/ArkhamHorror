module Arkham.Types.Asset.Cards.AdamLynch
  ( adamLynch
  , AdamLynch(..)
  ) where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner
import Arkham.Types.Classes
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Timing qualified as Timing
import Arkham.Types.Token

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

module Arkham.Types.Asset.Cards.AdamLynch
  ( adamLynch
  , AdamLynch(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Classes
import Arkham.Types.Id
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Token

newtype AdamLynch = AdamLynch AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

adamLynch :: AssetCard AdamLynch
adamLynch =
  allyWith AdamLynch Cards.adamLynch (1, 1)
    $ (isStoryL .~ True)
    . (slotsL .~ mempty)

instance HasActions AdamLynch

instance HasId (Maybe LocationId) env LocationMatcher => HasModifiersFor env AdamLynch where
  getModifiersFor (InvestigatorSource iid) (LocationTarget lid) (AdamLynch attrs)
    = do
      isSecurityOffice <- elem lid <$> getLocationIdWithTitle "Security Office"
      pure $ toModifiers
        attrs
        [ ActionCostSetToModifier 1 | isSecurityOffice && ownedBy attrs iid ]
  getModifiersFor _ _ _ = pure []

instance (HasQueue env, HasModifiersFor env ()) => RunMessage env AdamLynch where
  runMessage msg a@(AdamLynch attrs) = case msg of
    Discard target | isTarget attrs target ->
      a <$ pushAll [AddToken Tablet, RemoveFromGame target]
    _ -> AdamLynch <$> runMessage msg attrs

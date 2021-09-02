module Arkham.Types.Asset.Cards.HaroldWalsted
  ( haroldWalsted
  , HaroldWalsted(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import qualified Arkham.Types.Action as Action
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner
import Arkham.Types.Classes
import Arkham.Types.Id
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Target
import qualified Arkham.Types.Timing as Timing
import Arkham.Types.Token
import Arkham.Types.Trait

newtype HaroldWalsted = HaroldWalsted AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

haroldWalsted :: AssetCard HaroldWalsted
haroldWalsted =
  allyWith HaroldWalsted Cards.haroldWalsted (1, 1) (isStoryL .~ True)

instance HasAbilities HaroldWalsted where
  getAbilities (HaroldWalsted x) =
    [ mkAbility x 1
      $ ForcedAbility
      $ AssetLeavesPlay Timing.When
      $ AssetWithId
      $ toId x
    ]

instance
  ( HasSet Trait env LocationId
  , HasId LocationId env InvestigatorId
  )
  => HasModifiersFor env HaroldWalsted where
  getModifiersFor (SkillTestSource _ _ _ _ (Just Action.Investigate)) (InvestigatorTarget iid) (HaroldWalsted attrs)
    = do
      lid <- getId @LocationId iid
      isMiskatonic <- member Miskatonic <$> getSet lid
      pure $ toModifiers
        attrs
        [ SkillModifier SkillIntellect 2 | isMiskatonic && ownedBy attrs iid ]
  getModifiersFor _ _ _ = pure []

instance AssetRunner env  => RunMessage env HaroldWalsted where
  runMessage msg a@(HaroldWalsted attrs) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source ->
      a <$ pushAll [AddToken Tablet, RemoveFromGame $ toTarget attrs]
    _ -> HaroldWalsted <$> runMessage msg attrs

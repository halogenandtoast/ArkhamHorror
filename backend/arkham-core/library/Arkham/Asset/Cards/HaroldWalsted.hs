module Arkham.Asset.Cards.HaroldWalsted
  ( haroldWalsted
  , HaroldWalsted(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Action qualified as Action
import Arkham.Asset.Runner
import Arkham.Id
import Arkham.Matcher
import Arkham.Modifier
import Arkham.SkillType
import Arkham.Source
import Arkham.Target
import Arkham.Timing qualified as Timing
import Arkham.Token
import Arkham.Trait

newtype HaroldWalsted = HaroldWalsted AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

haroldWalsted :: AssetCard HaroldWalsted
haroldWalsted = allyWith
  HaroldWalsted
  Cards.haroldWalsted
  (1, 1)
  ((isStoryL .~ True) . (slotsL .~ mempty))

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

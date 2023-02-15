module Arkham.Asset.Cards.HaroldWalsted
  ( haroldWalsted
  , HaroldWalsted(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.SkillType
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

instance HasModifiersFor HaroldWalsted where
  getModifiersFor (InvestigatorTarget iid) (HaroldWalsted attrs) = do
    mAction <- getSkillTestAction
    case mAction of
      Just Action.Investigate -> do
        isMiskatonic <-
          selectAny
          $ LocationWithInvestigator (InvestigatorWithId iid)
          <> LocationWithTrait Miskatonic
        pure $ toModifiers
          attrs
          [ SkillModifier SkillIntellect 2
          | isMiskatonic && controlledBy attrs iid
          ]
      _ -> pure []
  getModifiersFor _ _ = pure []

instance RunMessage HaroldWalsted where
  runMessage msg a@(HaroldWalsted attrs) = case msg of
    UseCardAbility _ source 1 _ _ | isSource attrs source ->
      a <$ pushAll [AddToken Tablet, RemoveFromGame $ toTarget attrs]
    _ -> HaroldWalsted <$> runMessage msg attrs

module Arkham.Asset.Cards.GavriellaMizrah (
  gavriellaMizrah,
  GavriellaMizrah (..),
)
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Discover
import Arkham.Matcher
import Arkham.Message qualified as Msg

newtype GavriellaMizrah = GavriellaMizrah AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

gavriellaMizrah :: AssetCard GavriellaMizrah
gavriellaMizrah = allyWith GavriellaMizrah Cards.gavriellaMizrah (4, 1) (isStoryL .~ True)

instance HasModifiersFor GavriellaMizrah where
  getModifiersFor (InvestigatorTarget iid) (GavriellaMizrah a) | controlledBy a iid = do
    pure $ toModifiers a [SkillModifier #combat 1]
  getModifiersFor _ _ = pure []

instance HasAbilities GavriellaMizrah where
  getAbilities (GavriellaMizrah a) =
    [ controlledAbility
        a
        1
        (CanDiscoverCluesAt YourLocation <> OnLocation LocationWithAnyClues)
        $ ReactionAbility (EnemyAttacksEvenIfCancelled #after You AnyEnemyAttack AnyEnemy) (exhaust a)
    ]

instance RunMessage GavriellaMizrah where
  runMessage msg a@(GavriellaMizrah attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ Msg.DiscoverClues iid $ discoverAtYourLocation (toSource attrs) 1
      pure a
    _ -> GavriellaMizrah <$> runMessage msg attrs
